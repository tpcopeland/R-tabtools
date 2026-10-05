# D02 (documentation review 2026-09-27): stratetab() defaults its exposure
# and outcome labels from tt_rates() blocks, which know their grouping
# column and event variable; plain strate-shaped blocks (the Stata route)
# keep Stata's "Exposure 1" / "Outcome 1".

d02_lung <- function() {
  d <- survival::lung
  d$dead <- d$status - 1
  d$sex <- factor(d$sex, 1:2, c("Male", "Female"))
  d$ecog <- factor(pmin(d$ph.ecog, 2), 0:2, c("0", "1", "2 or more"))
  d
}

d02_strate <- function() {
  data.frame(arm = c("A", "B"), `_D` = c(30, 42), `_Y` = c(1000, 950),
             `_Rate` = c(0.03, 0.0442), `_Lower` = c(0.021, 0.0327),
             `_Upper` = c(0.0429, 0.0598), check.names = FALSE)
}

test_that("tt_rates() keeps the grouping column's label and records the event variable", {
  skip_if_not_installed("survival")
  d <- d02_lung()
  attr(d$sex, "label") <- "Sex"
  attr(d$dead, "label") <- "Death"
  r <- tt_rates(d, "time", "dead", by = "sex")
  expect_identical(attr(r$sex, "label"), "Sex")
  expect_identical(levels(r$sex), c("Male", "Female"))
  expect_identical(attr(r, "event"), "dead")
  expect_identical(attr(r, "event_label"), "Death")
  # Without labels: no label attribute, the event name is still recorded.
  r2 <- tt_rates(d02_lung(), "time", "dead", by = "sex")
  expect_null(attr(r2$sex, "label"))
  expect_identical(attr(r2, "event"), "dead")
  expect_null(attr(r2, "event_label"))
})

test_that("stratetab() of a tt_rates() block with labels uses them (the D02 repro)", {
  skip_if_not_installed("survival")
  d <- d02_lung()
  attr(d$sex, "label") <- "Sex"
  attr(d$dead, "label") <- "Death"
  tt <- stratetab(tt_rates(d, "time", "dead", by = "sex"))
  expect_identical(tt$body$c1, c("Sex", "   Male", "   Female"))
  expect_identical(tt$meta$frame$outcome_label, "Death")
  # The identity is the event variable, as a Cox model's (review M2).
  expect_identical(tt$meta$frame$outcome_id, "dead")
})

test_that("stratetab() of unlabelled tt_rates() blocks uses the column and event names", {
  skip_if_not_installed("survival")
  d <- d02_lung()
  tt <- stratetab(list(tt_rates(d, "time", "dead", by = "sex"),
                       tt_rates(d, "time", "dead", by = "ecog")), outcomes = 1)
  expect_identical(tt$body$c1[c(1, 4)], c("sex", "ecog"))
  expect_identical(tt$meta$frame$outcome_label, "dead")
  # Two outcomes, each named by its event variable.
  d$long <- as.integer(d$time > 365)
  tt <- stratetab(list(tt_rates(d, "time", "dead", by = "sex"),
                       tt_rates(d, "time", "long", by = "sex")), outcomes = 2)
  expect_identical(tt$meta$frame$outcome_label, c("dead", "long"))
  expect_identical(tt$body$c1[1], "sex")
})

test_that("explicit explabels and outlabels still win over the tt_rates() defaults", {
  skip_if_not_installed("survival")
  d <- d02_lung()
  attr(d$sex, "label") <- "Sex"
  attr(d$dead, "label") <- "Death"
  tt <- stratetab(tt_rates(d, "time", "dead", by = "sex"),
                  explabels = "Gender", outlabels = "Mortality")
  expect_identical(tt$body$c1[1], "Gender")
  expect_identical(tt$meta$frame$outcome_label, "Mortality")
  # Each can be given alone; the other still defaults from the blocks.
  tt <- stratetab(tt_rates(d, "time", "dead", by = "sex"), outlabels = "Mortality")
  expect_identical(tt$body$c1[1], "Sex")
  tt <- stratetab(tt_rates(d, "time", "dead", by = "sex"), explabels = "Gender")
  expect_identical(tt$meta$frame$outcome_label, "Death")
})

test_that("plain strate-shaped blocks keep Stata's Exposure k / Outcome k", {
  s <- d02_strate()
  tt <- stratetab(s, level = 95)
  expect_identical(tt$body$c1[1], "Exposure 1")
  expect_identical(tt$meta$frame$outcome_label, "Outcome 1")
  # Standardised through tt_rates() it is still the strate-file route.
  tt <- stratetab(tt_rates(s, level = 0.95))
  expect_identical(tt$body$c1[1], "Exposure 1")
  expect_identical(tt$meta$frame$outcome_label, "Outcome 1")
})

test_that("mixed, ungrouped or ambiguous blocks fall back to Stata's defaults", {
  skip_if_not_installed("survival")
  d <- d02_lung()
  attr(d$sex, "label") <- "Sex"
  # A tt_rates() block beside a strate-shaped one.
  s <- d02_strate()
  tt <- stratetab(list(tt_rates(d, "time", "dead", by = "sex"), s), outcomes = 1, level = 95)
  expect_identical(tt$body$c1[c(1, 4)], c("Exposure 1", "Exposure 2"))
  expect_identical(tt$meta$frame$outcome_label, "Outcome 1")
  # A block without a grouping variable has no exposure name to use; its
  # event still names the outcome.
  tt <- stratetab(tt_rates(d, "time", "dead"))
  expect_identical(tt$body$c1[1], "Exposure 1")
  expect_identical(tt$meta$frame$outcome_label, "dead")
  # The same grouping variable in every exposure (subgroups compared with
  # rateratio) would give duplicate labels: Stata's defaults instead.
  men <- d[d$sex == "Male", ]
  women <- d[d$sex == "Female", ]
  tt <- stratetab(list(tt_rates(men, "time", "dead", by = "ecog"),
                       tt_rates(women, "time", "dead", by = "ecog")),
                  outcomes = 1, rateratio = TRUE)
  expect_identical(tt$body$c1[c(1, 5)], c("Exposure 1", "Exposure 2"))
  expect_identical(tt$meta$frame$outcome_label, "dead")
  # Outcome columns whose exposures disagree on the event variable: Outcome k.
  d$long <- as.integer(d$time > 365)
  tt <- stratetab(list(tt_rates(d, "time", "dead", by = "sex"),
                       tt_rates(d, "time", "long", by = "ecog")), outcomes = 1)
  expect_identical(tt$body$c1[c(1, 4)], c("Sex", "ecog"))
  expect_identical(tt$meta$frame$outcome_label, "Outcome 1")
  # The same event in two outcome columns: duplicate identities, so Outcome k.
  tt <- stratetab(list(tt_rates(d, "time", "dead", by = "sex"),
                       tt_rates(d, "time", "dead", by = "sex")), outcomes = 2)
  expect_identical(tt$meta$frame$outcome_label, c("Outcome 1", "Outcome 2"))
})
