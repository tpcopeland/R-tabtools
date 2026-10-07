fc_cox_metadata_data <- function() {
  d <- data.frame(time = 1:18, event = rep(c(1, 0), 9), person = rep(1:6, 3))
  d$region <- factor(rep(c("A", "B", "C"), 6), levels = c("A", "B", "C", "Unused"))
  attr(d$region, "labels") <- c(A = 0, B = 3, C = 9, Unused = 11)
  attr(d$region, "label") <- "Region"
  d
}

test_that("Cox capture restores only lost retained code metadata without changing fits", {
  skip_if_not_installed("survival")
  d <- fc_cox_metadata_data()
  fit <- survival::coxph(survival::Surv(time, event) ~ region, data = d,
    ties = "breslow", model = TRUE, x = TRUE)
  before <- .fc_evidence(fit)
  source_before <- d
  record <- tt_fitcount(fit, "event", "person", "time", data = d)
  expect_identical(record$counts, c(obs = 18, events = 9, people = 6, people_ev = 3, exposure = 171))
  expect_identical(record$sample$row_ids, as.character(1:18))
  expect_identical(record$sample$source_origin, "caller_declared")
  expect_identical(record$snapshot$frame$region, fit$model$region)
  expect_identical(record$snapshot$source$region, d$region)
  expect_identical(levels(record$snapshot$frame$region), c("A", "B", "C", "Unused"))
  expect_identical(attr(record$snapshot$frame$region, "labels"), c(A = 0, B = 3, C = 9, Unused = 11))
  expect_identical(.fc_evidence(fit), before)
  expect_identical(d, source_before)
})

test_that("Cox term counts retain code zero, unused levels, and accepted record identity", {
  skip_if_not_installed("survival")
  d <- fc_cox_metadata_data()
  fit <- survival::coxph(survival::Surv(time, event) ~ region, data = d,
    ties = "breslow", model = TRUE, x = TRUE)
  record <- tt_fitcount(fit, "event", "person", "time", data = d, terms = TRUE)
  keys <- c("0.region", "3.region", "9.region")
  expect_true(all(keys %in% record$terms$count_key))
  expect_equal(record$terms$events[match(keys, record$terms$count_key)], c(3, 3, 3))
  expect_false("11.region" %in% record$terms$count_key)
  expect_identical(attr(record$snapshot$frame$region, "labels"), c(A = 0, B = 3, C = 9, Unused = 11))
  reordered_data <- d[18:1, ]
  # Caller row views also lose factor attributes; declare the same original
  # mapping explicitly rather than treating stripped metadata as historical.
  attr(reordered_data$region, "labels") <- attr(d$region, "labels")
  attr(reordered_data$region, "label") <- attr(d$region, "label")
  reordered <- tt_fitcount(fit, "event", "person", "time", data = reordered_data, terms = TRUE)
  expect_identical(reordered$counts, record$counts)
  expect_identical(reordered$sample$row_ids, record$sample$row_ids)
  expect_identical(reordered$terms, record$terms)
})

test_that("Cox metadata restoration cannot conceal source mapping or predictor mutations", {
  skip_if_not_installed("survival")
  d <- fc_cox_metadata_data()
  fit <- survival::coxph(survival::Surv(time, event) ~ region, data = d,
    ties = "breslow", model = TRUE, x = TRUE)
  bad <- d
  attr(bad$region, "labels") <- c(A = 9, B = 3, C = 0, Unused = 11)
  expect_error(tt_fitcount(fit, "event", data = bad), class = "tabtools_error_fitcount")
  bad <- d; attr(bad$region, "label") <- "Wrong region"
  expect_error(tt_fitcount(fit, "event", data = bad), class = "tabtools_error_fitcount")
  bad <- d; bad$region[1] <- "B"
  expect_error(tt_fitcount(fit, "event", data = bad), class = "tabtools_error_fitcount")
  bad <- d; bad$region <- ordered(as.character(bad$region), levels = levels(d$region))
  attributes(bad$region)[c("labels", "label")] <- attributes(d$region)[c("labels", "label")]
  expect_error(tt_fitcount(fit, "event", data = bad), class = "tabtools_error_fitcount")
})


test_that("source and retained-source alignment both preserve exact factor metadata", {
  d <- data.frame(y = c(1, 3, 2, 4, 5, 4, 6, 8), event = rep(c(1, 0), 4))
  d$region <- factor(rep(c("A", "B"), 4))
  attr(d$region, "labels") <- c(A = 0, B = 3)
  attr(d$region, "label") <- "Region"
  fit <- glm(y ~ region, data = d, family = gaussian())
  expect_identical(fit$data$region, d$region)
  expect_error(tt_fitcount(fit, "event", data = d[8:1, ]),
    class = "tabtools_error_fitcount")
  # Supply the original labelled source after reordering: base row subsetting
  # itself drops labels, so use an explicitly metadata-preserving row view.
  reordered <- d[8:1, ]
  attr(reordered$region, "labels") <- attr(d$region, "labels")
  attr(reordered$region, "label") <- attr(d$region, "label")
  record <- tt_fitcount(fit, "event", data = reordered)
  expect_identical(record$snapshot$source$region, d$region)
  expect_identical(record$counts[c("obs", "events")], c(obs = 8, events = 4))
  expect_identical(record$sample$row_ids, as.character(1:8))
  bad <- d
  attr(bad$region, "labels") <- c(A = 3, B = 0)
  expect_error(tt_fitcount(fit, "event", data = bad), class = "tabtools_error_fitcount")
})

test_that("optional term counts preserve codes on the positive-weight sample", {
  d <- data.frame(y = c(1, 4, 3, 7, 5, 8, 6, 10), event = rep(c(1, 0), 4),
    w = c(0, 0, rep(1, 6)))
  d$region <- factor(rep(c("A", "B"), 4), levels = c("A", "B", "Unused"))
  attr(d$region, "labels") <- c(A = 0, B = 3, Unused = 11)
  attr(d$region, "label") <- "Region"
  fit <- lm(y ~ region, data = d, weights = w)
  record <- tt_fitcount(fit, "event", data = d, terms = TRUE, weight_type = "aweight")
  expect_identical(record$counts[c("obs", "events")], c(obs = 6, events = 3))
  expect_identical(record$sample$row_ids, as.character(3:8))
  expect_identical(record$sample$excluded_zero_weight, 2L)
  keys <- c("0.region", "3.region")
  expect_true(all(keys %in% record$terms$count_key))
  expect_equal(record$terms$events[match(keys, record$terms$count_key)], c(3, 0))
  expect_identical(record$terms$n_records[match(keys, record$terms$count_key)], c(3L, 3L))
  expect_false("11.region" %in% record$terms$count_key)
  expect_identical(attr(record$snapshot$source$region, "labels"), c(A = 0, B = 3, Unused = 11))
})
