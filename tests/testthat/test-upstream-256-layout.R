# Isolated source contracts for the declared upstream 2.5.6 geometry port.
test_that("puttab panel headings remain unmerged and size only their label column", {
  d <- data.frame(lab = c("Short", "Other", "Third"), a = c("1", "4", "7"),
    b = c("2", "5", "8"), c = c("3", "6", "9"),
    pan = c(rep("Any narcolepsy code on or before delivery", 2L), "Second"))
  before <- d
  x <- puttab(d, panel = "pan", title = "T")
  lay <- tabtools:::.xlsx_layout_puttab(x)
  st <- tabtools:::.xlsx_apply_rules(lay$rules, nrow(lay$grid), ncol(lay$grid), x$style)
  expect_identical(st$merges, "A1:E1")
  expect_identical(unname(st$widths[c("2", "3", "4", "5")]), c(41, 8, 8, 8))
  expect_identical(lay$grid[c(3L, 6L), 2L], c("Any narcolepsy code on or before delivery", "Second"))
  expect_identical(as.vector(lay$grid[c(3L, 6L), 3:5]), rep("", 6L))
  expect_true(all(st$bold[c(3L, 6L), 2:5]))
  expect_true(all(st$top[c(3L, 6L), 2:5] == "thin"))
  expect_identical(d, before)
  d$pan[1:2] <- "A heading that is far longer than fifty characters, it really is long"
  cap <- puttab(d, panel = "pan")
  lcap <- tabtools:::.xlsx_layout_puttab(cap)
  scap <- tabtools:::.xlsx_apply_rules(lcap$rules, nrow(lcap$grid), ncol(lcap$grid), cap$style)
  expect_identical(unname(scap$widths["2"]), 50)
  expect_false("B3:E3" %in% scap$merges)
  span <- puttab(d, panel = "pan", spanheader = list(list(text = strrep("Long span ", 20L), first = 2L, last = 3L)))
  ls <- tabtools:::.xlsx_layout_puttab(span)
  ss <- tabtools:::.xlsx_apply_rules(ls$rules, nrow(ls$grid), ncol(ls$grid), span$style)
  expect_true("C2:D2" %in% ss$merges)
  expect_identical(unname(ss$widths[c("3", "4")]), c(8, 8))
})

test_that("table1 SMD width follows its display-width header and the active suppressed marker", {
  d <- data.frame(g = rep(1:2, each = 25L), x = 1:50, b = c(rep(0, 24L), 1, rep(0, 5L), rep(1, 20L)))
  width <- function(x) {
    l <- tabtools:::.xlsx_layout_table1(x)
    st <- tabtools:::.xlsx_apply_rules(l$rules, nrow(l$grid), ncol(l$grid), x$style)
    unname(st$widths[as.character(which(x$cols$role == "smd") + 1L)])
  }
  plain <- table1_tc(d, by = "g", vars = "x contn", smd = TRUE)
  expect_identical(width(plain), 8)
  masked <- table1_tc(d, by = "g", vars = "x contn \\ b bin", smd = TRUE, smallcells = 3L)
  expect_true("Suppressed" %in% masked$body[[which(masked$cols$role == "smd")]])
  expect_identical(width(masked), 10)
  marker <- plain; marker$body[[which(marker$cols$role == "smd")]][1L] <- "Suppressed"
  expect_identical(width(marker), 8)
  pair <- plain
  pair$header[[1L]]$text[which(pair$cols$role == "smd")] <- "SMD (Primary vs Secondary)"
  expect_identical(width(pair), 25)
  pair$header[[1L]]$text[which(pair$cols$role == "smd")] <- "SMD (東 vs B)"
  expect_identical(width(pair), 14)
})
