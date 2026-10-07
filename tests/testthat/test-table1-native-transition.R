# Actual pinned native probes for P1-06/P1-07/P1-08. No native invocation here.
transition_path <- function(...) test_path("fixtures", "table1_native_transition", ...)

test_that("native transition receipts authenticate current headers and all refusals", {
  source <- utils::read.csv(transition_path("SOURCE.csv"), colClasses = "character")
  info <- stats::setNames(source$value, source$key)
  expect_identical(unname(info[c("native_source_commit", "tabtools_version", "rng")]),
    c("712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129", "2.5.1", "mt64"))
  a <- utils::read.csv(transition_path("ARTIFACTS.csv"), colClasses = "character")
  expect_false(anyDuplicated(a$file) > 0L)
  expect_identical(unname(tools::md5sum(transition_path(a$file))), a$md5)
  refusal <- utils::read.csv(transition_path("REFUSALS.csv"), colClasses = "character")
  expect_identical(refusal$case, c("S11", "RW11", "RW12"))
  expect_identical(refusal$rc, c("498", "498", "198"))
  for (case in c("S17", "RW07")) {
    stored <- utils::read.csv(transition_path(paste0(case, "_stored.csv")), colClasses = "character", na.strings = character())
    expect_identical(stored$value[stored$name == "_golden_tabtools_version"], "2.5.1")
    expect_false("_cells_from" %in% stored$name)
  }
  cells <- golden_read_cells_file(transition_path("S17.csv"))
  expect_identical(cells[1L, ncol(cells)], "SMD (Placebo vs Drug)")
  note <- "SMD compares Placebo vs Drug only (the first two of 3 groups)."
  counts <- paste0("Counts below 4 are shown as <4; complementary cells are shown as ≥4 to prevent exact reconstruction. ",
    "Percentages are withheld for any variable carrying a suppressed count.")
  expect_identical(cells[nrow(cells), 1L], paste(counts, note))
  expect_identical(golden_read_lines(transition_path("S17_console.txt"))[1L], paste("Note:", note))
})

test_that("numeric by headers match native g = value without changing group identity", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10))
  tt <- table1_tc(d, by = "g", vars = c(x = "contn"), nopvalue = TRUE)
  native <- golden_read_cells_file(transition_path("NUMBY.csv"))
  expect_identical(native[1L, 2:3], c("g = 1", "g = 2"))
  expect_identical(unname(tt$header[[1L]]$text[2:3]), c("g = 1", "g = 2"))
  expect_identical(nrow(golden_compare_cells(tt, native)), 0L)
  expect_identical(golden_compare_console(utils::capture.output(print(tt)),
    golden_read_lines(transition_path("NUMBY_console.txt"))), character())
  expect_identical(tt$cols$role, c("label", "group", "group"))
  expect_identical(d$g, rep(1:2, each = 10))
})

test_that("only two literal native overflow cells differ from independently proved finite R SD", {
  d <- data.frame(x = 1:20, g = rep(1:2, each = 10), w = exact_num("1e200"), b = (1:20) %% 2)
  tt <- table1_tc(d, by = "g", vars = c(x = "contn", b = "bin"), wt = "w", smd = TRUE)
  native <- golden_read_cells_file(transition_path("OVERFLOW.csv"))
  actual <- golden_as_cells(tt)
  expect_identical(dim(native), c(5L, 4L))
  # Var(1:10)=55/6, independently of package moment/formatting helpers.
  for (group in 1:2) {
    take <- d$g == group
    moments <- tabtools:::.t1w_cont_stats(d$x[take], d$w[take], rep(1, sum(take)), "contn", "wt")
    expect_true(is.finite(moments$b))
    expect_equal(moments$b, sqrt(55 / 6), tolerance = 1e-12)
  }
  expect_identical(native[4L, 2:3], c("6±.", "16±."))
  expect_identical(actual[4L, 2:3], c("6±3", "16±3"))
  compare <- function(got, want) {
    expect_identical(want[4L, 2:3], c("6±.", "16±."))
    expect_identical(got[4L, 2:3], c("6±3", "16±3"))
    projected <- got
    projected[4L, 2:3] <- c("6±.", "16±.")
    expect_identical(nrow(golden_compare_cells(projected, want)), 0L)
  }
  compare(actual, native)
  for (at in list(c(4L, 2L), c(4L, 4L), c(5L, 2L), c(1L, 2L))) {
    bad <- actual
    bad[at[1L], at[2L]] <- "fault"
    golden_expect_detected(compare(bad, native))
  }
  bad <- native
  bad[4L, 2L] <- "6±0"
  golden_expect_detected(compare(actual, bad))
})
