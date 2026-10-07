t1p_local <- function(envir = parent.frame()) {
  keys <- paste0("tabtools.", tabtools:::.tt_option_keys)
  withr::local_options(stats::setNames(lapply(keys, getOption), keys), .local_envir = envir)
  state <- tabtools:::.tt_sink_state
  old <- as.list(state, all.names = TRUE)
  withr::defer({
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(old, envir = state)
  }, envir = envir)
  tabtools_options(clear = TRUE)
  state$written <- character()
  withr::local_tempdir(pattern = "wp-3f-", .local_envir = envir)
}

t1p_data <- function() {
  data.frame(g = factor(rep(c("A", "B"), each = 14)),
    category = factor(c(rep("Low", 2), rep("Mid", 6), rep("High", 4), NA, NA,
                        rep("Low", 5), rep("Mid", 9)), levels = c("Low", "Mid", "High")),
    age = seq_len(28), other = rep(seq_len(14), 2), w = rep(1, 28))
}

t1p_call <- function(data = t1p_data(), ...) {
  table1_tc(data, vars = c(category = "cat", age = "contn", other = "contn"), by = "g", ...)
}

t1p_mask_contract <- function(tt, threshold, mode, masked, linked) {
  testthat::expect_identical(tt$stored$smallcells,
    list(threshold = threshold, mode = mode, n_masked = masked, n_linked = linked))
  if (threshold > 0L) testthat::expect_identical(tt$stored$smallcells_mode,
    if (mode == "strict") "full" else "primary")
}

t1p_measure <- function(tt, id, metric, raw = FALSE) {
  ledger <- if (raw) tt$stored$raw$sample_accounting else tt$meta$sample_accounting
  m <- ledger$measures
  m[m$population_id == id & m$metric == metric, , drop = FALSE]
}

t1p_cells <- function(tt) as.matrix(as.data.frame(tt))

t1p_replace <- function(row, column, text = "Not reportable") {
  list(list(row = row, column = column, text = text))
}

t1p_ledger_changed <- function(before, after) {
  m <- before$meta$sample_accounting$measures
  n <- after$meta$sample_accounting$measures
  paste(m$population_id[m$status != n$status], m$metric[m$status != n$status], sep = ":")
}
