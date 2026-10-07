xt_data <- function(f, rowcodes = seq_len(nrow(f)), colcodes = seq_len(ncol(f))) {
  grid <- expand.grid(row = rowcodes, column = colcodes)
  grid$frequency <- as.vector(f)
  grid
}

xt_table <- function(f, ...) crosstab(xt_data(f), "row", "column", weights = "frequency", ...)

xt_no_session <- function() {
  withr::local_options(stats::setNames(rep(list(NULL), 8L),
    paste0("tabtools.", c("workbook", "markdown", "smallcells", "smallcells_mode", "masktext", "digits", "boldp", "headershade"))),
    .local_envir = parent.frame())
}
