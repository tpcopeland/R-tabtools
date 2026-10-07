# Small supplied-input builders shared by the WP-2D contract tests. No oracle
# values are derived here; expected publication text is literal in each test.
wp2d_numeric_rows <- function() {
  d <- data.frame(term = "raw_x", estimate = 0.1234567891,
                  conf.low = -0.9876543219, conf.high = 0.3456789123,
                  p.value = 0.04)
  attr(d, "conf.level") <- 0.95
  attr(d, "effect_scale") <- "Coef."
  d
}

wp2d_factor_table <- function() {
  regtab(lm(mpg ~ wt + factor(cyl), mtcars), stats = "n", models = "Fitted")
}

# Resolve documentation from the loaded namespace, never from the test
# working directory or a different installation found on .libPaths().
wp2d_help_text <- function(topic) {
  ns_path <- normalizePath(getNamespaceInfo("tabtools", "path"), mustWork = TRUE)
  man_dir <- file.path(ns_path, "man")
  page_name <- paste0(topic, ".Rd")
  if (dir.exists(man_dir)) {
    page_path <- file.path(man_dir, page_name)
    if (!file.exists(page_path)) stop("Missing current tabtools help page: ", topic, call. = FALSE)
    page <- tools::parse_Rd(page_path)
  } else {
    pages <- tools::Rd_db("tabtools", lib.loc = dirname(ns_path))
    page <- pages[[page_name]]
    if (is.null(page)) stop("Missing current tabtools help page: ", topic, call. = FALSE)
  }
  text <- paste(capture.output(tools::Rd2txt(page, options = list(underline_titles = FALSE))), collapse = "\n")
  gsub("[[:space:]]+", " ", text)
}

# Read the entire header/body/footer region from B2 onward. Keep trailing
# columns/rows, merged blanks and publication strings so dimensions/order
# are part of the comparison rather than inferred from the expected frame.
wp2d_xlsx_matrix <- function(path, sheet) {
  cells <- openxlsx2::wb_to_df(path, sheet = sheet, start_row = 2L, start_col = 2L,
                              col_names = FALSE, skip_empty_rows = FALSE,
                              skip_empty_cols = FALSE, fill_merged_cells = FALSE, convert = FALSE)
  out <- as.matrix(cells)
  out[is.na(out)] <- ""
  unname(out)
}
