# qa/check_examples.R - the installed public API as an installed user sees
# it (Milestone H, H16). Needs TABTOOLS_QA_LIB (set by qa/run_all.R): a
# library holding an installed copy. For every export of that copy:
#   - it is documented (an \alias in an installed Rd page);
#   - its page has examples, and they run without error.
# Reports every export; exits non-zero on any failure.
#   TABTOOLS_QA_LIB=<lib> Rscript qa/check_examples.R
file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
qa_dir <- if (length(file_arg)) dirname(normalizePath(file_arg[1])) else normalizePath("qa")
source(file.path(qa_dir, "tools", "qa_result.R"))  # qa_check(), qa_done() (Codex audit CX-6)
lib <- Sys.getenv("TABTOOLS_QA_LIB")
if (!nzchar(lib)) stop("TABTOOLS_QA_LIB is not set; run qa/run_all.R, which installs a copy first.")
suppressMessages(library(tabtools, lib.loc = lib))
cat("tabtools", format(utils::packageVersion("tabtools", lib.loc = lib)), "installed in", lib, "\n")

exports <- sort(getNamespaceExports("tabtools"))
db <- tools::Rd_db("tabtools", lib.loc = lib)
alias_of <- lapply(db, function(rd) unlist(tools:::.Rd_get_metadata(rd, "alias")))
has_examples <- vapply(db, function(rd) length(tools:::.Rd_get_section(rd, "examples")) > 0L, TRUE)

done <- character()
for (fn in exports) {
  page <- names(alias_of)[vapply(alias_of, function(a) fn %in% a, TRUE)]
  if (!length(page)) {
    qa_check(sprintf("%-20s no help page", fn), FALSE)
    next
  }
  page <- page[1]
  if (!has_examples[[page]]) {
    qa_check(sprintf("%-20s %s has no examples", fn, page), FALSE)
    next
  }
  if (page %in% done) {
    qa_check(sprintf("%-20s (examples of %s already run)", fn, page), TRUE)
    next
  }
  topic <- sub("\\.Rd$", "", page)
  res <- tryCatch({
    utils::capture.output(suppressMessages(
      utils::example(topic, package = "tabtools", lib.loc = lib, character.only = TRUE,
                     ask = FALSE, echo = FALSE, run.dontrun = FALSE)))
    NULL
  }, error = function(e) conditionMessage(e))
  done <- c(done, page)
  qa_check(sprintf("%-20s examples of %s %s", fn, page, if (is.null(res)) "run" else paste("failed:", res)),
           is.null(res))
}
qa_done("check_examples.R")
