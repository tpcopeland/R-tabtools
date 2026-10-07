# A plain editing/export frame deliberately carries no fitted-model row keys.
.tt_unkeyed_export <- function(x) {
  out <- x$body
  attr(out, "header") <- x$header
  attr(out, "command") <- x$command
  attr(out, "frame") <- x$meta$frame
  attr(out, "sample_accounting") <- x$meta[["sample_accounting", exact = TRUE]]
  attr(out, "composition_export") <- TRUE
  out
}
