.onLoad <- function(libname, pkgname) {
  # Persisted tabtools_options(persist = TRUE) defaults (Stata's `tabtools
  # set ..., permanent` profile) fill any key the session has not set.
  try(.tt_load_persisted(), silent = TRUE)
  invisible()
}
