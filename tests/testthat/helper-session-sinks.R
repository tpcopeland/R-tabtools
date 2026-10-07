ss_local <- function(envir = parent.frame()) {
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
  withr::local_tempdir(pattern = "wp-3a-", .local_envir = envir)
}

ss_rate <- function() {
  ct_block(c("A", "B"), c(2, 10), c(100, 200))
}

ss_cases <- function() {
  d <- data.frame(g = rep(c("A", "B"), each = 5), x = 1:10)
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  effects <- data.frame(term = "x", estimate = 1, conf.low = 0.5,
                        conf.high = 1.5, p.value = 0.001)
  attr(effects, "conf.level") <- 0.95
  rates <- ct_rates()
  models <- ct_models()
  list(
    table1 = function(...) table1_tc(d, by = "g", vars = c(x = "contn"), ...),
    desctab = function(...) desctab(d, by = "g", vars = c(x = "contn"), ...),
    regtab = function(...) regtab(fit, ...),
    effecttab = function(...) effecttab(effects, type = "margins", ...),
    stratetab = function(...) stratetab(ss_rate(), ...),
    survival_rates = function(...) stratetab(tt_rates(
      data.frame(g = rep(c("A", "B"), each = 5), time = 1:10,
                  event = rep(c(0, 1, 1, 0, 1), 2)),
      time = "time", event = "event", by = "g"), ...),
    comptab = function(...) comptab(list(models, models), rows = list(1, 1), ...),
    hrcomptab = function(...) hrcomptab(rates, list(models, models), rows = list(1, 4:5), ...),
    wttab = function(...) wttab(data.frame(w = c(0.5, 1, 2)), "w", ...)
  )
}

ss_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
ss_bytes <- function(path) readBin(path, "raw", n = file.size(path))
