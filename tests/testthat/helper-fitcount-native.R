# Pure R recipe matching qa/stata/fitcount_v251_cases.do. Native artifacts are
# generated only by the orchestrator's separately authenticated isolated run.
fitcount_native_case <- function(id, sinks = list()) {
  publish <- function(...) do.call(regtab, c(list(...), sinks))
  if (id %in% c("FC001", "FC003")) {
    data <- data.frame(y = c(1, 3, 2, 5, 4, 7), x = 1:6,
      ev = c(1, 0, 2, 0, 1, 0), pid = c("a", "a", "b", "c", "c", "d"),
      exposure = c(1, 2, 1, 3, 2, 4))
    fit <- lm(y ~ x, data = data)
    counts <- tt_fitcount(fit, "ev", "pid", "exposure", data = data, terms = TRUE)
    if (id == "FC001") return(publish(fit, fitcounts = counts,
      stats = "obs events people exposure", exposurelabel = "Person days"))
    spec <- list(list(name = "ev", mincell = 5, maskwith = "a"),
      list(name = "ev", pair = "total", mincell = 7), list(name = "a", maskwith = "b"),
      list(name = "b"), list(name = "index"), list(text = paste0("m", 1:12), label = "Column"))
    return(publish(rep(list(fit), 12), fitcounts = rep(list(counts), 12), stats = spec,
      stat_values = lapply(1:12, function(m) list(ev = 3, total = 40, a = 10, b = 0, index = as.numeric(m)))))
  }
  if (id == "FC002") {
    data <- data.frame(g = factor(rep(1:3, 6)), x = rep(0:5, each = 3))
    data$y <- 2 + data$x + as.numeric(data$g) + seq_len(18) %% 4
    data$ev <- c(0, 1, 2)[as.numeric(data$g)]
    fit <- lm(y ~ g * x, data = data, contrasts = list(g = contr.treatment(3, base = 2)))
    counts <- tt_fitcount(fit, "ev", data = data, terms = TRUE)
    return(publish(fit, fitcounts = counts, mincount = 7, interactions = "native", nointercept = FALSE))
  }
  stop("Unknown fit-count native case.", call. = FALSE)
}
