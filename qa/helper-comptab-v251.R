# Deterministic source data shared by installed controls and the native recipe.
ct_v251_data <- function() {
  d <- data.frame(g = factor(rep(c("A", "B", "C"), each = 12)),
    x = rep(1:12, 3), other = factor(rep(c("No", "Yes"), 18)), time = rep(10, 36))
  d$ev <- as.numeric(d$x <= rep(c(3, 5, 8), each = 12))
  d$ev2 <- as.numeric(d$x <= rep(c(2, 6, 9), each = 12))
  d
}
ct_v251_sources <- function() {
  d <- ct_v251_data()
  a <- glm(ev ~ g, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L))
  b <- glm(ev ~ g + x, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L))
  c <- glm(ev2 ~ g, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L))
  list(data = d, first = a, second = c,
    models = regtab(a, b, models = c("Crude", "Adjusted"), coef = "IRR", nointercept = TRUE),
    appendix = regtab(glm(ev ~ g + other + x, poisson(), d, control = glm.control(epsilon = 1e-12, maxit = 100L)), coef = "IRR", nointercept = TRUE),
    paired = regtab(a, c, models = c("First", "Second"), coef = "IRR", nointercept = TRUE),
    stars = regtab(a, coef = "IRR", nointercept = TRUE, stars = TRUE, starslevels = c(.8, .6, .2)),
    rate = stratetab(tt_rates(d, "time", "ev", by = "g"), ratescale = 100, unitlabel = "100"),
    rateonly = stratetab(list(tt_rates(d, "time", "ev", by = "g"), tt_rates(d, "time", "ev", by = "other")),
      outcomes = 1, ratescale = 100, unitlabel = "100"),
    tworates = stratetab(list(tt_rates(d, "time", "ev", by = "g"), tt_rates(d, "time", "ev2", by = "g")),
      outcomes = 2, outcomeids = c("ev", "ev2"), outlabels = c("First outcome", "Second outcome"), ratescale = 100, unitlabel = "100"))
}
ct_v251_cases <- function(z) list(
  CO001 = hrcomptab(z$rate, z$models, rows = 2:4, keyed = TRUE, allmodels = TRUE, cformat = "%8.4f"),
  CO002 = hrcomptab(z$rateonly, z$models, rows = 2:4, keyed = TRUE, allmodels = TRUE),
  CO003 = hrcomptab(z$rate, z$appendix, rows = "all", modelonly = TRUE),
  CO004 = hrcomptab(z$tworates, z$paired, rows = 3:4, outcomemap = list("Second", "First")),
  CO005 = comptab(z$stars, rows = 3:4, cformat = "%8.4f", cisep = " to "),
  CO006 = comptab(z$stars, rows = 3:4, cisep = " / "))

# Root-promoted native bytes; manifest checksum is fixed independently of the
# inventory being read. Missing/stale inputs stop before any parity assertion.
ct_v251_authenticate <- function(directory = testthat::test_path("data", "comptab_v251")) {
  inventory <- file.path(directory, "ARTIFACTS.csv")
  if (!file.exists(inventory) || !identical(unname(tools::md5sum(inventory)),
      "2b71a764342c16cd94c6b0556099b6e5")) stop("Native composite artifact inventory is missing or changed")
  files <- read.csv(inventory, colClasses = "character")
  if (!identical(names(files), c("file", "md5")) || nrow(files) != 32L ||
      anyNA(files) || anyDuplicated(files$file) || any(grepl("(^/|[.][.]|\\\\)", files$file))) {
    stop("Malformed native composite inventory")
  }
  actual <- unname(tools::md5sum(file.path(directory, files$file)))
  if (anyNA(actual) || !identical(actual, files$md5)) stop("Native composite artifact bytes changed or missing")
  required <- c(as.vector(outer(sprintf("CO%03d", 1:6), c(".csv", ".md", ".console.txt", "_stored.csv"), paste0)),
    "CO004_forest.csv", "comptab.xlsx", "SOURCE.csv", "artifacts.json", "launch.json", "cleanup.json", "driver.log", "process.log")
  if (!setequal(files$file, required)) stop("Incomplete native composite capture")
  source <- read.csv(file.path(directory, "SOURCE.csv"), colClasses = "character")
  if (!identical(names(source), c("key", "value")) || anyNA(source) || anyDuplicated(source$key)) stop("Invalid native source record")
  info <- stats::setNames(source$value, source$key)
  if (!identical(unname(info[c("native_source_commit", "tabtools_version", "stata_version", "native_case_count", "native_source_count", "capture_artifact_count")]),
      c("712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129", "2.5.1", "17", "6", "90", "29"))) stop("Unexpected native pin/runtime/capture counts")
  launch <- jsonlite::fromJSON(file.path(directory, "launch.json"))
  if (length(launch$native_source_sha256) != 90L ||
      !identical(unname(info[paste0("native_sha256/", names(launch$native_source_sha256))]),
                 unname(unlist(launch$native_source_sha256)))) stop("Native source closure mismatch")
  cleanup <- jsonlite::fromJSON(file.path(directory, "cleanup.json"))
  if (!identical(cleanup$exit_status, 0L) || !isTRUE(cleanup$absent) || !isTRUE(cleanup$descendants_stopped)) stop("Native capture did not complete cleanup")
  if (!any(trimws(readLines(file.path(directory, "driver.log"), warn = FALSE)) == "ROOT_COMPTAB_NATIVE_COMPLETE")) stop("Native completion marker missing")
  directory
}

ct_v251_console_box <- function(lines) {
  # Native logs wrap at linesize; rejoin only Stata's explicit continuation
  # marker and select the single complete listing, excluding command/log paths.
  joined <- character()
  for (line in lines) {
    if (startsWith(line, "> ") && length(joined)) joined[length(joined)] <- paste0(tail(joined, 1L), substring(line, 3L))
    else joined <- c(joined, line)
  }
  edges <- which(grepl("^\\s*\\+-+\\+\\s*$", joined))
  if (length(edges) != 2L) stop("A composite console control needs one complete boxed listing")
  joined[seq.int(edges[1L], edges[2L])]
}
