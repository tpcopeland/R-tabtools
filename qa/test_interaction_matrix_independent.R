library(testthat)
library(tabtools)

# Independent numerical controls use native fits and raw masks. The catalog
# probe parses source declarations; it never evaluates package internals.
imi_data <- function(seed) {
  withr::with_seed(seed, {
    n <- 72L
    d <- data.frame(id = paste0("imi-private-", seed, "-", seq_len(n)),
      x = rnorm(n), g = factor(rep(c("A", "B"), length.out = n), levels = c("A", "B", "absent")),
      w = rep(c(1, 2, 4), length.out = n), include = seq_len(n) <= 66L)
    d$y <- .5 + .8 * d$x - .3 * (d$g == "B") + rnorm(n, sd = .7)
    d$x[c(2, 7)] <- NA_real_
    d$y[c(4, 7)] <- NA_real_
    d$g[9] <- NA
    d$w[c(5, 12)] <- 0
    d$w[13] <- NA_real_
    rownames(d) <- d$id
    d
  })
}

imi_state <- function() {
  has_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(seed_exists = has_seed,
       seed = if (has_seed) get(".Random.seed", envir = .GlobalEnv) else NULL,
       options = options(), directory = getwd(), sinks = c(sink.number(), sink.number(type = "message")))
}

imi_capture_call <- function(expr) {
  warnings <- list()
  value <- tryCatch(withCallingHandlers(expr, warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- w
    invokeRestart("muffleWarning")
  }), error = identity)
  list(value = value, warnings = warnings, state = imi_state())
}

# Independent schema for this single-source, single-model fixture. Opaque
# source IDs are checked before any rename; all other metadata stays exact.
imi_flat_source <- function(table) {
  f <- table$meta$flat
  nr <- nrow(table$body)
  fields <- c("block_id", "row_keys", "row_types", "row_blocks", "states",
              "publication_overrides", "source_blocks", "composite")
  character_vector <- function(x, n) is.character(x) && !is.object(x) &&
    is.null(dim(x)) && length(x) == n && !anyNA(x)
  character_matrix <- function(x) is.matrix(x) && is.character(x) &&
    !is.object(x) && identical(dim(x), c(nr, 1L)) && !anyNA(x)
  valid <- is.list(f) && identical(names(f), fields) && identical(f$composite, FALSE) &&
    character_vector(f$block_id, 1L) && nzchar(f$block_id) &&
    character_vector(f$row_blocks, nr) && all(f$row_blocks == f$block_id) &&
    character_vector(f$row_keys, nr) && identical(f$row_keys, table$rows$key) &&
    character_vector(f$row_types, nr) && identical(f$row_types, table$rows$type) &&
    all(f$row_types %in% c("var", "level", "ref", "omitted", "empty", "re", "cat_header", "stat", "addrow", "header")) &&
    character_matrix(f$states) && character_matrix(f$source_blocks) &&
    is.list(f$publication_overrides) &&
    identical(names(f$publication_overrides), c("origin", "text")) &&
    character_matrix(f$publication_overrides$origin) &&
    character_matrix(f$publication_overrides$text) &&
    all(f$publication_overrides$origin == "") &&
    all(f$publication_overrides$text == "")
  if (!valid) stop("Invalid single-source flat identity schema or row transport.", call. = FALSE)
  structural <- f$row_types %in% c("cat_header", "stat", "addrow", "header")
  required <- f$states != "" & f$states != "absent"
  if (any(f$states[structural, , drop = FALSE] != "") ||
      any(!f$states[!structural, , drop = FALSE] %in%
          c("est", "ref", "omit", "notest", "absent", "empty", "constrained", "masked")) ||
      any(f$source_blocks != "" & f$source_blocks != f$block_id) ||
      any(required & !nzchar(f$source_blocks))) {
    stop("Invalid single-source flat identity cell transport or state.", call. = FALSE)
  }
  f
}

imi_rename_source <- function(later, initial) {
  first <- imi_flat_source(initial)
  second <- imi_flat_source(later)
  if (identical(first$block_id, second$block_id)) {
    stop("Independent calls must have distinct flat source identities.", call. = FALSE)
  }
  # Exactly one bijective later-ID -> initial-ID rename. Indexed replacement
  # preserves all blanks, names, dimnames, attributes and semantic fields.
  mapped <- later
  for (field in c("block_id", "row_blocks", "source_blocks")) {
    value <- mapped$meta$flat[[field]]
    value[value == second$block_id] <- first$block_id
    mapped$meta$flat[[field]] <- value
  }
  mapped
}

imi_counts <- function(table, expected, converted_accounting) {
  s <- table$meta$sample_accounting
  expect_type(s, "list")
  expect_equal(nrow(s$populations), 1L, tolerance = 0)
  m <- s$measures[match(names(expected), s$measures$metric), ]
  expect_identical(m$status, rep("available", length(expected)))
  expect_equal(m$value, unname(expected), tolerance = 0)
  expect_false(any(grepl("imi-private-|imi-foreign-", unlist(table$meta), fixed = FALSE)))
  expect_identical(converted_accounting, s)
}

imi_native_rows <- function(table, fit, variance) {
  b <- stats::coef(fit)
  V <- stats::vcov(fit)
  key <- ifelse(names(b) == "(Intercept)", "_cons", ifelse(names(b) == "gB", "2.g", names(b)))
  rows <- table$meta$regtab_rows
  index <- match(key, rows$key)
  expect_false(anyNA(index))
  expect_equal(rows$estimate[index], unname(b), tolerance = 1e-12)
  se <- sqrt(diag(V))
  critical <- stats::qt(.975, stats::df.residual(fit))
  expect_equal(rows$conf.low[index], unname(b - critical * se), tolerance = 1e-12)
  expect_equal(rows$conf.high[index], unname(b + critical * se), tolerance = 1e-12)
  expect_equal(variance, V, tolerance = 1e-12)
}

test_that("IMI-01 stored weighted lm survives mutable callers without state or private-ID leakage", {
  seen_ids <- character()
  for (seed in c(7301L, 9403L)) {
    d <- imi_data(seed)
    accepted <- d$include & stats::complete.cases(d[c("y", "x", "g", "w")])
    used <- accepted & !is.na(d$w) & d$w > 0
    native_data <- d[used, ]
    oracle <- stats::lm(y ~ x + g, native_data, weights = w)
    fit <- stats::lm(y ~ x + g, d, weights = w, subset = include, na.action = na.exclude, model = TRUE)
    before_fit <- fit
    state <- imi_state()
    first_call <- imi_capture_call(regtab(fit, vce = "model", keepintercept = TRUE, stats = "n"))
    initial <- first_call$value
    variance_call <- imi_capture_call(tt_vcov(oracle, "model"))
    conversion_call <- imi_capture_call(attr(as.data.frame(initial), "sample_accounting", exact = TRUE))
    # The stored model survives a caller data rebind, changed values and
    # renumbered observations. Original-input size remains unrecoverable.
    d <- d[rev(seq_len(nrow(d))), ]
    d$x <- 1000 + seq_len(nrow(d))
    d$y <- -2000 + seq_len(nrow(d))
    d$w <- 1
    rownames(d) <- NULL
    later_call <- imi_capture_call(regtab(fit, vce = "model", keepintercept = TRUE, stats = "n"))
    later <- later_call$value
    invalid_call <- imi_capture_call(regtab(fit, vce = "invalid-vce"))
    # Freeze every package outcome/state before an expectation can load a
    # failure formatter. The complete options/RNG/directory/sink oracle stays.
    for (call in list(first_call, variance_call, conversion_call, later_call, invalid_call)) {
      expect_identical(call$state, state)
    }
    expect_identical(class(fit)[1L], "lm")
    expect_s3_class(initial, "tt_table")
    expect_s3_class(later, "tt_table")
    for (call in list(first_call, variance_call, conversion_call, invalid_call)) expect_length(call$warnings, 0L)
    expect_length(later_call$warnings, 1L)
    if (length(later_call$warnings) == 1L) {
      expect_s3_class(later_call$warnings[[1L]], "rlang_warning")
      expect_match(conditionMessage(later_call$warnings[[1L]]), "data no longer match its model frame")
    }
    expect_s3_class(invalid_call$value, "rlang_error")
    imi_native_rows(initial, oracle, variance_call$value)
    imi_counts(initial, c(eligible_n = sum(accepted), used_n = sum(used), fitted_n = sum(used),
      frame_n = sum(accepted), zero_weight_n = sum(accepted & !used), reported_n = sum(used)), conversion_call$value)
    first <- imi_flat_source(initial)
    second <- imi_flat_source(later)
    ids <- c(first$block_id, second$block_id)
    expect_false(anyDuplicated(c(seen_ids, ids)) > 0L)
    seen_ids <- c(seen_ids, ids)
    expect_identical(imi_rename_source(later, initial), initial)
    input <- later$meta$sample_accounting$measures
    expect_identical(input$status[input$metric == "input_n"], "unavailable")
    expect_true(is.na(input$value[input$metric == "input_n"]))
    expect_identical(fit, before_fit)
    # Corrupt IDs cannot be normalized into a pass, and semantic changes
    # remain visible to the same exact whole-object comparison.
    reused <- later
    for (field in c("block_id", "row_blocks", "source_blocks")) {
      value <- reused$meta$flat[[field]]
      value[value == second$block_id] <- first$block_id
      reused$meta$flat[[field]] <- value
    }
    expect_error(imi_rename_source(reused, initial), "distinct flat source identities")
    for (field in c("block_id", "row_blocks", "source_blocks")) {
      corrupt <- later
      corrupt$meta$flat[[field]][1L] <- paste0(second$block_id, "-foreign")
      expect_error(imi_rename_source(corrupt, initial), "Invalid single-source flat identity")
    }
    corrupt <- later
    corrupt$meta$flat$source_blocks[which(second$states == "est")[1L]] <- ""
    expect_error(imi_rename_source(corrupt, initial), "cell transport")
    corrupt <- later
    corrupt$meta$flat$row_blocks <- head(second$row_blocks, -1L)
    expect_error(imi_rename_source(corrupt, initial), "row transport")
    corrupt <- later
    corrupt$meta$flat$source_blocks <- t(second$source_blocks)
    expect_error(imi_rename_source(corrupt, initial), "identity schema")
    corrupt <- later
    corrupt$meta$flat$states <- NULL
    expect_error(imi_rename_source(corrupt, initial), "identity schema")
    for (bad_id in list(NA_character_, "", 1L)) {
      corrupt <- later
      corrupt$meta$flat$block_id <- bad_id
      expect_error(imi_rename_source(corrupt, initial), "identity schema")
    }
    mutations <- list(later, later, later, later, later, later, later, later)
    mutations[[1L]]$meta$flat$source_blocks[which(second$row_types == "stat")[1L], 1L] <- ""
    dimnames(mutations[[2L]]$meta$flat$source_blocks) <- list(as.character(seq_len(nrow(later$body))), "model")
    mutations[[3L]]$meta$flat$states[which(second$states == "est")[1L]] <- "omit"
    mutations[[4L]]$meta$regtab_rows$estimate[1L] <- 12345
    mutations[[5L]]$body[1L, 2L] <- "Changed literal"
    mutations[[6L]]$meta$sample_accounting$measures$value[1L] <- 12345
    mutations[[7L]]$header[[1L]]$text[1L] <- "Changed header"
    attr(mutations[[8L]]$meta$flat, "probe") <- TRUE
    for (mutated in mutations) expect_false(identical(imi_rename_source(mutated, initial), initial))
    cat(sprintf("\nIM-SEED case_id=IMI-01 seed=%d\n", seed))
  }
})

test_that("IMI-02 strict MI distinguishes reordered subjects from equal-N foreign IDs", {
  for (seed in c(7301L, 9403L)) {
    d <- imi_data(seed)
    before_data <- d
    completed <- withr::with_seed(seed + 1L, lapply(seq_len(3L), function(k) {
      a <- d
      a$y <- a$y + (k - 1) * (.04 * a$x + rnorm(nrow(a), sd = .03))
      a <- a[sample.int(nrow(a)), ]
      rownames(a) <- NULL
      a
    }))
    fits <- lapply(completed, function(a)
      stats::lm(y ~ x + g, a, weights = w, subset = include, na.action = na.exclude))
    ids <- lapply(completed, function(a) {
      mask <- a$include & stats::complete.cases(a[c("y", "x", "g", "w")]) & !is.na(a$w) & a$w > 0
      a$id[mask]
    })
    expect_false(identical(ids[[1]], ids[[2]]))
    expect_setequal(ids[[1]], ids[[2]])
    before_fits <- fits
    mi <- tt_mi(fits, observation_ids = ids, sample_check = "strict")
    expect_identical(tt_mi(mi), mi)
    expect_error(tt_mi(mi, observation_ids = NULL), class = "tabtools_error_mi_sample_identity")
    state <- imi_state()
    t <- regtab(mi, vce = "model", keepintercept = TRUE, stats = "n")
    Q <- vapply(fits, stats::coef, numeric(3L))
    U <- lapply(fits, stats::vcov)
    W <- Reduce(`+`, U) / length(U)
    B <- stats::cov(base::t(Q))
    T <- W + (1 + 1 / length(U)) * B
    expect_equal(tt_vcov(mi, "model"), T, tolerance = 1e-12)
    keys <- c("_cons", "x", "2.g")
    rows <- t$meta$regtab_rows
    positions <- match(keys, rows$key)
    expect_false(anyNA(positions))
    expect_equal(rows$estimate[positions], unname(rowMeans(Q)), tolerance = 1e-12)
    expect_false(any(grepl("imi-private-", unlist(t$meta), fixed = TRUE)))
    expect_identical(t$meta$mi_sample_identity[[1L]]$method, "explicit_ids")
    expect_equal(t$meta$sample_accounting$measures$value[
      t$meta$sample_accounting$measures$metric == "fitted_n"], rep(length(ids[[1]]), 3L), tolerance = 0)
    # Same sizes and renumbered row names are insufficient: swap one ID
    # after construction and require both public inference entry points to
    # recheck the explicit subject sets before calculating a plausible table.
    tampered <- mi
    tampered$observation_ids[[2]][1] <- paste0("imi-foreign-", seed)
    expect_identical(lengths(tampered$observation_ids), lengths(mi$observation_ids))
    expect_error(regtab(tampered, vce = "model"), class = "tabtools_error_mi_sample_mismatch")
    expect_error(tt_vcov(tampered, "model"), class = "tabtools_error_mi_sample_mismatch")
    expect_identical(fits, before_fits)
    expect_identical(d, before_data)
    expect_identical(imi_state(), state)
    cat(sprintf("\nIM-SEED case_id=IMI-02 seed=%d\n", seed))
  }
})

imi_head <- function(x) {
  if (!is.call(x)) return("")
  h <- x[[1L]]
  if (is.symbol(h)) return(as.character(h))
  if (is.call(h) && as.character(h[[1L]]) %in% c("::", ":::")) {
    return(paste(as.character(h[[2L]]), as.character(h[[3L]]), sep = as.character(h[[1L]])))
  }
  ""
}

imi_walk <- function(x, predicate) {
  out <- if (isTRUE(predicate(x))) list(x) else list()
  if (is.call(x) || is.expression(x) || is.pairlist(x)) {
    parts <- as.list(x)
    for (i in seq_along(parts)) {
      if (identical(parts[[i]], quote(expr = ))) next
      out <- c(out, imi_walk(parts[[i]], predicate))
    }
  }
  out
}

imi_strings <- function(x) {
  nodes <- imi_walk(x, function(z) is.character(z))
  unlist(nodes, use.names = FALSE)
}

imi_seed_loops <- function(x) {
  loops <- imi_walk(x, function(z) imi_head(z) == "for" &&
    is.symbol(z[[2L]]) && grepl("(^|_)seed$", as.character(z[[2L]])))
  unlist(lapply(loops, function(z) {
    value <- z[[3L]]
    if (imi_head(value) != "c") return(integer())
    args <- as.list(value)[-1L]
    if (!all(vapply(args, function(a) is.numeric(a) && length(a) == 1L, logical(1)))) return(integer())
    as.integer(unlist(args))
  }), use.names = FALSE)
}

imi_invocations <- function(x) {
  calls <- imi_walk(x, is.call)
  heads <- vapply(calls, imi_head, "")
  indirect <- lapply(calls[heads %in% c("do.call", "base::do.call")], function(call) {
    target <- call[[2L]]
    if (is.symbol(target) || is.character(target)) return(as.character(target))
    if (imi_head(target) == "::") return(paste(target[[2L]], target[[3L]], sep = "::"))
    ""
  })
  c(heads, unlist(indirect, use.names = FALSE))
}

test_that("IMI-03 static manifest claims reconcile with adapter catalog and QA source AST", {
  file <- test_path("data", "interaction_matrix.csv")
  expect_true(file.exists(file))
  if (!file.exists(file)) stop("The curated interaction manifest is missing.")
  manifest <- utils::read.csv(file, stringsAsFactors = FALSE)
  expect_identical(names(manifest), c("case_id", "suite", "adapter", "seeds", "conditions", "oracle", "evidence_limit"))
  expect_true(all(grepl("^IM[A-Z]-[0-9]{2}$", manifest$case_id)))
  expect_false(anyDuplicated(manifest[c("case_id", "adapter")]) > 0)
  expect_false(anyDuplicated(unique(manifest[c("case_id", "suite")])$case_id) > 0)
  source <- parse(test_path("..", "R", "regtab.R"), keep.source = FALSE)
  registration <- imi_walk(source, function(x) imi_head(x) == "<-" &&
    is.symbol(x[[2L]]) && identical(as.character(x[[2L]]), ".rt_supported_classes"))
  expect_length(registration, 1L)
  native <- names(as.list(registration[[1L]][[3L]])[-1L])
  expect_length(native, 20L)
  expect_setequal(intersect(unique(manifest$adapter), native), native)
  constructors <- list(lm = c("lm", "stats::lm"), glm = c("glm", "stats::glm"), negbin = "MASS::glm.nb",
    coxph = c("coxph", "survival::coxph"), clogit = c("clogit", "survival::clogit"), polr = "MASS::polr",
    clm = "ordinal::clm", multinom = "nnet::multinom", zeroinfl = "pscl::zeroinfl", hurdle = "pscl::hurdle",
    survreg = "survival::survreg", tobit = "AER::tobit", crr = "cmprsk::crr", lmerMod = "lme4::lmer",
    glmerMod = "lme4::glmer", lme = "nlme::lme", glmmTMB = "glmmTMB::glmmTMB",
    geeglm = "geepack::geeglm", svyglm = "survey::svyglm", glm_weightit = "WeightIt::glm_weightit")
  expect_setequal(names(constructors), native)
  routes <- list(`tt_mi:lm` = "tt_mi", `tt_mi:glm` = "tt_mi", `tt_mi:coxph` = "tt_mi", `tt_mi:crr` = "tt_mi",
    `tt_mi:mira` = c("tt_mi", "mice::as.mira"), `tt_uv:lm` = "regtab_uv",
    `effecttab:predictions` = c("effecttab", "marginaleffects::avg_predictions"),
    `effecttab:comparisons` = c("effecttab", "marginaleffects::avg_comparisons"),
    `effecttab:slopes` = c("effecttab", "marginaleffects::avg_slopes"),
    `effecttab:hypotheses` = c("effecttab", "marginaleffects::hypotheses"),
    `effecttab:matrix` = c("effecttab", "cbind"), `effecttab:data.frame` = c("effecttab", "data.frame"),
    `regtab:data.frame` = c("regtab", "data.frame"), `regtab:tbl_df` = c("regtab", "tibble::as_tibble"),
    tt_from_modelsummary = "tt_from_modelsummary", lmerModLmerTest = "lmerTest::lmer",
    tt_merge = "tt_merge", tt_stack = "tt_stack", comptab = "comptab", puttab = "puttab", stacktab = "stacktab",
    `as.data.frame:tt_table` = "as.data.frame", table1_tc = "table1_tc", desctab = "desctab", wttab = "wttab",
    tt_rates = "tt_rates", stratetab = "stratetab")
  expect_true(all(manifest$adapter %in% c(native, names(routes))))
  for (suite in unique(manifest$suite)) {
    path <- test_path(suite)
    expect_true(file.exists(path), info = suite)
    ast <- parse(path, keep.source = FALSE)
    blocks <- imi_walk(ast, function(x) imi_head(x) %in% c("test_that", "testthat::test_that"))
    ids <- vapply(blocks, function(b) {
      text <- imi_strings(b[[2L]])
      hits <- regmatches(text, regexpr("IM[A-Z]-[0-9]{2}", text))
      hits <- unique(hits[nzchar(hits)])
      if (length(hits) != 1L) return("")
      hits
    }, "")
    expect_true(all(nzchar(ids)), info = suite)
    expect_false(anyDuplicated(ids) > 0, info = suite)
    wanted <- unique(manifest$case_id[manifest$suite == suite])
    # IMI-03 is a static assertion block, not a seeded numerical case.
    expect_setequal(setdiff(ids, "IMI-03"), wanted)
    for (case in wanted) {
      block <- blocks[[match(case, ids)]]
      declared <- unique(manifest$seeds[manifest$case_id == case])
      expect_length(declared, 1L)
      seeds <- as.integer(strsplit(declared, ";", fixed = TRUE)[[1L]])
      expect_gte(length(seeds), 2L)
      expect_false(anyNA(seeds))
      expect_true(all(seeds > 0))
      actual <- imi_seed_loops(block)
      if (!length(actual)) actual <- imi_seed_loops(ast) # dynamically declared descriptive blocks
      expect_true(all(seeds %in% actual), info = paste(suite, case, "literal seed loops"))
      calls <- imi_invocations(block)
      literals <- imi_strings(block)
      adapters <- manifest$adapter[manifest$case_id == case]
      for (adapter in intersect(adapters, native)) {
        expect_true(any(calls %in% constructors[[adapter]]), info = paste(case, adapter, "native constructor"))
        expect_true(adapter %in% literals, info = paste(case, adapter, "genuine class assertion"))
      }
      for (adapter in intersect(adapters, names(routes))) {
        expect_true(all(routes[[adapter]] %in% calls), info = paste(case, adapter, "public route evidence"))
      }
      expect_false(any(startsWith(calls, "tabtools:::")), info = paste(case, "public numerical oracles"))
    }
  }
  runner <- parse(test_path("run_all.R"), keep.source = FALSE)
  assignments <- imi_walk(runner, function(x) imi_head(x) == "<-" &&
    is.symbol(x[[2L]]) && identical(as.character(x[[2L]]), "interaction_files"))
  expect_length(assignments, 1L)
  curated <- imi_strings(assignments[[1L]][[3L]])
  expect_setequal(unique(manifest$suite), curated)
  workflow <- readLines(test_path("..", ".github", "workflows", "R-CMD-check.yaml"), warn = FALSE)
  expect_true(any(grepl("run: Rscript qa/run_all.R interaction", workflow, fixed = TRUE)))
  expect_true(any(grepl("windows-latest", workflow, fixed = TRUE)))
})
