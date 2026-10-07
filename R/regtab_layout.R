# regtab layout (plan tasks 4.4-4.7, 4.9-4.10): the multi-model row union,
# row selection, cell text, dimnonsig, statistics and addrow rows, compact
# and nopvalue column layouts, headers, and stored results.

# ---------------------------------------------------------------------------
# Row union (task 4.4)

#' Union the per-model rows by raw key
#'
#' Stata's collect layout lists each coefficient name once, in order of first
#' appearance across the collected models, with `_cons` last (probed
#' 2026-09-25: `regress price mpg` + `regress price mpg weight` lists mpg,
#' weight, Intercept; `mpg turn` + `mpg weight turn` lists mpg, turn,
#' weight). A row absent from a model stays blank there. A key new to a
#' later model joins its own block (a new level stays under its factor).
#' @param mrows List of `tt_regtab_rows()` results.
#' @return list(rows = master data.frame, cells = list per model of
#'   data.frames aligned with rows: status, estimate, conf.low, conf.high,
#'   p.value, term, ancillary).
#' @keywords internal
#' @noRd
tt_regtab_union <- function(mrows) {
  mrows <- .rt_align_frames(mrows)
  master <- NULL
  for (m in seq_along(mrows)) {
    r <- mrows[[m]]
    r <- r[r$kind != "intercept", , drop = FALSE]
    for (i in seq_len(nrow(r))) {
      if (!is.null(master) && r$key[i] %in% master$key) next
      new <- r[i, intersect(c("key", "block", "kind", "label", "role", "parent_key", "term_signature", "level_identity", "parent_label"), names(r)), drop = FALSE]
      if (is.null(master)) {
        master <- new
      } else {
        same <- which(master$block == new$block)
        # In the multi-equation layout an equation's intercept stays last
        # within it (collect lists _cons last): a later model's new term
        # goes before it.
        if (length(same) && !grepl("::_cons$", new$key)) {
          body <- same[!grepl("::_cons$", master$key[same])]
          same <- if (length(body)) body else min(same) - 1L
        }
        at <- if (length(same)) max(same) else nrow(master)
        master <- rbind(master[seq_len(at), , drop = FALSE], new,
                        master[setdiff(seq_len(nrow(master)), seq_len(at)), , drop = FALSE])
      }
    }
  }
  # A variable (or factor header) row shows the first variable label any
  # model has: a model fitted on unlabelled data shows the bare name, which
  # must not hide a later model's label (backlog). Stata's models share one
  # dataset, so its labels never differ.
  if (!is.null(master) && length(mrows) > 1L) {
    for (i in which(master$kind %in% c("var", "cat_header"))) {
      bare <- c(master$key[i], master$block[i])
      if (!master$label[i] %in% bare) next
      for (r in mrows[-1]) {
        j <- match(master$key[i], r$key)
        if (!is.na(j) && !r$label[j] %in% bare) {
          master$label[i] <- r$label[j]
          break
        }
      }
    }
  }
  icpt <- do.call(rbind, lapply(mrows, function(r) r[r$kind == "intercept", intersect(c("key", "block", "kind", "label", "role", "parent_key", "term_signature", "level_identity", "parent_label"), names(r)), drop = FALSE]))
  if (!is.null(icpt) && nrow(icpt)) master <- rbind(master, icpt[1, , drop = FALSE])
  if (is.null(master)) master <- data.frame(key = character(), block = character(), kind = character(),
                                            label = character(), role = character(), stringsAsFactors = FALSE)
  rownames(master) <- NULL
  cells <- lapply(mrows, function(r) {
    hit <- match(master$key, r$key)
    cell <- data.frame(status = r$status[hit], estimate = r$estimate[hit], conf.low = r$conf.low[hit],
               conf.high = r$conf.high[hit], p.value = r$p.value[hit], term = r$term[hit],
               ancillary = r$ancillary[hit] %in% TRUE, stringsAsFactors = FALSE)
    for (field in setdiff(names(r), c("key", "block", "kind", "label", "role", "status", "estimate", "conf.low", "conf.high", "p.value", "term", "ancillary"))) cell[[field]] <- r[[field]][hit]
    cell$status[is.na(cell$status)] <- "absent"
    cell$status[cell$status == "base"] <- "ref"
    cell$status[cell$status == "cns"] <- "constrained"
    cell$status[cell$status == "header"] <- ""
    cell
  })
  list(rows = master, cells = cells)
}

# ---------------------------------------------------------------------------
# Multi-equation (coleq#colname) layout for single-equation models (Phase 5
# review P0-3)

#' Whether the table uses Stata's coleq#colname layout
#'
#' `regtab.ado:1338-1351`: with a multi-equation estimator (mlogit, zip,
#' zinb, churdle) in the collection, more than one equation, and no mixed
#' model, every row of every model reads `<equation label>: <label>`.
#' @keywords internal
#' @noRd
.rt_coleq_layout <- function(infos) {
  multi <- any(vapply(infos, function(i) !is.null(i$equations), TRUE))
  re <- any(vapply(infos, function(i) !identical(i$re_family %||% "none", "none"), TRUE))
  multi && !re
}

# The equation of a single-equation model in that layout: its dependent
# variable, labelled with the variable's label (probes B22, B23, D07, G01,
# G03, G34); `_t` ("Analysis time when record ends", stset's label) for
# stcox, stcrreg and streg (G23, G24); intreg's "model" (G32).
.rt_coleq_of <- function(fit, info) {
  cmd <- info$stata_cmd
  if (cmd %in% c("stcox", "stcrreg", "streg")) return(list(name = "_t", label = "Analysis time when record ends"))
  if (identical(cmd, "intreg")) return(list(name = "model", label = "model"))
  f <- tryCatch(stats::formula(fit), error = function(e) NULL)
  dv <- if (!is.null(f) && length(f) == 3L) all.vars(f[[2L]])[1] else NA_character_
  if (is.na(dv)) return(NULL)
  mf <- tryCatch(.rt_frame(fit)$mf, error = function(e) NULL)
  col <- if (!is.null(mf)) mf[[dv]] else NULL
  if (is.null(col)) {
    # A transformed response (Surv(y, ...), cbind(y, n - y)) keeps no
    # column of its own: read the variable from the model's data.
    src <- tryCatch({
      fd <- if (isS4(fit)) NULL else fit$data
      if (is.data.frame(fd)) fd else eval(stats::getCall(fit)$data, environment(stats::formula(fit)))
    }, error = function(e) NULL)
    if (is.data.frame(src)) col <- src[[dv]]
  }
  lab <- if (!is.null(col)) var_label(col, dv) else dv
  list(name = dv, label = .rt_eq_label(lab))
}

#' Re-key a single-equation model's rows into the multi-equation layout
#'
#' Rows of the model's own equation are keyed `<depvar>::<key>` (so they
#' merge with a zip/zinb count equation of the same outcome) and labelled
#' `<depvar label>: <label>`: factor header rows and indented level rows
#' as in the single-equation layout (tabtools 2.1.12; 2.1.11 showed the raw
#' key and no header), the intercept `Intercept`, fvgen rows by their fvgen
#' labels (probes G28, G29, G31).
#' Ancillary rows (cutpoints, ln_p, lnalpha, sigma, ...) belong to Stata's
#' `/` equation: `Ancillary: <label>` (G04, G24, D07, G35); intreg adds
#' its `lnsigma` equation's `Scale: Intercept` (G32); tobit's trailing
#' variance row reads `Ancillary: var(e.y)` (G33).
#' @keywords internal
#' @noRd
.rt_coleq_rows <- function(rows, fit, info, o, trailing = FALSE) {
  if (is.null(rows) || !nrow(rows)) return(rows)
  eq <- .rt_coleq_of(fit, info)
  if (is.null(eq)) return(rows)
  at <- attr(rows, "positional")
  anc_key <- function(k) .rt_eq_key("/", k)
  if (trailing) {
    rows$key <- anc_key(rows$key)
    rows$block <- "/"
    rows$label <- paste0("Ancillary: ", rows$label)
    rows$role <- "ancillary"
    return(rows)
  }
  anc <- rows$ancillary %in% TRUE
  lvl <- rows$kind %in% "level"
  core <- ifelse(lvl, rows$label, ifelse(rows$kind %in% "intercept", "Intercept", trimws(rows$label)))
  rows$label <- ifelse(anc, paste0("Ancillary: ", rows$label), paste0(eq$label, ": ", core))
  rows$key <- ifelse(anc, anc_key(rows$key), .rt_eq_key(eq$name, rows$key))
  rows$block <- ifelse(anc, "/", eq$name)
  # Every row of Stata's `/` equation is dropped by nointercept in this
  # layout (its "Ancillary:" rule), whatever the parameter.
  rows$role[anc] <- "ancillary"
  rows$kind[rows$kind %in% c("intercept", "flat", "int_level")] <- ifelse(
    rows$kind[rows$kind %in% c("intercept", "flat", "int_level")] == "int_level", "level", "var")
  rows$kind[rows$kind %in% "int_header"] <- "cat_header"
  if (identical(info$stata_cmd, "intreg")) {
    V <- tt_vcov(fit, o$vce)
    if ("Log(scale)" %in% rownames(V)) {
      w <- .rt_wald_named(c("Log(scale)" = log(fit$scale[1])), V, "Log(scale)", Inf, o$level)
      sc <- .rt_row(.rt_eq_key("lnsigma", "_cons"), "lnsigma", "var", "Scale: Intercept", "est",
                    term = "Log(scale)", estimate = w$estimate, conf.low = w$conf.low,
                    conf.high = w$conf.high, p.value = w$p.value, ancillary = TRUE, role = "ancillary")
      first_anc <- which(rows$block == "/")[1]
      rows <- if (is.na(first_anc)) rbind(rows, sc) else
        rbind(rows[seq_len(first_anc - 1L), , drop = FALSE], sc, rows[first_anc:nrow(rows), , drop = FALSE])
    }
  }
  rownames(rows) <- NULL
  attr(rows, "positional") <- at
  rows
}

# Structural rows (cutpoints and ancillary parameters: role "cutpoint",
# "ancillary", "auxiliary") are keyed in Stata's `/` equation, "/::<name>"
# (`regtab.ado` sees them as `/:cut1`, `/:ln_p`, ...), in every model before
# the union, so no model's covariate of the same name (`p`, `alpha`,
# `cut1`, `sigma`) can share their row (Milestone H review T2B-03: the union
# joined models by key alone; Stata tabtools 2.1.13 keeps them apart too,
# goldens R68-R73). keep()/drop() match the colname, so `cut1`
# and `ln_p` still select them. Rows already in an equation (the coleq
# layout, zeroinfl's `/::lnalpha`) are left alone.
.rt_ns_keys <- function(rows) {
  if (is.null(rows) || !nrow(rows) || is.null(rows$role)) return(rows)
  at <- attr(rows, "positional")
  idx <- rows$role %in% c("cutpoint", "ancillary", "auxiliary") & !grepl("::", rows$key, fixed = TRUE)
  if (any(idx)) {
    old <- rows$key[idx]
    rows$key[idx] <- .rt_eq_key("/", old)
    own <- rows$block[idx] == old
    rows$block[idx][own] <- rows$key[idx][own]
  }
  attr(rows, "positional") <- at
  rows
}

# A structural row whose name is also a covariate's keeps its own label
# (tabtools 2.1.12 labels the ancillary `alpha` of `nbreg y alpha` "alpha",
# beside the covariate; 2.1.11 showed Stata's colname `/p`, review T2B-14,
# which R copied): the two rows differ by key (`/::alpha`) and position.

# Preserve optional analytical provenance across different model classes and
# empty/failed models, filling unavailable fields with typed missing values.
.rt_align_frames <- function(frames) {
  fields <- unique(unlist(lapply(frames, names), use.names = FALSE))
  templates <- lapply(fields, function(field) {
    frames[[which(vapply(frames, function(x) field %in% names(x), TRUE))[1L]]][[field]]
  })
  names(templates) <- fields
  lapply(frames, function(x) {
    for (field in setdiff(fields, names(x))) x[[field]] <- templates[[field]][rep(NA_integer_, nrow(x))]
    x[fields]
  })
}

# Append a union of trailing rows below the coefficient union.
.rt_union_append <- function(u, v) {
  u$rows$trailing <- rep(FALSE, nrow(u$rows))
  if (!nrow(v$rows)) return(u)
  v$rows$trailing <- TRUE
  u$rows <- do.call(rbind, .rt_align_frames(list(u$rows, v$rows)))
  rownames(u$rows) <- NULL
  u$cells <- Map(function(a, b) {
    out <- do.call(rbind, .rt_align_frames(list(a, b)))
    rownames(out) <- NULL
    out
  }, u$cells, v$cells)
  u
}

.rt_subset <- function(u, keep) {
  u$rows <- u$rows[keep, , drop = FALSE]
  rownames(u$rows) <- NULL
  u$cells <- lapply(u$cells, function(c) {
    c <- c[keep, , drop = FALSE]
    rownames(c) <- NULL
    c
  })
  u
}

# ---------------------------------------------------------------------------
# Row selection (task 4.6, `_tabtools_match_rows.ado`)

# Normalise factor operators only (`_tabtools_match_normalize`): `i.`,
# `c.`, `o.`, `b.`, `bn.`, `ib#.`, `ibn.` are stripped; `2b.` / `2bn.` /
# `2o.` become `2.`. Identifiers and numeric levels are untouched.
.rt_match_normalize <- function(term) {
  vapply(term, function(t) {
    parts <- strsplit(t, "#", fixed = TRUE)[[1]]
    parts <- vapply(parts, function(p) {
      dot <- regexpr(".", p, fixed = TRUE)
      if (dot > 0) {
        pre <- substr(p, 1L, dot - 1L)
        suf <- substring(p, dot + 1L)
        if (grepl("^(i|c|o|b|bn|ib[0-9]+|ibn)$", pre)) {
          p <- suf
        } else if (grepl("^[0-9]+(b|bn|o)$", pre)) {
          p <- paste0(sub("^([0-9]+).*$", "\\1", pre), ".", suf)
        }
      }
      p
    }, "")
    paste(parts, collapse = "#")
  }, "", USE.NAMES = FALSE)
}

#' Which rows keep()/drop() terms select
#'
#' A term selects a row when it equals the row's normalised raw key, or (for
#' a term without `#`) equals any `#`-component of it, or is a bare variable
#' name equal to a component's variable part (`rep78` selects `3.rep78`).
#' An explicit level (`2.arm`) never selects another level (`20.arm`), and
#' interaction terms match only exactly (`_tabtools_match_rows.ado:44-86`).
#' R addition: a term equal to the row's R coefficient name also selects it,
#' and a bare variable name selects the rows of a function-call term of it
#' (`x` selects `poly(x, 2)1` and `poly(x, 2)2`).
#' @param keys Raw row keys.
#' @param terms Selection terms.
#' @param rterms R coefficient names per row (NA for none).
#' @return Logical vector.
#' @keywords internal
#' @noRd
tt_match_rows <- function(keys, terms, rterms = rep(NA_character_, length(keys))) {
  terms <- .rt_match_normalize(terms)
  out <- rep(FALSE, length(keys))
  for (i in seq_along(keys)) {
    if (!nzchar(keys[i])) next
    key <- .rt_match_normalize(trimws(keys[i]))
    parts <- strsplit(key, "#", fixed = TRUE)[[1]]
    for (t in terms) {
      if (identical(key, t) || (!is.na(rterms[i]) && identical(rterms[i], t))) {
        out[i] <- TRUE
        next
      }
      if (grepl("#", t, fixed = TRUE)) next
      # R addition (muse P2-4): a bare variable name selects the columns of
      # a basis term built from it (poly(x, 2)1, ns(x, 3)2: a call followed
      # by its column number), which Stata has no key for; not a
      # random-effect row var(x) nor a wrapped factor's 6.factor(x).
      if (grepl("^[A-Za-z][A-Za-z0-9._:]*\\(.*\\)[0-9]+$", key) && t %in% .rt_key_vars(key)) out[i] <- TRUE
      for (comp in parts) {
        if (identical(comp, t)) out[i] <- TRUE
        dot <- regexpr(".", comp, fixed = TRUE)
        if (dot > 0 && !grepl(".", t, fixed = TRUE) &&
            grepl("^[0-9]+$", substr(comp, 1L, dot - 1L)) &&
            identical(substring(comp, dot + 1L), t)) out[i] <- TRUE
      }
    }
  }
  out
}

# Variables of a function-call row key such as "poly(x, 2)1" (its column
# number dropped); none when it does not parse.
.rt_key_vars <- function(key) {
  e <- tryCatch(str2lang(sub("[0-9]+$", "", key)), error = function(e) NULL)
  if (is.null(e)) character() else all.vars(e)
}

# Any model's R coefficient name for each union row.
.rt_row_terms <- function(u) {
  tm <- rep(NA_character_, nrow(u$rows))
  for (c in u$cells) tm[is.na(tm)] <- c$term[is.na(tm)]
  tm
}

.rt_select <- function(u, keep = NULL, drop = NULL, labelmatch = FALSE) {
  for (sel in c("keep", "drop")) {
    spec <- if (sel == "keep") keep else drop
    if (is.null(spec)) next
    n_pre <- nrow(u$rows)
    hit <- if (labelmatch) {
      lab <- tolower(u$rows$label)
      Reduce(`|`, lapply(tolower(spec), function(t) grepl(t, lab, fixed = TRUE)), rep(FALSE, n_pre))
    } else {
      tt_match_rows(.rt_match_key(u$rows$key), spec, .rt_row_terms(u))
    }
    wrapped <- if (labelmatch) character() else .rt_wrapped_terms(spec, u$rows$key)
    if (sel == "keep") {
      if (n_pre > 0 && !any(hit)) {
        cli::cli_abort(c("{.arg keep} matched no coefficient rows; check exact names or use {.code labelmatch = TRUE} for display labels.",
                         "i" = if (length(wrapped)) .rt_wrapped_hint(wrapped)),
                       call = NULL)
      }
      u <- .rt_subset(u, hit)
    } else {
      if (n_pre > 0 && sum(hit) >= n_pre) {
        cli::cli_abort("{.arg drop} would remove every coefficient row, leaving an empty table; check the {.arg drop} spec.",
                       call = NULL)
      }
      if (length(wrapped)) {
        cli::cli_warn(c("{.arg drop} term{?s} {.val {names(wrapped)}} matched no row.",
                        "i" = .rt_wrapped_hint(wrapped)))
      }
      u <- .rt_subset(u, !hit)
    }
  }
  u
}

# One hint for all such terms (a vector would split into several bullets
# named i1, i2, ...; stack review).
.rt_wrapped_hint <- function(wrapped) {
  paste0(paste0("{.val ", names(wrapped), "} is {.val ", wrapped, "}", collapse = ", "),
         " in the model: write that, or convert the column in the data.")
}

# keep()/drop() terms that match no row but name the variable of a factor
# made in the formula (`factor(period)`, keyed `factor(period)` and
# `2.factor(period)`; backlog): each such term, named, with the key that
# would match.
.rt_wrapped_terms <- function(spec, keys) {
  bare <- unique(sub("^.*::", "", sub("^[^.(]*\\.", "", unlist(strsplit(keys, "#", fixed = TRUE)))))
  inner <- sub("^[A-Za-z_.][A-Za-z0-9_.]*\\((.*)\\)$", "\\1", bare)
  out <- character()
  for (t in unique(trimws(spec))) {
    if (!nzchar(t) || t %in% bare) next
    k <- bare[inner == t & bare != t]
    if (length(k)) out[t] <- k[1]
  }
  out
}

# Factor level codes that are level positions (a factor without Stata value
# codes, e.g. from haven::as_factor()) differ from the Stata codes a native
# key or a keep()/drop() level term means ("0.foreign" is Domestic in Stata,
# while the first level has position 1). Say so whenever the table relies
# on them (review P1-1).
.rt_note_positional <- function(mrows, o) {
  pos <- unique(unlist(lapply(mrows, attr, "positional")))
  if (!length(pos)) return(invisible(NULL))
  comp_vars <- function(keys) {
    parts <- unlist(strsplit(.rt_match_normalize(keys), "#", fixed = TRUE))
    lv <- grepl("^[0-9]+\\.", parts)
    unique(sub("^[0-9]+\\.", "", parts[lv]))
  }
  used <- character()
  if (identical(o$interactions, "native")) {
    for (r in mrows) {
      k <- r$key[r$kind %in% "int_level"]
      used <- c(used, intersect(comp_vars(k), pos))
    }
  }
  if (!isTRUE(o$labelmatch)) used <- c(used, intersect(comp_vars(c(o$keep, o$drop)), pos))
  used <- unique(used)
  if (length(used)) {
    cli::cli_inform(c(
      "i" = "Level codes of {.var {used}} are level positions (1, 2, ...), not Stata value codes.",
      " " = "Native interaction keys and {.arg keep}/{.arg drop} level terms such as {.val {paste0('1.', used[1])}} use them; convert haven-labelled data with {.fn tt_as_factor} to keep the Stata codes."
    ))
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Cell text (task 4.5)

# Estimate text: Stata pre-rounds with round(x, 10^-digits) and formats with
# %32.<digits>f (`regtab.ado:1983-1989`, `:2079-2085`); CI bounds are
# formatted without the pre-round (`:2146`).
.rt_est_text <- function(x, digits, numeric_format = NULL) {
  if (!is.null(numeric_format$cformat)) {
    out <- rep("", length(x))
    ok <- is.finite(x)
    out[ok] <- .tt_format_numeric(x[ok], numeric_format)
    return(out)
  }
  fmt <- paste0("%32.", digits, "f")
  ifelse(is.finite(x), stata_fmt(stata_round(x, 10^-digits), fmt), "")
}

.rt_ci_text <- function(lo, hi, digits, sep, numeric_format = NULL) {
  fmt <- paste0("%32.", digits, "f")
  ok <- is.finite(lo) & is.finite(hi)
  out <- rep("", length(lo))
  render <- function(x) if (is.null(numeric_format$cformat)) stata_fmt(x, fmt) else
    .tt_format_numeric(x, numeric_format)
  out[ok] <- paste0("(", render(lo[ok]), sep, render(hi[ok]), ")")
  out
}

# Numeric p read back from its displayed text, as regtab's boldp/highlight
# do (`regtab.ado:3000-3011`): "<0.001" counts as 0 and anything real()
# cannot parse (">0.99", blanks) is missing.
.rt_p_from_text <- function(s) {
  s <- trimws(s)
  out <- suppressWarnings(as.numeric(s))
  out[startsWith(s, "<")] <- 0
  out[startsWith(s, ">")] <- NA_real_
  out
}

# dimnonsig (`regtab.ado:2154-2197`): a row is non-significant unless some
# model's raw (unrounded) CI excludes that model's null; rows without a CI
# are not dimmed unless they are reference/omitted/empty rows; a category
# parent row (no CI, no values) is dimmed only when it has a reference child
# and no significant child. Children are identified by structural parent keys.
# Analytical states and structural blocks, before publication masks.
.rt_dimnonsig <- function(u, nulls) {
  n <- nrow(u$rows)
  seen <- significant <- constrained <- rep(FALSE, n)
  for (m in seq_along(u$cells)) {
    c <- u$cells[[m]]
    has <- c$status %in% "est" & !c$ancillary &
      is.finite(c$conf.low) & is.finite(c$conf.high) & c$conf.low <= c$conf.high
    seen <- seen | has
    significant <- significant | (has & (c$conf.high < nulls[m] | c$conf.low > nulls[m]))
    constrained <- constrained | c$status %in% c("base", "ref", "omit", "empty", "notest", "cns", "constrained")
  }
  dim <- (seen | constrained) & !significant
  heads <- which(u$rows$kind %in% c("cat_header", "int_header"))
  for (i in heads) {
    parent <- .rt_placement_parent(u$rows)
    child <- which(parent == u$rows$key[i] & !u$rows$kind %in% c("cat_header", "int_header"))
    dim[i] <- any(constrained[child]) && !any(significant[child])
  }
  dim
}

# r(table) row names (`regtab.ado:2923-2957`, 2.1.12): "." and " " become
# "_", "," and ":" are removed, the result is cut to 32 bytes. A name that
# `matrix rownames` would not store unchanged (an interaction "a#b", a
# covariance "cov(x_cons)" read back as a variance, a rejected bracketed key;
# see .stata_rowname_survives()) has every character outside [A-Za-z0-9_]
# replaced by "_" ("1_foreign#3_rep78" -> "1_foreign_3_rep78"). Up to 2.1.9
# the interaction was stored as "c.1_foreign#c.3_rep78" and a rejected key
# sent every row to r1, r2, ... Since 2.1.12 the names are also unique in
# row order: a repeat takes the first free suffix _2, _3, ..., its base cut
# to 32 bytes less the suffix (golden R40: two labels sharing their first
# 32 characters).
.rt_rowname <- function(label, k) {
  s <- gsub(".", "_", label, fixed = TRUE)
  s <- gsub(" ", "_", s, fixed = TRUE)
  s <- gsub(",", "", s, fixed = TRUE)
  s <- gsub(":", "", s, fixed = TRUE)
  s <- vapply(s, function(x) {
    b <- charToRaw(enc2utf8(x))
    if (length(b) > 32L) {
      b <- b[seq_len(32L)]
      x <- rawToChar(b)
      Encoding(x) <- "UTF-8"
    }
    x
  }, "", USE.NAMES = FALSE)
  s[!nzchar(s)] <- paste0("row", k[!nzchar(s)])
  bad <- !.stata_rowname_survives(s)
  s[bad] <- substr(gsub("[^A-Za-z0-9_]", "_", enc2utf8(s[bad]), perl = TRUE), 1L, 32L)
  bytes32 <- function(x, n) {
    b <- charToRaw(enc2utf8(x))
    if (length(b) <= n) return(x)
    x <- rawToChar(b[seq_len(n)])
    Encoding(x) <- "UTF-8"
    x
  }
  for (i in seq_along(s)) {
    base <- s[i]
    j <- 1L
    while (s[i] %in% s[seq_len(i - 1L)]) {
      j <- j + 1L
      tail <- paste0("_", j)
      s[i] <- paste0(bytes32(base, 32L - nchar(tail)), tail)
    }
  }
  s
}

# ---------------------------------------------------------------------------
# Statistics rows (task 4.9, `regtab.ado:2364-2572`; task 5.17, tabtools
# 2.1.12 `regtab.ado:808-843`, `:2976-3260`, `:3549`)

# The stats() token registry, in Stata's row order (2.1.12): counts first
# (Observations or Subjects, Events, Groups, Imputations), then the
# likelihood criteria (AIC, QICu, BIC, Log-likelihood), then ICC, R-squared
# and the linear-model rows (Adjusted R-squared, Root MSE, F statistic),
# with Largest FMI last, whatever the order of the tokens, and the R-only
# text row `vce` after it (Stata regtab has no such token). Each entry: the
# tokens that request it (lower case, as Stata lowers them), the
# tt_model_stats() field it shows, its label and display format, and the
# name of its stored r() scalar (`<stored>_<model>`). `n`, `aic`/`qic` and
# `r2` have their own rules in .rt_stats_rows() (Subjects vs Observations,
# the QICu fallback, the pseudo R-squared label); their label and format
# here are the defaults.
.rt_stat_registry <- list(
  n = list(tokens = c("n", "n_sub", "subjects"), field = "N", label = "Observations", fmt = "%12.0fc", stored = "n"),
  obs = list(tokens = "obs", field = "obs", label = "Observations", fmt = "%12.0fc", stored = "obs"),
  events = list(tokens = "events", field = "events", label = "Events", fmt = "%12.0fc", stored = "events"),
  people = list(tokens = "people", field = "people", label = "People", fmt = "%12.0fc", stored = "people"),
  exposure = list(tokens = "exposure", field = "exposure", label = "Exposure", fmt = "%12.0fc", stored = "exposure"),
  groups = list(tokens = "groups", field = "groups", label = "Groups", fmt = "%12.0fc", stored = "groups"),
  mi_m = list(tokens = "mi_m", field = "mi_m", label = "Imputations", fmt = "%12.0fc", stored = "mi_m"),
  aic = list(tokens = "aic", field = "aic", label = "AIC", fmt = "%12.2f", stored = "aic"),
  qic = list(tokens = "qic", field = "qic", label = "QICu", fmt = "%12.2f", stored = "qic"),
  bic = list(tokens = "bic", field = "bic", label = "BIC", fmt = "%12.2f", stored = "bic"),
  ll = list(tokens = "ll", field = "ll", label = "Log-likelihood", fmt = "%12.2f", stored = "ll"),
  icc = list(tokens = "icc", field = "icc", label = "ICC", fmt = "%5.3f", stored = "icc"),
  r2 = list(tokens = c("r2", "r-squared"), field = "r2", label = "R\u00b2", fmt = "%5.3f", stored = NA_character_),
  r2_a = list(tokens = "r2_a", field = "r2_a", label = "Adjusted R\u00b2", fmt = "%5.3f", stored = "r2_a"),
  rmse = list(tokens = "rmse", field = "rmse", label = "Root MSE", fmt = "%9.3f", stored = "rmse"),
  F = list(tokens = "f", field = "F", label = "F statistic", fmt = "%9.2f", stored = "F"),
  fmi = list(tokens = "fmi", field = "fmi", label = "Largest FMI", fmt = "%6.4f", stored = "fmi"),
  # R only (task 5.18): each model's variance as text, in the vce_note
  # footnote's words (.rt_vce_label()); no stored scalar, as for r2.
  vce = list(tokens = "vce", field = "vce_text", label = "Standard errors", fmt = NA_character_, stored = NA_character_)
)

.rt_stat_tokens <- unlist(lapply(.rt_stat_registry, `[[`, "tokens"), use.names = FALSE)

# stats: one space-separated string or a character vector of tokens
# (case-insensitive). Returns one logical per registry entry (in registry
# order), or NULL when no stats were asked for. Unknown tokens warn, as in
# Stata, and are ignored.
.rt_parse_stats <- function(stats) {
  if (is.null(stats)) return(NULL)
  if (!is.character(stats) || anyNA(stats)) {
    cli::cli_abort("{.arg stats} must be a character vector of tokens, e.g. {.code c(\"n\", \"aic\")}.", call = NULL)
  }
  raw <- unlist(strsplit(paste(stats, collapse = " "), "[[:space:]]+"))
  raw <- raw[nzchar(raw)]
  tok <- tolower(raw)
  # Each unknown token as typed, once per occurrence, as Stata echoes it
  # (`regtab.ado:838-844`; C4 review F8).
  for (b in raw[!tok %in% .rt_stat_tokens]) {
    cli::cli_warn("{.arg stats} token {.val {b}} not recognized and ignored; valid: n (n_sub/subjects) events groups mi_m aic bic qic ll icc r2 r2_a rmse F fmi; R only: vce.")
  }
  lapply(.rt_stat_registry, function(e) any(tok %in% e$tokens))
}

# Row keys of the body (task 6.7): the Stata-style key keep/drop match
# (`wt`, `cyl`, `6.cyl`, `4.cyl#1.am`, `_cons`, an equation's `4::wt`),
# its variable (`wt`, `cyl`, `cyl#am`, `_cons`; the equation dropped) and,
# for a level or interaction cell, its level codes (`6`, `4#1`; `c` for a
# continuous factor of an interaction, as in `1.foreign#c.mpg`).
# Converters and composites align rows on these, never on the labels. Only
# level rows are split at the dot: a variable may have one in its name.
.rt_row_keys <- function(ur) {
  key <- as.character(ur$key)
  var <- sub("^.*::", "", key)
  level <- rep(NA_character_, length(key))
  for (i in which(ur$kind %in% c("level", "int_level", "flat"))) {
    parts <- strsplit(var[i], "#", fixed = TRUE)[[1]]
    # A column of a matrix term (`poly(x, 2)1`) has no level code.
    if (!all(grepl(".", parts, fixed = TRUE))) next
    level[i] <- paste(sub("\\..*$", "", parts), collapse = "#")
    var[i] <- paste(sub("^[^.]*\\.", "", parts), collapse = "#")
  }
  data.frame(key = key, var = var, level = level, stringsAsFactors = FALSE)
}

# Rows of (label, one value text per model) in Stata's fixed order, plus the
# stored per-model scalars. A row whose statistic no model reports is
# omitted; a model that does not report it has a blank cell.
.rt_stats_rows <- function(want, st) {
  M <- length(st)
  get <- function(nm) vapply(st, function(s) as.numeric(s[[nm]] %||% NA_real_)[1], 0)
  rows <- list()
  add <- function(label, vals, fmt, key = nm) {
    txt <- ifelse(is.na(vals), "", stata_fmt(vals, fmt))
    rows[[length(rows) + 1L]] <<- list(label = label, values = txt, key = paste0("stat:", key),
      raw_values = list(vals), part_names = key,
      part_states = list(ifelse(is.na(vals), "blank", "available")))
  }
  vals <- list()
  N <- get("N")
  Nsub <- get("N_sub")
  any_sub <- any(!is.na(Nsub))
  Nshow <- ifelse(!is.na(Nsub), Nsub, N)
  # A weighted Cox model whose weights vary within subject: blank (P2-7).
  # A weighted tt() coxph whose observations' weights cannot be recovered:
  # blank too, never the unweighted count.
  Nshow[vapply(st, function(s) isTRUE(s$nsub_note %in% c("varies", "unverified")), TRUE)] <- NA_real_
  qic_by_aic <- FALSE
  for (nm in names(.rt_stat_registry)) {
    if (!isTRUE(want[[nm]])) next
    e <- .rt_stat_registry[[nm]]
    if (nm == "vce") {
      # Text, not a number: shown as it is, blank where unknown.
      txt <- vapply(st, function(s) as.character(s$vce_text %||% "")[1], "")
      if (any(nzchar(txt))) rows[[length(rows) + 1L]] <- list(label = e$label, values = txt, key = "stat:vce",
        raw_values = list(), part_names = character(), part_states = list())
      next
    }
    v <- if (nm == "n") Nshow else get(e$field)
    vals[[nm]] <- v
    if (nm == "n") {
      if (any(!is.na(v))) add(if (any_sub) "Subjects" else "Observations", v, e$fmt)
    } else if (nm == "aic") {
      # Fixed-scale GEE models have no AIC: the row falls back to QICu.
      if (any(!is.na(v))) {
        add(e$label, v, e$fmt)
      } else if (any(!is.na(get("qic")))) {
        add("QICu", get("qic"), e$fmt, key = "qic")
        qic_by_aic <- TRUE
      }
    } else if (nm == "qic") {
      if (!qic_by_aic && any(!is.na(v))) add(e$label, v, e$fmt)
    } else if (nm == "r2") {
      # Prefer r2, then r2_p (pseudo), then r2_a (adjusted).
      r2p <- get("r2_p")
      val <- ifelse(!is.na(v), v, ifelse(!is.na(r2p), r2p, get("r2_a")))
      if (any(!is.na(val))) {
        lab <- if (!any(!is.na(v)) && any(!is.na(r2p))) "Pseudo R\u00b2" else
          if (any(!is.na(v)) && any(!is.na(r2p))) "R\u00b2 / Pseudo R\u00b2" else "R\u00b2"
        add(lab, val, e$fmt)
      }
    } else if (any(!is.na(v))) {
      add(e$label, v, e$fmt)
    }
  }
  # Full-precision stored scalars (`regtab.ado:3540-3558`): aic, bic, qic
  # (with stats(qic) or stats(aic)), ll, n, groups, the 2.1.12 tokens, icc.
  stored <- list()
  qic <- get("qic")
  for (m in seq_len(M)) {
    for (nm in c("aic", "bic", "qic", "ll", "n", "obs", "people", "exposure", "groups", "events", "mi_m", "r2_a", "rmse", "F", "fmi", "icc")) {
      w <- if (nm == "qic") isTRUE(want$qic) || isTRUE(want$aic) else isTRUE(want[[nm]])
      v <- if (nm == "qic") qic else vals[[nm]]
      if (w && !is.null(v) && !is.na(v[m])) stored[[paste0(.rt_stat_registry[[nm]]$stored, "_", m)]] <- v[m]
    }
  }
  list(rows = rows, stored = stored)
}

# stat_fun (R only): user rows below the Stata statistics, one per named
# entry, each a function of the fit (a tt_mi() object for a multiply imputed
# model) returning one number (shown in `fmt`, default %9.3f), one string
# (shown as it is), or NA/NULL (a blank cell); or list(fun = , fmt = ). As
# for the Stata rows, a row no model fills is omitted.
.rt_parse_stat_fun <- function(stat_fun) {
  if (is.null(stat_fun)) return(list())
  if (is.function(stat_fun) || !is.list(stat_fun) || is.null(names(stat_fun)) || any(!nzchar(names(stat_fun))) ||
      anyNA(names(stat_fun))) {
    cli::cli_abort("{.arg stat_fun} must be a named list of functions, e.g. {.code list(\"C statistic\" = function(fit) ...)}.",
                   call = NULL)
  }
  if (anyDuplicated(names(stat_fun))) {
    cli::cli_abort("The labels of {.arg stat_fun} must be unique.", call = NULL)
  }
  lapply(names(stat_fun), function(nm) {
    x <- stat_fun[[nm]]
    fmt <- "%9.3f"
    if (is.list(x) && !is.function(x)) {
      fmt <- x$fmt %||% fmt
      x <- x$fun
    }
    if (!is.function(x)) {
      cli::cli_abort("{.arg stat_fun} entry {.val {nm}} must be a function of the fit, or {.code list(fun = , fmt = )}.",
                     call = NULL)
    }
    if (!is.character(fmt) || length(fmt) != 1L || is.na(fmt)) {
      cli::cli_abort("{.arg stat_fun} entry {.val {nm}}: {.arg fmt} must be one Stata display format such as {.val %9.3f}.",
                     call = NULL)
    }
    .parse_stata_fmt(fmt)
    list(label = nm, fun = x, fmt = fmt)
  })
}

.rt_stat_fun_rows <- function(sf, fits) {
  rows <- list()
  for (e in sf) {
    txt <- vapply(seq_along(fits), function(m) {
      if (.rt_failed(fits[[m]])) return("")
      v <- tryCatch(e$fun(fits[[m]]), error = function(err) {
        cli::cli_abort("{.arg stat_fun} entry {.val {e$label}} failed for model {m}.", parent = err, call = NULL)
      })
      if (is.null(v) || (length(v) == 1L && is.na(v))) return("")
      if (length(v) != 1L || !(is.numeric(v) || is.character(v))) {
        cli::cli_abort("{.arg stat_fun} entry {.val {e$label}} must return one number or one string (model {m}).",
                       call = NULL)
      }
      if (is.character(v)) v else stata_fmt(v, e$fmt)
    }, "")
    # statfun:<label>, apart from Stata's stat:<token> (a stat_fun named
    # "n" beside stats = "n"; stack review item 5).
    if (any(nzchar(txt))) rows[[length(rows) + 1L]] <- list(label = e$label, values = txt, key = paste0("statfun:", e$label))
  }
  rows
}

# Notes Stata prints when a requested statistic cannot be shown
# (`regtab.ado:987-999`, `:935-942`): ICC for models whose family has no
# closed-form level-1 variance (a stats method sets `icc_note`), QICu for
# GEE models whose dispersion is not fixed at 1 (`qic_note`).
.rt_stat_notes <- function(st, want) {
  flag <- function(nm) which(vapply(st, function(s) isTRUE(s[[nm]]), TRUE))
  icc <- flag("icc_note")
  if (isTRUE(want$icc) && length(icc)) {
    msg <- if (length(icc) == length(st)) {
      "Note: ICC not computed (no closed-form level-1 variance for the requested model family)"
    } else {
      paste0("Note: ICC not computed for model(s) ", paste(icc, collapse = " "), " (no closed-form level-1 variance)")
    }
    cli::cli_inform(msg)
  }
  # Probability-weight statistics regtab does not reproduce (review P0-4,
  # P3-1): a family whose Stata [pw] pseudo-log-likelihood is unverified,
  # or a weighted Efron Cox fit (stcox refuses it).
  which_note <- function(v) which(vapply(st, function(s) identical(s$pw_note, v), TRUE))
  want_ll <- isTRUE(want$ll) || isTRUE(want$aic) || isTRUE(want$bic) || isTRUE(want$r2)
  fam <- which_note("family")
  if (want_ll && length(fam)) {
    cli::cli_inform(paste0(
      "Note: log-likelihood, AIC, BIC and pseudo R\u00b2 not shown for model(s) ", paste(fam, collapse = " "),
      ": Stata's pseudo-log-likelihood under probability weights has not been verified for this family"))
  }
  efr <- which_note("efron")
  if (want_ll && length(efr)) {
    cli::cli_inform(paste0(
      "Note: log-likelihood, AIC and BIC not shown for model(s) ", paste(efr, collapse = " "),
      ": weighted Cox fits with Efron ties have no Stata analogue (use ties = \"breslow\")"))
  }
  # nbreg's constant-only model could not be fitted (review T2B-16).
  r2n <- flag("r2p_note")
  if (isTRUE(want$r2) && length(r2n)) {
    cli::cli_inform(paste0(
      "Note: pseudo R\u00b2 not shown for model(s) ", paste(r2n, collapse = " "),
      ": the constant-only negative binomial model (Stata's e(ll_0)) could not be fitted"))
  }
  rsp <- which_note("response")
  if (want_ll && length(rsp)) {
    cli::cli_inform(paste0(
      "Note: log-likelihood, AIC and BIC not shown for model(s) ", paste(rsp, collapse = " "),
      ": the fit keeps no response (y = FALSE) and its data could not be checked, so Stata's weighted pseudo-log-likelihood cannot be formed"))
  }
  ttn <- which_note("tt")
  if (want_ll && length(ttn)) {
    cli::cli_inform(paste0(
      "Note: log-likelihood, AIC and BIC not shown for model(s) ", paste(ttn, collapse = " "),
      ": the fit's tt() terms expand its data over the event times, and the observations' weights and statuses could not be matched to the fit's expanded records (a fit with y = FALSE keeps none of their statuses), so Stata's weighted pseudo-log-likelihood cannot be formed"))
  }
  # Weighted Cox subjects (review policy (c), P2-7).
  if (isTRUE(want$n)) {
    ws <- which(vapply(st, function(s) identical(s$nsub_note, "weighted"), TRUE))
    if (length(ws)) {
      cli::cli_inform(paste0(
        "Note: Subjects for model(s) ", paste(ws, collapse = " "),
        " is the sum of the subjects' weights, as Stata's e(N_sub) after stset [pweight]"))
    }
    vs <- which(vapply(st, function(s) identical(s$nsub_note, "varies"), TRUE))
    if (length(vs)) {
      cli::cli_inform(paste0(
        "Note: Subjects not shown for model(s) ", paste(vs, collapse = " "),
        ": the weights vary within subject (the fit's id, or its cluster), which Stata's stset refuses"))
    }
    us <- which(vapply(st, function(s) identical(s$nsub_note, "unverified"), TRUE))
    if (length(us)) {
      cli::cli_inform(paste0(
        "Note: Subjects not shown for model(s) ", paste(us, collapse = " "),
        ": the fit's tt() terms expand its data over the event times, and the observations' own weights could not be matched to the fit's expanded records (a fit with y = FALSE keeps too little to match them unless its weights are all equal: refit with y = TRUE)"))
    }
  }
  # Weighted Cox failures (task 5.17): Stata's e(N_fail) after stset
  # [pweight] is the sum of the failures' weights, kept with a note as the
  # weighted Subjects are.
  if (isTRUE(want$events)) {
    we <- which(vapply(st, function(s) identical(s$events_note, "weighted"), TRUE))
    if (length(we)) {
      cli::cli_inform(paste0(
        "Note: Events for model(s) ", paste(we, collapse = " "),
        " is the sum of the failures' weights, as Stata's e(N_fail) after stset [pweight]"))
    }
    # A weighted coxph whose statuses cannot be paired with its weights
    # (y = FALSE and data that cannot be checked row by row, such as a tt()
    # fit): blank, never the unweighted count.
    wr <- which(vapply(st, function(s) identical(s$events_note, "response"), TRUE))
    if (length(wr)) {
      cli::cli_inform(paste0(
        "Note: Events not shown for model(s) ", paste(wr, collapse = " "),
        ": the fit keeps no response (y = FALSE) and its data could not be checked, so the sum of the failures' weights (Stata's e(N_fail) after stset [pweight]) cannot be formed"))
    }
  }
  qic <- flag("qic_note")
  if ((isTRUE(want$qic) || isTRUE(want$aic)) && length(qic)) {
    cli::cli_inform(c(
      paste0("Note: QICu unavailable for GEE model(s) ", paste(qic, collapse = " "), ": dispersion is not fixed at 1"),
      " " = "use a fixed scale of 1 (geeglm(scale.fix = TRUE)), or compute all candidates externally with one common scale"
    ))
  }
  invisible(NULL)
}

# addrow: list("Label" = values) or a Stata-style string
# `"P trend" 0.032 \ "P interaction" 0.15`. Values fill the models' first
# columns positionally, as typed (`regtab.ado:2581-2625`).
.rt_parse_addrow <- function(addrow) {
  if (is.null(addrow)) return(list())
  if (is.character(addrow) && is.null(names(addrow)) && length(addrow) == 1L) {
    chunks <- trimws(strsplit(addrow, "\\", fixed = TRUE)[[1]])
    chunks <- chunks[nzchar(chunks)]
    return(lapply(chunks, function(ch) {
      m <- regmatches(ch, regexec('^"([^"]*)"\\s*(.*)$', ch))[[1]]
      if (length(m)) {
        lab <- m[2]
        rest <- m[3]
      } else {
        lab <- sub("\\s.*$", "", ch)
        rest <- sub("^\\S+\\s*", "", ch)
      }
      vals <- strsplit(trimws(rest), "[[:space:]]+")[[1]]
      list(label = lab, values = vals[nzchar(vals)])
    }))
  }
  if (!is.list(addrow) && !is.atomic(addrow) || is.null(names(addrow)) || any(!nzchar(names(addrow)))) {
    cli::cli_abort("{.arg addrow} must be a named list, e.g. {.code list(\"P trend\" = c(0.032, 0.041))}.",
                   call = NULL)
  }
  lapply(seq_along(addrow), function(i) {
    v <- addrow[[i]]
    # Numbers print in full decimal form, as typed in Stata's addrow()
    # (100000 and 0.00001, not 1e+05 and 1e-05; review P2-2); strings are
    # kept verbatim.
    txt <- if (is.numeric(v)) {
      vapply(v, function(x) {
        if (is.na(x)) "" else .tt_fmt_sig(x, 15L)
      }, "")
    } else as.character(v)
    list(label = names(addrow)[i], values = txt)
  })
}

# ---------------------------------------------------------------------------
# Assembly

#' Build the regtab tt_table from fitted models
#' @keywords internal
#' @noRd
tt_regtab_build <- function(fits, infos, o) {
  M <- length(fits)
  scale <- tt_models_scale(infos, coef = o$coef, cdisc = o$cdisc,
                           nointercept = o$nointercept, keepintercept = o$keepintercept)
  # Per-model variance (milestone 5w): o$vce and o$cluster hold one entry
  # per model; every row hook sees its own model's pair as o$vce/o$cluster.
  vces <- o$vce
  clusters <- o$cluster
  if (!is.list(vces)) vces <- rep(list(vces %||% "stata"), M)
  if (!is.list(clusters) || length(clusters) != M) clusters <- rep(list(NULL), M)
  om <- lapply(seq_len(M), function(m) {
    x <- o
    x$vce <- vces[[m]]
    x["cluster"] <- list(clusters[[m]])
    x
  })
  # Variance fallbacks policy allows (mixed models, H-D6) warn once per
  # model and call, and are recorded in stored$vce_fallback.
  fallback <- vector("list", M)
  with_fb <- function(m, expr) {
    withCallingHandlers(expr, tabtools_vce_fallback = function(w) {
      seen <- w$fallback %in% fallback[[m]]
      if (!seen) fallback[[m]] <<- c(fallback[[m]], w$fallback)
      if (seen) invokeRestart("muffleWarning")
    })
  }
  failed <- vapply(fits, .rt_failed, TRUE)
  mrows <- lapply(seq_len(M), function(m) {
    if (failed[m]) return(.rt_failed_rows())
    with_fb(m, tt_regtab_rows(fits[[m]], infos[[m]], level = o$level, ci_method = o$ci_method,
                              interactions = o$interactions, xsymbol = o$xsymbol, vsref = o$vsref,
                              vce = vces[[m]], cluster = clusters[[m]]))
  })
  # A multiply imputed model (tt_mi, task 5.14) has its numbers pooled in
  # its rows and stats methods; everything that reads the model's frame,
  # labels or structure reads its first imputation.
  is_mi <- vapply(fits, inherits, TRUE, "tt_mi")
  fits1 <- lapply(fits, .rt_mi_first)
  mrows <- lapply(seq_len(M), function(m) .rt_count_rows(mrows[[m]], fits[[m]], (o$fitcounts %||% rep(list(NULL), M))[[m]]))
  .rt_note_positional(mrows, o)
  mrows <- .rt_join_levels(mrows, fits1)
  rejoined <- attr(mrows, "rejoined")
  # Random-effects checks across models (R/regtab_models_mixed.R).
  o <- .rt_re_prepare(fits1[!failed], infos[!failed], o)
  for (m in seq_len(M)) {
    keep <- om[[m]][c("vce", "cluster")]
    om[[m]] <- o
    om[[m]]$vce <- keep$vce
    om[[m]]["cluster"] <- list(keep$cluster)
  }
  # P1-8: a glmmTMB covariance interval left blank is explained in the
  # footnote (every sink) as well as on the console.
  re_notes <- character()
  trows <- lapply(seq_len(M), function(m) {
    if (failed[m]) return(.rt_failed_rows())
    withCallingHandlers(
      with_fb(m, tt_regtab_trailing_rows(fits1[[m]], infos[[m]], om[[m]])),
      tabtools_note_tmb_cov_ci = function(cnd) {
        fn <- paste0("Model ", m, ": ", cnd$footnote)
        if (!fn %in% re_notes) re_notes <<- c(re_notes, fn)
      }) %||%
      .rt_row("", "", "", "")[0L, , drop = FALSE]
  })
  # Beside a multi-equation model every model takes the coleq#colname
  # layout (review P0-3).
  if (.rt_coleq_layout(infos)) {
    for (m in seq_len(M)) {
      if (failed[m] || !is.null(infos[[m]]$equations) || is.data.frame(fits[[m]])) next
      mrows[[m]] <- .rt_coleq_rows(mrows[[m]], fits1[[m]], infos[[m]], om[[m]])
      trows[[m]] <- .rt_coleq_rows(trows[[m]], fits1[[m]], infos[[m]], om[[m]], trailing = TRUE)
    }
  }
  # Two row hooks (see R/regtab_generics.R): ancillary rows are already in
  # mrows, before the intercept the union puts last; trailing rows form
  # their own union, appended after it.
  mrows <- lapply(mrows, .rt_ns_keys)
  {
    mrows <- lapply(seq_len(M), function(m) .rt_parent_labels(.rt_count_rows(mrows[[m]], fits[[m]], (o$fitcounts %||% rep(list(NULL), M))[[m]]), fits1[[m]]))
    trows <- lapply(seq_len(M), function(m) .rt_parent_labels(.rt_count_rows(.rt_ns_keys(trows[[m]]), fits[[m]], (o$fitcounts %||% rep(list(NULL), M))[[m]]), fits1[[m]]))
  }
  u <- .rt_union_append(tt_regtab_union(mrows), tt_regtab_union(trows))
  u <- .rt_order_joined_levels(u, rejoined)
  # nointercept: intercepts, cutpoints and ancillary rows go together,
  # identified by their structural role (H5, decision H-D2), never by their
  # label, as Stata tabtools 2.1.12 identifies them from the coefficient's
  # equation and name (2.1.11 matched the displayed label and dropped a
  # covariate named `p`, `alpha`, `cut1`, ...; take_action item 10, fixed).
  # Random-effects rows are
  # exempt (Stata matches them under their raw `var(...)` names, which never
  # match), except tobit's "Ancillary: var(e.y)" in the multi-equation
  # layout, whose role is "ancillary".
  if (scale$nointercept && nrow(u$rows)) {
    u <- .rt_subset(u, !(u$rows$role %in% .rt_noint_roles))
  }
  # cutlabels relabel the kept cut# rows (ancillary rows of ordered models).
  coef_rows <- !u$rows$trailing
  # A cutpoint is a row with the cutpoint role, or, in the multi-equation
  # layout (where every `/` row is ancillary), a `cut#` of the `/` equation.
  raw <- .rt_match_key(u$rows$key)
  is_cut <- u$rows$role %in% "cutpoint" | (startsWith(u$rows$key, "/::") & grepl("^cut[0-9]+$", raw))
  u$rows$label[coef_rows] <- .rt_apply_cutlabels(u$rows$label[coef_rows], o$cutlabels,
                                                 is_cut[coef_rows], raw[coef_rows])
  u <- .rt_select(u, keep = o$keep, drop = o$drop, labelmatch = o$labelmatch)
  if (!nrow(u$rows)) cli::cli_abort("No coefficient rows to display.", call = NULL)
  if (isTRUE(o$reftop)) u <- .rt_reftop(u)
  nr <- nrow(u$rows)
  raw_u <- u
  dim <- if (o$dimnonsig) .rt_dimnonsig(raw_u, vapply(infos, function(i) as.numeric(i$null_value), 0)) else rep(FALSE, nr)
  table <- .rt_numeric_table(raw_u)
  masks <- list(stored = list(), provenance = NULL)
  {
    o$fits <- fits
    masks <- .rt_mincount_union(u, o)
    u <- masks$u
  }

  # Per-model est / CI / p text.
  est_text <- ci_text <- p_text <- vector("list", M)
  for (m in seq_len(M)) {
    c <- u$cells[[m]]
    e <- rep("", nr)
    st <- c$status
    is_est <- st %in% c("est", "notest", "constrained")
    e[is_est] <- .rt_est_text(c$estimate[is_est], o$digits, o$numeric_format)
    e[st %in% c("base", "ref")] <- o$refcat
    e[st %in% "omit"] <- o$omitlabel
    e[st %in% c("empty", "masked")] <- o$emptylabel
    e[st %in% "notest"] <- o$notestlabel %||% "Not estimable"
    fixed <- st %in% "constrained"
    if (!is.null(c$mask_reason)) e[st %in% "absent" & c$mask_reason %in% "sample_absent"] <- o$absentlabel %||% "Absent"
    ci <- rep("", nr)
    # regtab.ado:1846-1850: a fixed coefficient keeps its numeric estimate;
    # cnslabel occupies the separate CI cell, rather than a merged label row.
    ci[fixed] <- o$cnslabel %||% "(constrained)"
    inference <- st %in% "est"
    ci[inference] <- .rt_ci_text(c$conf.low[inference], c$conf.high[inference], o$digits, o$sep, o$numeric_format)
    p <- rep("", nr)
    p[inference] <- format_p(c$p.value[inference], o$pdp, o$highpdp)
    est_text[[m]] <- e
    ci_text[[m]] <- ci
    p_text[[m]] <- p
  }
  # Stars on the estimate text (`regtab.ado:2232-2238`).
  if (o$stars) {
    sl <- o$starslevels
    for (m in seq_len(M)) {
      p <- u$cells[[m]]$p.value
      ok <- u$cells[[m]]$status %in% "est" & !is.na(p)
      s <- ifelse(p < sl[3], "***", ifelse(p < sl[2], "**", ifelse(p < sl[1], "*", "")))
      est_text[[m]][ok] <- paste0(est_text[[m]][ok], s[ok])
    }
  }

  publication <- .rt_apply_cellnote(u, list(est = est_text, ci = ci_text, p = p_text), o$cellnote %||% list())
  u <- publication$u
  est_text <- publication$text$est; ci_text <- publication$text$ci; p_text <- publication$text$p
  publication_table <- .rt_numeric_table(u, publication = TRUE)

  # Row metadata for the body so far.
  kind <- u$rows$kind
  type <- ifelse(kind %in% c("cat_header", "int_header"), "cat_header",
                 ifelse(kind %in% c("level", "int_level", "flat"), "level", "var"))
  type[kind %in% "re"] <- "re"
  for (i in which(type == "level")) {
    st <- unique(vapply(u$cells, function(c) c$status[i] %||% NA_character_, "")[
      !is.na(vapply(u$cells, function(c) c$status[i] %||% NA_character_, ""))])
    if (length(st) == 1L && st %in% c("base", "ref", "omit", "empty")) {
      type[i] <- c(base = "ref", ref = "ref", omit = "omitted", empty = "empty")[[st]]
    }
  }
  labels <- u$rows$label
  keys <- .rt_row_keys(u$rows)
  st_all <- lapply(seq_len(M), function(m) {
    if (failed[m]) return(list())
    s <- with_fb(m, .rt_quiet_zero_weight(tt_model_stats(fits[[m]], infos[[m]], vce = vces[[m]],
                                                         cluster = clusters[[m]])))
    # Probability weights (user weights with a robust or cluster vce, a
    # robust weighted coxph, geeglm's glm reading): Stata's
    # pseudo-log-likelihood and the AIC/BIC from it (review P0-4;
    # R/regtab_vce.R). A multiply imputed model has no likelihood.
    if (!is_mi[m] && .rt_pw_reading(fits[[m]], vces[[m]])) s <- .rt_pw_stats(fits[[m]], s, vces[[m]])
    # stats = "vce" (R only): the variance in the vce_note footnote's words.
    if (isTRUE(o$stats$vce)) s$vce_text <- .rt_vce_label(fits[[m]], infos[[m]], vces[[m]], (o$cluster_spec %||% clusters)[[m]])
    s
  })
  if (!is.null(o$stats)) .rt_stat_notes(st_all, o$stats)
  stats_out <- .rt_rich_stats(o, st_all, fits)
  # R-only user statistics rows (stat_fun), after Stata's.
  user_rows <- .rt_stat_fun_rows(o$stat_fun %||% list(), fits)
  statistic_rows <- lapply(c(stats_out$rows, user_rows), function(r) { r$type <- "stat"; r })
  placed <- .rt_place_extra(u, list(est = est_text, ci = ci_text, p = p_text), labels, keys, type, dim,
                            statistic_rows, o$addrow)
  coefficient_type <- type
  labels <- placed$labels; keys <- placed$keys; type <- placed$type; dim <- placed$dim
  est_text <- placed$text$est; ci_text <- placed$text$ci; p_text <- placed$text$p
  nb <- length(labels)

  # p-values for boldp/highlight, read from the p text before compact and
  # nopvalue change the columns (`regtab.ado:2645-2653`).
  pvals <- matrix(vapply(p_text, .rt_p_from_text, numeric(nb)), nb, M)

  # Headers.
  ci_level <- round(o$level * 100, 8)
  ci_head <- o$cilabel %||% paste0(.rt_pct_text(o$level), "% CI")
  model_labels <- o$models
  est_head <- scale$headers

  cols <- list()
  body <- list(labels)
  h1 <- ""
  h2 <- ""
  role <- "label"
  model_ix <- NA_integer_
  for (m in seq_len(M)) {
    if (o$compact) {
      et <- est_text[[m]]
      ct <- ci_text[[m]]
      merged <- ifelse(nzchar(ct), paste0(et, " ", ct), et)
      body <- c(body, list(merged))
      h1 <- c(h1, model_labels[m])
      h2 <- c(h2, trimws(paste(est_head[m], ci_head)))
      role <- c(role, "est_ci")
    } else {
      body <- c(body, list(est_text[[m]], ci_text[[m]]))
      h1 <- c(h1, model_labels[m], "")
      h2 <- c(h2, est_head[m], ci_head)
      role <- c(role, "est", "ci")
    }
    model_ix <- c(model_ix, rep(m, if (o$compact) 1L else 2L))
    if (!o$nopvalue) {
      body <- c(body, list(p_text[[m]]))
      h1 <- c(h1, "")
      h2 <- c(h2, o$plabel %||% "p-value")
      role <- c(role, "pval")
      model_ix <- c(model_ix, m)
    }
  }
  body <- as.data.frame(body, stringsAsFactors = FALSE, col.names = paste0("c", seq_along(body)))
  cols <- data.frame(role = role, model = model_ix, console_width = NA_integer_, stringsAsFactors = FALSE)
  rows <- data.frame(type = type, indent = nchar(labels) - nchar(sub("^ +", "", labels)),
                     dim = dim, stringsAsFactors = FALSE)
  rows[c("key", "var", "level")] <- keys
  rows$block <- seq_len(nb)
  rows$inserted <- placed$inserted
  rows$p <- apply(pvals, 1L, function(p) if (all(is.na(p))) NA_real_ else min(p, na.rm = TRUE))

  # vce_note (milestone 5w): a sentence naming each model's non-default
  # variance, appended to the footnote (every sink), as the stars note is
  # to the xlsx footnote.
  # A user-supplied variance is always named (task 5.18).
  failed_notes <- vapply(which(failed), function(m) paste0("Model ", m, ": ", fits[[m]]$reason), "")
  if (length(failed_notes)) o$footnote <- .tt_append_footnotes(o$footnote, failed_notes)
  if (length(re_notes)) {
    o$footnote <- .tt_append_footnotes(o$footnote, re_notes)
  }
  user_vce <- any(vapply(vces, identical, TRUE, "user"))
  if (isTRUE(o$vce_note) || user_vce) {
    note <- .rt_vce_footnote(fits1, infos, vces, o$cluster_spec %||% clusters, o$models, user_only = !isTRUE(o$vce_note))
    if (nzchar(note)) {
      o$footnote <- .tt_append_footnotes(o$footnote, note)
    }
  }
  # polr/clm links with no Stata ordered command (cloglog, loglog, cauchit,
  # ...): the coefficients are on that link's scale; say so (CAT P1-9).
  ord_links <- unique(unlist(lapply(infos, function(i) {
    if (!is.null(i) && i$class %in% c("polr", "clm") && is.na(i$stata_cmd)) as.character(i$link)[1]
  })))
  ord_links <- ord_links[!is.na(ord_links)]
  if (length(ord_links)) {
    note <- sprintf("Ordinal model coefficients are on the %s link scale.", paste(ord_links, collapse = "/"))
    o$footnote <- .tt_append_footnotes(o$footnote, note)
  }
  # xlsx footnote with the stars note (`regtab.ado:2985-2998`).
  xfoot <- o$footnote %||% ""
  if (o$stars) {
    note <- sprintf("* p<%s, ** p<%s, *** p<%s", o$starstext[1], o$starstext[2], o$starstext[3])
    o$footnote <- .tt_append_footnotes(o$footnote, note)
    xfoot <- o$footnote
  }

  # From the models, not the header (task H11; R/regtab_methods.R).
  methods <- tt_regtab_methods(infos[!failed], vapply(fits1[!failed], .rt_n_predictors, 0), o$level, o$stars,
                               if (o$starslevels_given) o$starslevels else NULL,
                               ci_method = o$ci_method %||% "wald")
  stored <- list(N_rows = nb + 3, N_cols = ncol(body) + 1, N_models = M, ci_level = ci_level,
                 coef_label = scale$coef_label, methods = methods,
                 ci_method = o$ci_method %||% "wald")
  # One entry per model ("" for none) when any model's variance fell back.
  fb <- vapply(fallback, function(x) paste(unique(x), collapse = "; "), "")
  if (any(nzchar(fb))) stored$vce_fallback <- fb
  if (o$stars) stored$stars <- "stars"
  if (!is.null(table)) stored$table <- table
  stored$table_role <- "raw_analytical"
  stored$publication_table <- publication_table
  stored <- c(stored, masks$stored)
  stored <- c(stored, stats_out$stored)

  # Phase 7 metadata: per-row numbers and per-model frame characteristics
  # (`regtab.ado:2285-2299`, `:2961-2976`).
  long <- .rt_long_rows(u, coefficient_type, placed$row_map)
  raw_long <- .rt_long_rows(raw_u, coefficient_type, placed$row_map)
  frame_meta <- list(
    source = "regtab", ci_level = ci_level, n_models = M,
    statistic_ids = paste(c(if (o$compact) "estimate_ci" else c("estimate", "ci"),
                            if (!o$nopvalue) "pvalue"), collapse = " "),
    model_id = vapply(infos, function(i) i$model_id %||% NA_character_, ""),
    outcome_id = vapply(infos, function(i) i$outcome_id %||% NA_character_, ""),
    effect_scale = vapply(infos, function(i) i$effect_scale, ""),
    distribution = vapply(infos, function(i) i$distribution %||% NA_character_, ""),
    metric = vapply(infos, function(i) i$metric %||% NA_character_, ""),
    provenance = vapply(infos, function(i) i$provenance %||% "fitted_model", ""),
    model_label = model_labels
  )
  meta <- list(refcat = o$refcat, omitlabel = o$omitlabel, emptylabel = o$emptylabel,
               labelwidth = o$labelwidth, compact = o$compact, xlsx_footnote = xfoot,
               stars_notes = if (o$stars) note else character(),
               pvals = pvals, sheet = o$sheet, regtab_rows = long, regtab_raw_rows = raw_long,
               regtab_mask_provenance = masks$provenance, regtab_stats_provenance = stats_out$provenance,
               fitcount_identity = o$fitcount_identity,
               fitcount_raw = lapply(o$fitcounts, function(record) {
                 if (is.null(record)) return(NULL)
                 record[intersect(c("schema_version", "evidence", "sample", "counts", "terms", "levels", "states"), names(record))]
               }), frame = frame_meta)
  samples <- o$sample_accounting %||% lapply(seq_len(M), function(m) {
    .tt_sample_model_population(fits[[m]], "fit", model = m)
  })
  samples <- lapply(seq_len(M), function(m) .tt_sample_model_reported(samples[[m]], st_all[[m]]))
  meta$sample_accounting <- .tt_sample_bind(samples, prefixes = paste0("model", seq_len(M)))
  meta$flat <- .tt_flat_metadata(placed$cells, rows, M)
  tt <- tt_table(body, list(list(text = h1), list(text = h2)), rows = rows, cols = cols,
           title = o$title, footnote = o$footnote, style = o$style, stored = stored,
           command = "regtab", meta = meta)
  if (isTRUE(o$transpose)) tt <- .rt_transpose_table(tt, u, publication$text, statistic_rows, o, scale)
  tt
}

# A confidence level (a proportion) as the percentage text of the CI header
# and the methods sentence: 15 significant digits, so 0.999 reads "99.9",
# as Stata tabtools 2.1.12 prints it (`string(level, "%21.15g")`, golden
# R67). 2.1.11 printed the double's noise ("99.90000000000001% CI"), which
# R never copied (pre-release review P2-2).
.rt_pct_text <- function(level) {
  trimws(formatC(round(level * 100, 8), digits = 15, format = "g"))
}
