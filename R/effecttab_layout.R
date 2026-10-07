# effecttab labels, rows and layout (plan tasks 7.8-7.9): the multi-model
# row union, Stata's teffects and margins labels, the cell text, the
# addrow rows, the stored results and the frame metadata. The workbook uses
# regtab's regression rule set (write_xlsx.R) with effecttab's own column
# widths (layout field width_rule = "effecttab", X1/X2); tabtools (since
# 2.1.11) top-aligns every body cell and merges Reference rows per model
# block, as regtab does, so nothing else differs (C1, C2).

# ---------------------------------------------------------------------------
# Stata baseline: tabtools 2.1.12 (Stata-Tools f42ee9cd), the golden
# baseline of E01-E26, W12, W13.

# The interval and p-value headers: "<L>% CI" and "p-value", for a matrix
# (from()) too (effecttab.ado:655-660; 2.1.11 wrote "(95% CI)" and "p"
# there).
.et_stat_headers <- function(ci_txt) c(paste0(ci_txt, "% CI"), "p-value")

# ---------------------------------------------------------------------------
# Row union

# Factor levels across models, joined by level (codex audit F01), as
# regtab joins them (R/regtab_union_check.R): a plain factor's or a
# character variable's level code is its position among that result's
# levels, so a factor relevelled in one model, or a subset lacking a level,
# numbers its levels differently, and joining by key put one model's
# estimate under another level's label. A variable whose positional codes
# disagree across models (a code naming two levels, or a level under two
# codes) is recoded from one map over every model: the levels in order of
# first appearance, model 1's order first. Models that already agree keep
# their keys.
.et_join_levels <- function(mrows) {
  if (length(mrows) < 2L) return(mrows)
  comps <- lapply(mrows, .et_level_parts)
  vars <- unique(unlist(lapply(seq_along(mrows), function(m) {
    cp <- comps[[m]]
    cp$var[mrows[[m]]$positional[cp$row] %in% TRUE]
  })))
  rejoined <- character()
  for (v in vars) {
    has <- which(vapply(seq_along(mrows), function(m) {
      cp <- comps[[m]]
      any(cp$var == v & mrows[[m]]$positional[cp$row] %in% TRUE)
    }, TRUE))
    if (length(has) < 2L) next
    maps <- lapply(comps[has], function(cp) {
      u <- unique(cp[cp$var == v & !is.na(cp$code) & !is.na(cp$text), c("code", "text"), drop = FALSE])
      u[order(suppressWarnings(as.numeric(u$code)), method = "radix"), , drop = FALSE]
    })
    all_pairs <- unique(do.call(rbind, maps))
    if (!anyDuplicated(all_pairs$code) && !anyDuplicated(all_pairs$text)) next
    union <- .rt_merge_levels(lapply(maps, `[[`, "text"))
    rejoined <- c(rejoined, v)
    for (j in seq_along(has)) {
      m <- has[j]
      map <- stats::setNames(as.character(match(maps[[j]]$text, union)), maps[[j]]$code)
      hit <- comps[[m]]$var == v & comps[[m]]$code %in% names(map)
      comps[[m]]$code[hit] <- map[comps[[m]]$code[hit]]
      mrows[[m]] <- .et_rekey_parts(mrows[[m]], comps[[m]], unique(comps[[m]]$row[hit]))
    }
  }
  attr(mrows, "rejoined") <- rejoined
  mrows
}

# The level codes a row's key is built from, one per part: a level, a
# base level (teffects contrasts, pairwise contrasts), or each variable of
# a combination (by = c("sex", "highbp") keys "1.sex#2.highbp").
.et_level_parts <- function(r) {
  out <- list()
  for (i in seq_len(nrow(r))) {
    k <- r$kind[i]
    v <- r$variable[i]
    if (!k %in% c("level", "ref", "pomean", "contrast") || is.na(v)) next
    if (!grepl("#", v, fixed = TRUE)) {
      # A pairwise contrast row ("six vs four") names its level first.
      lt <- r$level_text[i]
      vs_base <- paste0(" vs ", r$base_text[i])
      if (k == "level" && !is.na(lt) && !is.na(r$base_text[i]) && endsWith(lt, vs_base)) {
        lt <- substr(lt, 1L, nchar(lt) - nchar(vs_base))
      }
      out[[length(out) + 1L]] <- data.frame(row = i, slot = "level", var = v, code = r$level[i],
                                            text = lt, stringsAsFactors = FALSE)
      if (!is.na(r$base[i])) {
        out[[length(out) + 1L]] <- data.frame(row = i, slot = "base", var = v, code = r$base[i],
                                              text = r$base_text[i], stringsAsFactors = FALSE)
      }
      next
    }
    vs <- strsplit(v, "#", fixed = TRUE)[[1]]
    parts <- strsplit(r$key[i], "#", fixed = TRUE)[[1]]
    texts <- if (is.na(r$level_text[i])) character() else strsplit(r$level_text[i], "#", fixed = TRUE)[[1]]
    if (!identical(r$key[i], r$level[i]) || length(parts) != length(vs) || length(texts) != length(vs)) next
    code <- sub("\\..*$", "", parts)
    if (!all(parts == paste0(code, ".", vs))) next
    out[[length(out) + 1L]] <- data.frame(row = i, slot = as.character(seq_along(vs)), var = vs, code = code,
                                          text = texts, stringsAsFactors = FALSE)
  }
  if (!length(out)) {
    return(data.frame(row = integer(), slot = character(), var = character(), code = character(),
                      text = character(), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Rebuild the keys (and level/base codes) of rows `at` from their parts.
.et_rekey_parts <- function(r, cp, at) {
  for (i in at) {
    p <- cp[cp$row == i, , drop = FALSE]
    v <- r$variable[i]
    if (grepl("#", v, fixed = TRUE)) {
      k <- paste(paste0(p$code, ".", p$var), collapse = "#")
      r$key[i] <- k
      r$level[i] <- k
      next
    }
    old_l <- r$level[i]
    old_b <- r$base[i]
    new_l <- p$code[p$slot == "level"][1]
    new_b <- if (any(p$slot == "base")) p$code[p$slot == "base"][1] else old_b
    k <- r$key[i]
    if (identical(k, paste0(old_l, ".", v))) {
      k <- paste0(new_l, ".", v)
    } else if (identical(k, paste0("r", old_l, "vs", old_b, ".", v))) {
      k <- paste0("r", new_l, "vs", new_b, ".", v)
    } else if (identical(k, paste0(old_l, "vs", old_b, ".", v))) {
      k <- paste0(new_l, "vs", new_b, ".", v)
    }
    r$key[i] <- k
    r$level[i] <- new_l
    r$base[i] <- new_b
  }
  r
}

# Refuse levels that still cannot be matched across models (codex audit
# F01), as regtab does (.rt_check_level_union()): after .et_join_levels(),
# a positional code can meet a value code of another model (a plain factor
# beside a tt_as_factor() or haven-labelled one), so that one key names two
# levels ("1.am" Auto in one model, Man in the other) or one level has two
# keys (it would be shown twice). Variables coded by value in every model
# may carry different value labels in different data and are not checked.
.et_check_level_union <- function(mrows) {
  parts <- do.call(rbind, lapply(seq_along(mrows), function(m) {
    cp <- .et_level_parts(mrows[[m]])
    if (!nrow(cp)) return(NULL)
    cp$model <- m
    cp$pos <- mrows[[m]]$positional[cp$row] %in% TRUE
    cp
  }))
  if (is.null(parts)) return(invisible(TRUE))
  parts <- parts[!is.na(parts$code) & !is.na(parts$text), , drop = FALSE]
  bad <- character()
  for (v in unique(parts$var[parts$pos])) {
    x <- unique(parts[parts$var == v, c("code", "text", "model"), drop = FALSE])
    for (k in unique(x$code)) {
      y <- x[x$code == k, , drop = FALSE]
      if (length(unique(y$text)) > 1L) {
        bad <- c(bad, paste0(k, ".", v, ": ", paste0("\"", y$text, "\" (model ", y$model, ")", collapse = " vs ")))
      }
    }
    for (t in unique(x$text)) {
      y <- x[x$text == t, , drop = FALSE]
      if (length(unique(y$code)) > 1L) {
        bad <- c(bad, paste0("\"", t, "\" of ", v, ": ", paste0(y$code, ".", v, " (model ", y$model, ")", collapse = " vs ")))
      }
    }
  }
  if (length(bad)) {
    cli::cli_abort(c(
      "Factor levels cannot be matched across models: a key names different levels, or a level has different keys.",
      stats::setNames(utils::head(bad, 6L), rep("x", min(length(bad), 6L))),
      "i" = "Give the variable the same levels, coded the same way, in every model (for example {.fn tt_as_factor} in all or none)."
    ), class = "tabtools_error_effect_row_mismatch", call = NULL)
  }
  invisible(TRUE)
}

# Rows of every model, each key once in order of first appearance; a key
# new to a later model joins its variable's block (a new level stays under
# its factor), as the collection lists colname levels. Rows no model fills
# (no number and no Reference/Omitted/Empty status) are dropped, as the
# collection renderer drops them, except from a matrix, which Stata copies
# row for row.
.et_union <- function(mrows, from_matrix) {
  if (!from_matrix && length(mrows) > 1L) {
    mrows <- .et_join_levels(mrows)
    .et_check_level_union(mrows)
  }
  rejoined <- attr(mrows, "rejoined") %||% character()
  master <- NULL
  for (m in seq_along(mrows)) {
    r <- mrows[[m]]
    for (i in seq_len(nrow(r))) {
      if (!is.null(master) && r$key[i] %in% master$key) next
      new <- r[i, , drop = FALSE]
      if (is.null(master)) {
        master <- new
        next
      }
      blk <- if (!is.na(new$parent)) which(master$parent %in% new$parent) else integer()
      at <- if (length(blk)) max(blk) else nrow(master)
      master <- rbind(master[seq_len(at), , drop = FALSE], new, master[-seq_len(at), , drop = FALSE])
    }
  }
  if (is.null(master)) master <- .et_empty_rows()
  # A re-coded variable's levels are listed in their new code order (a
  # level new to a later model sits where model order puts it, not last).
  for (v in rejoined) {
    ix <- which(master$kind %in% c("level", "ref") & master$parent %in% v)
    if (length(ix) < 2L || any(diff(ix) != 1L)) next
    code <- suppressWarnings(as.numeric(master$level[ix]))
    if (anyNA(code) || !all(master$key[ix] == paste0(master$level[ix], ".", v))) next
    master[ix, ] <- master[ix[order(code)], , drop = FALSE]
  }
  rownames(master) <- NULL
  cells <- lapply(mrows, function(r) {
    hit <- match(master$key, r$key)
    data.frame(estimate = r$estimate[hit], conf.low = r$conf.low[hit], conf.high = r$conf.high[hit],
               p.value = r$p.value[hit], status = r$status[hit], stringsAsFactors = FALSE)
  })
  if (!from_matrix && nrow(master)) {
    filled <- Reduce(`|`, lapply(cells, function(c) {
      c$status %in% c("base", "omit", "empty") |
        (c$status %in% "est" & (is.finite(c$estimate) | is.finite(c$conf.low) | is.finite(c$conf.high) |
                                  is.finite(c$p.value)))
    }), rep(FALSE, nrow(master)))
    master <- master[filled, , drop = FALSE]
    cells <- lapply(cells, function(c) c[filled, , drop = FALSE])
  }
  # A factor's levels are headed by the variable (the collection renderer's
  # factor parents): a heading row wherever the parent changes.
  if (nrow(master) && any(!is.na(master$parent))) {
    out <- list()
    cout <- lapply(cells, function(c) list())
    last <- NA_character_
    for (i in seq_len(nrow(master))) {
      p <- master$parent[i]
      if (!is.na(p) && !identical(p, last)) {
        out[[length(out) + 1L]] <- .et_rows(1L, key = p, kind = "cat_header", variable = master$variable[i],
                                            var_label = master$var_label[i], status = NA_character_)
        for (m in seq_along(cells)) {
          cout[[m]][[length(cout[[m]]) + 1L]] <- data.frame(estimate = NA_real_, conf.low = NA_real_,
                                                            conf.high = NA_real_, p.value = NA_real_,
                                                            status = NA_character_, stringsAsFactors = FALSE)
        }
      }
      last <- p
      out[[length(out) + 1L]] <- master[i, , drop = FALSE]
      for (m in seq_along(cells)) cout[[m]][[length(cout[[m]]) + 1L]] <- cells[[m]][i, , drop = FALSE]
    }
    master <- do.call(rbind, out)
    cells <- lapply(cout, function(l) do.call(rbind, l))
  }
  rownames(master) <- NULL
  cells <- lapply(cells, function(c) {
    rownames(c) <- NULL
    c
  })
  list(rows = master, cells = cells)
}

# ---------------------------------------------------------------------------
# Labels (effecttab.ado:495-533, :694-743, :896-906)

# tlabels(): a named character vector (codes as names) or Stata's string
# `0 "SSRI" 1 "SNRI"`. Returns a named character vector, NULL for none.
.et_parse_tlabels <- function(tlabels) {
  if (is.null(tlabels)) return(NULL)
  if (is.list(tlabels)) tlabels <- unlist(tlabels)
  if (is.numeric(tlabels) || is.factor(tlabels)) tlabels <- stats::setNames(as.character(tlabels), names(tlabels))
  if (!is.character(tlabels) || anyNA(tlabels)) {
    cli::cli_abort("{.arg tlabels} must be a named character vector, e.g. {.code c(\"0\" = \"SSRI\", \"1\" = \"SNRI\")}.",
                   call = NULL)
  }
  if (is.null(names(tlabels)) && length(tlabels) == 1L) {
    s <- tlabels
    toks <- regmatches(s, gregexpr('"[^"]*"|[^[:space:]]+', s))[[1]]
    toks <- sub('^"(.*)"$', "\\1", toks)
    if (!length(toks) || length(toks) %% 2L) {
      cli::cli_abort("{.arg tlabels} must pair each level with a label: {.code '0 \"SSRI\" 1 \"SNRI\"'}.", call = NULL)
    }
    tlabels <- stats::setNames(toks[c(FALSE, TRUE)], toks[c(TRUE, FALSE)])
  }
  nm <- names(tlabels)
  if (is.null(nm) || anyNA(nm) || any(!nzchar(nm))) {
    cli::cli_abort("Every element of {.arg tlabels} needs the level it labels as its name.", call = NULL)
  }
  tlabels
}

# Stata's teffects labels under clean/tlabels: `<Tvarlabel> (<lev> vs
# <base>)` and `<Tvarlabel> = <lev> (PO Mean)`, or `<lablev> vs <labbase>`
# and `<lablev> (PO Mean)` when a level has a label. The variable label has
# its first letter capitalised (upper(), ASCII only) and "_" shown as a
# space. tlabels() replaces the value labels altogether: a level it does not
# name keeps its code (effecttab.ado:509-523). A plain factor's positional
# code never shadows a name that is a level's own text (audit 2026-09-29
# B03): its level's text is tried first, and its position only when that is
# no level's text (`texts`, the level texts of the variable's rows).
.et_te_label <- function(r, tlabels, texts = character()) {
  tv <- r$var_label
  if (is.na(tv)) tv <- r$variable
  first <- substr(tv, 1L, 1L)
  if (grepl("^[a-z]$", first)) tv <- paste0(toupper(first), substring(tv, 2L))
  tv <- gsub("_", " ", tv, fixed = TRUE)
  positional <- isTRUE(r$positional)
  part <- function(code, text) {
    if (is.na(code)) return(code)
    if (!is.null(tlabels)) {
      # Stata walks every pair and keeps the last non-empty label for the
      # level (an empty one does not override, :511-519).
      named <- function(k) tlabels[names(tlabels) == k & nzchar(tlabels)]
      # R convenience: a plain factor's level may be named by its level.
      hit <- if (positional && !is.na(text)) {
        h <- named(text)
        if (!length(h) && !code %in% texts) h <- named(code)
        h
      } else {
        h <- named(code)
        if (!length(h) && !is.na(text)) h <- named(text)
        h
      }
      if (length(hit)) return(unname(hit[length(hit)]))
      # A level tlabels does not name keeps its code; a plain factor's
      # code is only its position, so it keeps its level instead.
      return(if (positional && !is.na(text)) text else code)
    }
    if (is.na(text)) code else text
  }
  lev <- part(r$level, r$level_text)
  if (identical(r$kind, "contrast")) {
    base <- part(r$base, r$base_text)
    if (!identical(lev, r$level) || !identical(base, r$base)) return(paste(lev, "vs", base))
    return(paste0(tv, " (", r$level, " vs ", r$base, ")"))
  }
  if (!identical(lev, r$level)) return(paste0(lev, " (PO Mean)"))
  # (A plain factor's level shown instead of its position is a label too.)
  paste0(tv, " = ", r$level, " (PO Mean)")
}

.et_labels <- function(rows, clean, tlabels) {
  vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, , drop = FALSE]
    if (!is.na(r$label)) return(r$label)
    texts <- function() {
      same <- !is.na(rows$variable) & rows$variable %in% r$variable
      t <- c(rows$level_text[same], rows$base_text[same])
      unique(t[!is.na(t)])
    }
    switch(r$kind,
      contrast = , pomean = if (clean) .et_te_label(r, tlabels, texts()) else r$key,
      cat_header = .et_or(r$var_label, r$key),
      level = , ref = if (!is.na(r$level_text)) paste0("  ", r$level_text) else r$key,
      .et_or(r$var_label, r$key))
  }, "")
}

# ---------------------------------------------------------------------------
# Cell text (effecttab.ado:1007-1139)

# Collect path: the estimate pre-rounded with round(x, 10^-digits), then
# %21.<digits>f; interval bounds formatted without the pre-round; p-values
# by the pdp/highpdp rules. A matrix (from()) goes through its own text
# first: the estimate is string(x, "%21.<d>f") before it is rounded and
# formatted again, the bounds are formatted, read back and formatted again
# (so -0.004 shows 0.0 at one decimal, where a collected bound keeps
# -0.0), and the p-value is read back from string(p, "%21.0g")
# (:656-676, :1059-1106).
.et_fmt <- function(d) paste0("%21.", d, "f")

# Every number effecttab shows goes through .et_num() (cells, the matrix
# text round trip) or .et_level_text() (the level in the headers and the
# methods sentence), plus the shared format_p() and, for level codes,
# stata_macro_text(): all of them the platform-independent formatter of
# R/format.R (src/stata_fmt.c).
.et_num <- function(x, fmt) stata_fmt(x, fmt)

.et_est_text <- function(x, d, matrix) {
  out <- rep("", length(x))
  ok <- is.finite(x)
  v <- x[ok]
  if (matrix) v <- as.numeric(.et_num(v, .et_fmt(d))) + 0
  out[ok] <- .et_num(stata_round(v, 10^-d), .et_fmt(d))
  out
}

.et_bound <- function(x, d, matrix) {
  if (matrix) x <- as.numeric(.et_num(x, .et_fmt(d))) + 0
  .et_num(x, .et_fmt(d))
}

.et_ci_text <- function(lo, hi, d, sep, matrix) {
  out <- rep("", length(lo))
  ok <- is.finite(lo) & is.finite(hi)
  out[ok] <- paste0("(", .et_bound(lo[ok], d, matrix), sep, .et_bound(hi[ok], d, matrix), ")")
  out
}

.et_p_value <- function(p, matrix) {
  if (!matrix) return(p)
  out <- rep(NA_real_, length(p))
  ok <- is.finite(p)
  out[ok] <- suppressWarnings(as.numeric(.et_num(p[ok], "%21.0g")))
  out
}

# effecttab's boldp/highlight read the p text back (:1474-1492): "<0.001"
# counts as 0, and anything real() cannot parse (">0.99", blanks) is
# missing, so a ">0.99" cell is never bold (regtab strips the ">").
.et_p_from_text <- function(s) {
  s <- trimws(s)
  out <- suppressWarnings(as.numeric(s))
  out[startsWith(s, "<")] <- 0
  out
}

# r(table) row names (effecttab.ado:996-1004): "." and " " become "_", ","
# is removed, the name is cut to 32 bytes, and `matrix rownames` stores it:
# a name "a#b" becomes the interaction "c.a#c.b" (golden E23); a name it
# rejects makes the whole command fail, leaving Stata's r1, r2, ... (the
# rejection rules of .stata_rowname_survives()).
.et_rowname <- function(label, k) {
  s <- gsub(".", "_", label, fixed = TRUE)
  s <- gsub(" ", "_", s, fixed = TRUE)
  s <- gsub(",", "", s, fixed = TRUE)
  s <- vapply(s, function(x) {
    b <- charToRaw(enc2utf8(x))
    if (length(b) > 32L) {
      x <- rawToChar(b[seq_len(32L)])
      Encoding(x) <- "UTF-8"
    }
    x
  }, "", USE.NAMES = FALSE)
  s[!nzchar(s)] <- paste0("row", k[!nzchar(s)])
  # Names are kept unique before `matrix rownames` (effecttab.ado:1011-1019):
  # a repeat takes _2, _3, ... within 32 bytes, retried until it is new
  # (c("A", "A", "A_2") gives A, A_2, A_2_2).
  s <- .et_dedupe_names(s)
  inter <- grepl(".#.", s, perl = TRUE)
  s[inter] <- vapply(strsplit(s[inter], "#", fixed = TRUE), function(p) paste0("c.", p, collapse = "#"), "")
  s <- sub("^cov\\((.+)\\)$", "var(\\1)", s)
  s <- sub("^/", "", s)
  rejected <- grepl("\\[[^]]*\\].*\\[", s, perl = TRUE) | grepl("\\([^)]*\\[[^)]*$", s, perl = TRUE) | !nzchar(s)
  if (any(rejected)) s <- paste0("r", seq_along(s))
  s
}

.et_substr_bytes <- function(x, n) {
  b <- charToRaw(enc2utf8(x))
  if (length(b) <= n) return(x)
  out <- rawToChar(b[seq_len(n)])
  Encoding(out) <- "UTF-8"
  out
}

.et_dedupe_names <- function(s) {
  used <- character()
  for (i in seq_along(s)) {
    base <- s[i]
    nm <- base
    k <- 1L
    while (nm %in% used) {
      k <- k + 1L
      sfx <- paste0("_", k)
      nm <- paste0(.et_substr_bytes(base, 32L - nchar(sfx)), sfx)
    }
    used <- c(used, nm)
    s[i] <- nm
  }
  s
}

# The level as the headers and the methods sentence show it:
# string(level, "%21.15g") (effecttab.ado:260-263), at most 15 significant
# digits, so 99.9 reads "99.9", not the double's 99.90000000000001.
.et_level_text <- function(x) .tt_level_text(x)

# ---------------------------------------------------------------------------
# Methods sentence (effecttab.ado:1267-1299), without Stata's closing
# "Analysis performed in Stata ..." sentence.

.et_estimators <- c(ipw = "inverse probability weighting", ra = "regression adjustment",
                    aipw = "augmented inverse probability weighting",
                    ipwra = "inverse probability weighted regression adjustment",
                    psmatch = "propensity score matching", nnmatch = "nearest-neighbor matching")

.et_methods <- function(type, M, from_matrix, estimator, source, ci_txt, estimand = NULL, pomeans_only = FALSE) {
  if (from_matrix) {
    return(paste0("Effect estimates were formatted from the supplied matrix with ", ci_txt,
                  "% confidence intervals and p-values."))
  }
  if (M > 1L) {
    return(paste0("Effect estimates from multiple collected models were formatted with ", ci_txt,
                  "% confidence intervals."))
  }
  if (type == "teffects") {
    # What the table holds: potential-outcome means alone (pomeans), effects
    # on the treated (atet), or average treatment effects. Stata says
    # "Average treatment effects" for all three (review F19).
    what <- if (pomeans_only) "Potential-outcome means" else
      switch(estimand %||% "", ATET = "Average treatment effects on the treated",
             ATC = "Average treatment effects on the controls", "Average treatment effects")
    est <- tolower(estimator %||% "")
    if (est %in% names(.et_estimators)) {
      return(paste0(what, " estimated using ", .et_estimators[[est]], " with ", ci_txt, "% confidence intervals."))
    }
    return(paste0(what, " were formatted with ", ci_txt, "% confidence intervals."))
  }
  if (identical(source, "marginaleffects")) {
    return(paste0("Marginal effects estimated using the marginaleffects package with ", ci_txt,
                  "% confidence intervals."))
  }
  paste0("Marginal effects estimated using the margins command with ", ci_txt, "% confidence intervals.")
}

# ---------------------------------------------------------------------------
# Assembly

#' Build the effecttab tt_table from effect rows
#' @param mrows List of per-model effect rows (tt_effect_rows(), pieces
#'   stacked).
#' @param o Options.
#' @keywords internal
#' @noRd
tt_effecttab_build <- function(mrows, o) {
  M <- length(mrows)
  from_matrix <- o$from_matrix
  u <- .et_union(mrows, from_matrix)
  if (!nrow(u$rows)) cli::cli_abort("No effect rows to display.", call = NULL)
  labels <- .et_labels(u$rows, o$clean, o$tlabels)
  nr <- nrow(u$rows)
  d <- o$digits
  refs <- c(base = o$refcat, omit = o$omitlabel, empty = o$emptylabel)
  est_text <- ci_text <- p_text <- vector("list", M)
  for (m in seq_len(M)) {
    c <- u$cells[[m]]
    is_est <- c$status %in% "est"
    e <- rep("", nr)
    e[is_est] <- if (is.null(o$numeric_format$cformat)) {
      .et_est_text(c$estimate[is_est], d, from_matrix)
    } else .rt_est_text(c$estimate[is_est], d, o$numeric_format)
    con <- c$status %in% names(refs)
    e[con] <- unname(refs[c$status[con]])
    ci <- rep("", nr)
    ci[is_est] <- if (is.null(o$numeric_format$cformat)) {
      .et_ci_text(c$conf.low[is_est], c$conf.high[is_est], d, o$sep, from_matrix)
    } else .rt_ci_text(c$conf.low[is_est], c$conf.high[is_est], d, o$sep, o$numeric_format)
    p <- rep("", nr)
    pv <- .et_p_value(c$p.value, from_matrix)
    p[is_est] <- format_p(pv[is_est], o$pdp, o$highpdp)
    est_text[[m]] <- e
    ci_text[[m]] <- ci
    p_text[[m]] <- p
    u$cells[[m]]$p.shown <- pv
  }

  # r(table): rows with an estimate or a p-value in some model, before the
  # addrow rows (effecttab.ado:853-1005); estimate and p per model.
  has <- Reduce(`|`, lapply(u$cells, function(c) {
    (c$status %in% "est" & is.finite(c$estimate)) | (c$status %in% "est" & is.finite(c$p.shown))
  }), rep(FALSE, nr))
  table <- NULL
  if (any(has)) {
    idx <- which(has)
    table <- matrix(NA_real_, length(idx), 2L * M)
    for (m in seq_len(M)) {
      c <- u$cells[[m]]
      ok <- c$status[idx] %in% "est"
      table[, 2L * m - 1L] <- ifelse(ok & is.finite(c$estimate[idx]), c$estimate[idx], NA_real_)
      table[, 2L * m] <- ifelse(ok & is.finite(c$p.shown[idx]), c$p.shown[idx], NA_real_)
    }
    dimnames(table) <- list(.et_rowname(labels[idx], seq_along(idx)), paste0("c", seq_len(2L * M)))
  }

  # Row metadata.
  kind <- u$rows$kind
  type <- ifelse(kind == "cat_header", "cat_header", ifelse(kind %in% c("level", "ref"), "level", "var"))
  for (i in which(type == "level")) {
    st <- unique(stats::na.omit(vapply(u$cells, function(c) c$status[i], "")))
    if (length(st) == 1L && st %in% names(refs)) type[i] <- c(base = "ref", omit = "omitted", empty = "empty")[[st]]
  }
  addrow <- rep(FALSE, nr)
  for (r in o$addrow) {
    labels <- c(labels, r$label)
    type <- c(type, "var")
    addrow <- c(addrow, TRUE)
    vals <- c(r$values, rep("", M))[seq_len(M)]
    for (m in seq_len(M)) {
      est_text[[m]] <- c(est_text[[m]], vals[m])
      ci_text[[m]] <- c(ci_text[[m]], "")
      p_text[[m]] <- c(p_text[[m]], "")
    }
  }
  nb <- length(labels)
  pvals <- matrix(vapply(p_text, .et_p_from_text, numeric(nb)), nb, M)

  # Headers: model labels over each block (blank by default, EO10), then
  # the effect, "<L>% CI" and "p-value", a matrix too (effecttab.ado:553,
  # :655-660; 2.1.11's matrix "(<L>% CI)"/"p", EO9, is gone in 2.1.12).
  ci_txt <- .et_level_text(o$ci_level)
  heads <- .et_stat_headers(ci_txt)
  ci_head <- heads[1]
  p_head <- heads[2]
  body <- list(labels)
  h1 <- ""
  h2 <- ""
  for (m in seq_len(M)) {
    body <- c(body, list(est_text[[m]], ci_text[[m]], p_text[[m]]))
    h1 <- c(h1, o$models[m], "", "")
    h2 <- c(h2, o$effect, ci_head, p_head)
  }
  body <- as.data.frame(body, stringsAsFactors = FALSE, col.names = paste0("c", seq_along(body)))
  cols <- data.frame(role = c("label", rep(c("est", "ci", "pval"), M)),
                     model = c(NA_integer_, rep(seq_len(M), each = 3L)),
                     console_width = NA_integer_, stringsAsFactors = FALSE)
  rows <- data.frame(type = type, indent = nchar(labels) - nchar(sub("^ +", "", labels)),
                     block = seq_len(nb), addrow = addrow,
                     key = c(u$rows$key, vapply(o$addrow, function(r) paste0("addrow:", r$label), "")),
                     var = c(u$rows$variable, rep(NA_character_, nb - nr)),
                     level = c(u$rows$level, rep(NA_character_, nb - nr)), stringsAsFactors = FALSE)
  rows$p <- apply(pvals, 1L, function(p) if (all(is.na(p))) NA_real_ else min(p, na.rm = TRUE))

  methods <- .et_methods(o$type, M, from_matrix, o$estimator, o$source, ci_txt, o$estimand,
                         pomeans_only = o$type == "teffects" && all(u$rows$kind == "pomean"))
  stored <- list(N_rows = nb + 3, N_cols = 2 + 3 * M, ci_level = o$ci_level, effect_label = o$effect,
                 type = o$type, methods = methods)
  if (!is.null(table)) stored$table <- table

  # The frame characteristics a composite reads (effecttab.ado:1436-1453)
  # and the numbers behind every row (the shape regtab keeps, so
  # as_forest_data() reads it).
  long <- do.call(rbind, lapply(seq_len(M), function(m) {
    c <- u$cells[[m]]
    data.frame(row = seq_len(nr), key = u$rows$key, label = labels[seq_len(nr)], type = type[seq_len(nr)],
               model = m, status = c$status, estimate = c$estimate, conf.low = c$conf.low,
               conf.high = c$conf.high, p.value = c$p.shown, stringsAsFactors = FALSE)
  }))
  frame <- list(source = "effecttab", ci_level = o$ci_level, n_models = M,
                statistic_ids = "estimate ci pvalue", model_id = o$model_id,
                outcome_id = rep("", M), effect_scale = rep(o$effect, M), model_label = o$models,
                effect_additive = o$additive %||% rep(NA, M),
                effect_log_scale = o$log_scale %||% rep("", M))
  effect_rows <- u$rows
  effect_rows$label <- labels[seq_len(nr)]
  meta <- list(refcat = o$refcat, omitlabel = o$omitlabel, emptylabel = o$emptylabel,
               labelwidth = o$labelwidth, compact = FALSE, xlsx_footnote = o$footnote, pvals = pvals,
               sheet = o$sheet, effect_label = o$effect, regtab_rows = long, effect_rows = effect_rows,
               frame = frame,
               sample_accounting = o$sample_accounting,
               forest_model_label = ifelse(nzchar(o$models), o$models, paste("Model", seq_len(M))))
  meta$flat <- .tt_flat_metadata(u$cells, rows, M)
  tt_table(body, list(list(text = h1), list(text = h2)), rows = rows, cols = cols,
           title = o$title, footnote = o$footnote, style = o$style, stored = stored,
           command = "effecttab", meta = meta)
}
