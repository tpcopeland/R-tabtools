# Factor levels across models, joined by level (Milestone H task H1; review
# of group t2a, P0-3 and P1-1).
#
# Rows of several models are joined by key. A factor level's key carries
# its Stata value code when the factor has one (a "labels" attribute, an
# integer level, a logical's 0/1), else its position among the fit's
# levels (`.rt_codes()`), and a character predictor its position among the
# fit's sorted values; a data frame's levels are keyed the same way
# (R/regtab_escape.R). Positions are not codes: a factor relevelled in one
# fit, a subset that lacks a level (lm() drops unused levels) or a
# character predictor missing a value in one fit numbers its levels
# differently, and joining by position printed one model's estimate under
# another level's label.
#
# Stata joins by value code, and renders those tables correctly (probes in
# qa/stata/make_regtab_union.do: an ib3. base, a subset without level 2, an
# encoded string missing a value): each model's reference sits on its own
# level's row, and a level a model lacks is empty there. R's analogue of the
# value code is the level itself, so positional codes are replaced by codes
# from one map per variable, built over every model: the levels in order of
# first appearance (model 1's order first), or, for a character predictor
# (Stata's `encode`), the sorted values. When every model already numbers a
# variable's levels the same way nothing changes, so single-model tables and
# tables of models on the same levels keep their keys (the goldens).

# Level codes of one model's positional variables: a list, per variable,
# of list(code = old codes, label = levels, char = is character).
.rt_level_maps <- function(fit, rows) {
  out <- list()
  if (is.data.frame(fit)) {
    # Multi-equation frames (multinomial, two-part) key a level
    # "<eq>::<code>.<var>" and label it "<Eq>: <level>": the same level map
    # in every equation (muse P2-11: they were skipped, so two frames with
    # reordered levels joined by position, one model's Reference under
    # another level's label).
    body <- sub("^.*::", "", rows$key)
    multi <- grepl("::", rows$key, fixed = TRUE)
    for (v in attr(rows, "positional") %||% character()) {
      code <- sub("^(-?[0-9]+)\\..*$", "\\1", body)
      sel <- rows$kind %in% "level" & grepl("^-?[0-9]+\\.", body) & substring(body, nchar(code) + 2L) == v &
        (multi | rows$block %in% v)
      if (!any(sel)) next
      lab <- trimws(ifelse(multi[sel], sub("^[^:]*:", "", rows$label[sel]), rows$label[sel]))
      pairs <- unique(data.frame(code = code[sel], label = lab, stringsAsFactors = FALSE))
      if (anyDuplicated(pairs$code) || anyDuplicated(pairs$label)) next
      out[[v]] <- list(code = pairs$code, label = pairs$label, char = FALSE)
    }
    return(out)
  }
  mf <- tryCatch(.rt_frame(fit)$mf, error = function(e) NULL)
  if (!is.data.frame(mf)) return(out)
  for (v in names(mf)) {
    x <- mf[[v]]
    if (!(is.factor(x) || is.character(x))) next
    codes <- .rt_codes(x)
    lev <- names(codes)
    vl <- attr(x, "labels", exact = TRUE)
    coded <- (!is.null(vl) && !is.null(names(vl)) && is.numeric(vl) & lev %in% names(vl)) |
      grepl("^-?[0-9]+$", lev)
    # Only variables whose every level is positional; a mix of value codes
    # and positions is left to the check below.
    if (any(coded) || !length(lev)) next
    out[[v]] <- list(code = unname(codes), label = lev, char = is.character(x))
  }
  out
}

# Replace the level code of variable `v` in a key ("2.treat",
# "1.treat#c.age", "mid::2.treat") through `map` (old code -> new code).
.rt_rekey <- function(keys, v, map) {
  vapply(keys, function(k) {
    if (is.na(k) || !nzchar(k)) return(k)
    pre <- if (grepl("::", k, fixed = TRUE)) sub("^(.*::).*$", "\\1", k) else ""
    body <- substring(k, nchar(pre) + 1L)
    parts <- strsplit(body, "#", fixed = TRUE)[[1]]
    for (j in seq_along(parts)) {
      m <- regmatches(parts[j], regexec("^(-?[0-9]+)\\.(.*)$", parts[j]))[[1]]
      if (length(m) && identical(m[3], v) && m[2] %in% names(map)) parts[j] <- paste0(map[[m[2]]], ".", v)
    }
    paste0(pre, paste(parts, collapse = "#"))
  }, "", USE.NAMES = FALSE)
}

#' Join factor levels across models by level, not by position
#'
#' @param mrows List of `tt_regtab_rows()` results.
#' @param fits The fits (for their model frames).
#' @return `mrows` with positional level codes replaced where the models
#'   number a variable's levels differently.
#' @keywords internal
#' @noRd
.rt_join_levels <- function(mrows, fits) {
  if (length(mrows) < 2L) return(mrows)
  maps <- lapply(seq_along(mrows), function(m) .rt_level_maps(fits[[m]], mrows[[m]]))
  vars <- unique(unlist(lapply(maps, names)))
  rejoined <- character()
  for (v in vars) {
    has <- which(vapply(maps, function(mp) !is.null(mp[[v]]), TRUE))
    if (length(has) < 2L) next
    code <- unlist(lapply(maps[has], function(mp) mp[[v]]$code))
    lab <- unlist(lapply(maps[has], function(mp) mp[[v]]$label))
    pairs <- unique(paste(code, lab, sep = "\r"))
    if (!anyDuplicated(sub("\r.*$", "", pairs)) && !anyDuplicated(sub("^.*\r", "", pairs))) next
    all_char <- all(vapply(maps[has], function(mp) isTRUE(mp[[v]]$char), TRUE))
    union <- if (all_char) levels(factor(unique(lab))) else .rt_merge_levels(lapply(maps[has], function(mp) mp[[v]]$label))
    rejoined <- c(rejoined, v)
    for (m in has) {
      mp <- maps[[m]][[v]]
      map <- stats::setNames(as.character(match(mp$label, union)), mp$code)
      r <- mrows[[m]]
      at <- attributes(r)[c("positional")]
      r$key <- .rt_rekey(r$key, v, map)
      r$block <- .rt_rekey(r$block, v, map)
      attr(r, "positional") <- at$positional
      mrows[[m]] <- r
    }
  }
  .rt_check_level_union(mrows, vars)
  attr(mrows, "rejoined") <- rejoined
  mrows
}

# One level order from several models' orders: model 1's, with each level
# another model adds placed after the nearest level before it in that
# model (a subset model that lacks "fair" first, the full model second:
# poor, fair, avg, ...), as Stata orders by value code.
.rt_merge_levels <- function(orders) {
  out <- orders[[1]]
  for (o in orders[-1]) {
    for (j in seq_along(o)) {
      if (o[j] %in% out) next
      prev <- rev(o[seq_len(j - 1L)][o[seq_len(j - 1L)] %in% out])[1]
      at <- if (is.na(prev)) 0L else match(prev, out)
      out <- append(out, o[j], after = at)
    }
  }
  out
}

#' Order the joined levels of a variable by their new codes
#'
#' The union puts a level new to a later model at the end of its block;
#' for the variables `.rt_join_levels()` re-keyed, the block's level rows
#' are put in code order instead (Stata's value-code order).
#' @param u A `tt_regtab_union()` result.
#' @param vars Re-keyed variables (`attr(mrows, "rejoined")`).
#' @keywords internal
#' @noRd
.rt_order_joined_levels <- function(u, vars) {
  if (!length(vars) || !nrow(u$rows)) return(u)
  perm <- seq_len(nrow(u$rows))
  for (v in vars) {
    ix <- which(u$rows$kind %in% "level" & u$rows$block %in% v)
    if (length(ix) < 2L || any(diff(ix) != 1L)) next
    code <- suppressWarnings(as.numeric(sub("^(-?[0-9]+)\\..*$", "\\1", u$rows$key[ix])))
    if (anyNA(code) || !all(u$rows$key[ix] == paste0(code, ".", v))) next
    perm[ix] <- ix[order(code)]
  }
  if (identical(perm, seq_len(nrow(u$rows)))) return(u)
  .rt_subset(u, perm)
}

#' Refuse level rows that share a key but name different levels
#'
#' A last guard after `.rt_join_levels()`: a variable whose levels mix
#' value codes and positions (a factor with levels "0" and "a"), or a data
#' frame whose levels were keyed by position without labels regtab could
#' join, can still give one key two levels. That table is refused, never
#' shown with one model's estimate under another level's label.
#' @param mrows List of `tt_regtab_rows()` results.
#' @param vars Variables to check (positional in some model).
#' @keywords internal
#' @noRd
.rt_check_level_union <- function(mrows, vars = character()) {
  if (length(mrows) < 2L) return(invisible(TRUE))
  pos <- unique(c(vars, unlist(lapply(mrows, attr, "positional"))))
  if (!length(pos)) return(invisible(TRUE))
  lev <- do.call(rbind, lapply(seq_along(mrows), function(m) {
    r <- mrows[[m]]
    r <- r[r$kind %in% "level" & r$block %in% pos, c("key", "block", "label"), drop = FALSE]
    if (nrow(r)) r$model <- m else r$model <- integer()
    r
  }))
  if (is.null(lev) || !nrow(lev)) return(invisible(TRUE))
  lev$label <- trimws(lev$label)
  bad <- character()
  for (k in unique(lev$key)) {
    x <- lev[lev$key == k, , drop = FALSE]
    if (length(unique(x$label)) > 1L) {
      bad <- c(bad, paste0(k, ": ", paste0("\"", x$label, "\" (model ", x$model, ")", collapse = " vs ")))
    }
  }
  # ...and one level under two keys (it would show twice).
  for (b in unique(lev$block)) {
    x <- unique(lev[lev$block == b, c("key", "label", "model"), drop = FALSE])
    for (l in unique(x$label)) {
      y <- x[x$label == l, , drop = FALSE]
      if (length(unique(y$key)) > 1L) {
        bad <- c(bad, paste0("\"", l, "\": ", paste0(y$key, " (model ", y$model, ")", collapse = " vs ")))
      }
    }
  }
  if (length(bad)) {
    cli::cli_abort(c(
      "Factor levels cannot be matched across models: a key names different levels, or a level has different keys.",
      stats::setNames(utils::head(bad, 6L), rep("x", min(length(bad), 6L))),
      "i" = "regtab joins levels by their value codes, or by the levels themselves when there are none; these levels have neither consistently (for example a factor mixing numeric and text levels, or a data frame keyed by position).",
      "i" = "Give the variable the same levels in every model, keep Stata value codes with {.fn tt_as_factor}, or give a data frame a {.field key} column."
    ), call = NULL)
  }
  invisible(TRUE)
}
