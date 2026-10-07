# Crosstab publication geometry, pinned crosstab.ado:518-699,851-949.
.xt_build <- function(sample, inference, sc, o, style, sheet, title, footnote) {
  f <- sample$counts; nr <- nrow(f); nc <- ncol(f)
  rs <- rowSums(f); cs <- colSums(f); n <- sum(f)
  denominator <- switch(o$percent, column = matrix(rep(cs, each = nr), nr, nc),
                        row = matrix(rep(rs, nc), nr, nc), total = matrix(n, nr, nc))
  pct <- 100 * (f / denominator)
  pct[sc$pct_linked] <- NA_real_
  show_count <- function(value, code) {
    if (code == 0L) return(stata_fmt(value, "%11.0fc"))
    if (!is.null(o$masktext)) return(o$masktext)
    as.character(tt_sc_render(value, code, o$smallcells, "%11.0fc"))
  }
  body <- matrix("", nr + 2L, nc + 2L)
  body[seq_len(nr), 1L] <- sample$row$text
  for (r in seq_len(nr)) for (c in seq_len(nc)) {
    value <- show_count(f[r, c], sc$mask[r, c])
    if (!sc$pct_linked[r, c]) value <- paste0(value, " (", stata_fmt(pct[r, c], paste0("%21.", o$digits, "f")), "%)")
    body[r, c + 1L] <- value
  }
  body[seq_len(nr), nc + 2L] <- vapply(seq_len(nr), function(r) show_count(rs[r], sc$rowmask[r]), "")
  totalrow <- nr + 1L
  body[totalrow, 1L] <- "Total"
  body[totalrow, seq_len(nc) + 1L] <- vapply(seq_len(nc), function(c) show_count(cs[c], sc$colmask[c]), "")
  body[totalrow, nc + 2L] <- show_count(n, sc$totalmask)
  marker <- o$masktext %||% "Suppressed"
  p_phrase <- if (!inference$test_available) "not computed" else if (inference$p < .001) "p < 0.001" else
    paste0("p = ", stata_fmt(inference$p, "%5.3f"))
  body[nr + 2L, 1L] <- if (sc$derived) paste0(inference$test_name, ": ", marker) else
    if (inference$test_method == "Fisher exact") paste0(inference$test_name, ": ", p_phrase) else
      paste0(inference$test_name, ": chi2 = ", stata_fmt(inference$chi2, "%6.2f"), ", ", p_phrase)
  role <- c(rep("count", nr), "total", "test")
  for (measure in c("or", "rr", "rd")) if (isTRUE(o[[measure]])) {
    fmt <- paste0("%21.", o$digits + if (measure == "rd") 2L else 0L, "f")
    text <- if (sc$derived) paste0(toupper(measure), " = ", marker) else
      paste0(toupper(measure), " = ", stata_fmt(inference[[measure]], fmt), " (", .tt_level_text(o$level),
        "% CI: ", stata_fmt(inference[[paste0(measure, "_lo")]], fmt), ", ",
        stata_fmt(inference[[paste0(measure, "_hi")]], fmt), ")")
    body <- rbind(body, c(text, rep("", nc + 1L)))
    role <- c(role, measure)
  }
  if (o$trend || o$cochran) {
    text <- if (sc$derived) marker else if (inference$p_trend < .001) "<0.001" else
      stata_fmt(inference$p_trend, "%5.3f")
    lead <- if (o$cochran) "P for trend (Cochran-Armitage) = " else "P for trend = "
    body <- rbind(body, c(paste0(lead, text), rep("", nc + 1L)))
    role <- c(role, "trend")
  }
  stored <- inference
  if (sc$derived) for (key in names(stored)) if (is.numeric(stored[[key]])) stored[[key]] <- NA_real_
  stored$table <- f; stored$table[sc$mask > 0L] <- NA_real_
  stored$N <- if (sc$totalmask > 0L) NA_real_ else n
  stored$ci_level <- o$level
  n_derived <- as.integer(sum(sc$pct_linked) + if (sc$derived) sum(!role %in% c("count", "total")) else 0L)
  n_masked <- as.integer(sc$n_primary + sc$n_secondary)
  stored$smallcells <- list(threshold = as.integer(o$smallcells %||% 0L), mode = o$mode,
                           n_masked = n_masked, n_linked = n_derived)
  if (!is.null(o$smallcells)) {
    stored$smallcells_mode <- if (o$mode == "strict") "full" else "primary"
    stored$N_primary_suppressed <- as.integer(sc$n_primary)
    stored$N_secondary_suppressed <- as.integer(sc$n_secondary)
    stored$N_derived_suppressed <- n_derived
    stored$suppression <- sc$mask
    dimnames(stored$suppression) <- dimnames(f)
  }
  methods <- paste0("Cross-tabulation of ", sample$descriptor, " by ascending column codes. ",
                    "Statistical significance: ", inference$test_name, " (", inference$test_method, ").")
  if (o$or || o$rr || o$rd) methods <- paste0(methods,
    " Associations compare the second column with the first for the second row outcome: ",
    paste(c(if (o$or) "sample OR with native cc equal-tailed limits (Cornfield for zero counts)",
            if (o$rr) "RR with log-Wald limits", if (o$rd) "RD with binomial Wald limits"), collapse = "; "), ".")
  if (o$trend || o$cochran) methods <- paste0(methods, " Trend: ", inference$trend_method, ".")
  if (!inference$test_available) methods <- paste0(methods, " The exact test could not be computed (",
    inference$test_unavailable_reason, "); no approximation was substituted.")
  if (sc$derived) methods <- paste0(methods, " Count-dependent inference is withheld under strict count protection.")
  stored$methods <- paste0(methods, " Analysis performed in R ", getRversion(), ".")
  rs[sc$rowmask > 0L] <- NA_real_; cs[sc$colmask > 0L] <- NA_real_
  rowp <- rep(NA_real_, nrow(body))
  rowp[role == "test"] <- stored$p
  if ("trend" %in% role) rowp[role == "trend"] <- stored$p_trend
  rows <- data.frame(type = ifelse(role == "count", "level", "stat"),
                     key = paste0("crosstab:", seq_len(nrow(body))),
                     var = NA_character_, level = NA_character_, p = rowp,
                     crosstab_role = role, stringsAsFactors = FALSE)
  cols <- data.frame(role = c("label", rep("group", nc), "total"),
                     model = NA_real_, stringsAsFactors = FALSE)
  layout <- list(indent = 0L, align = "right", console_sepby = FALSE, console_title = TRUE,
                 console_blank = TRUE, console_footnote = TRUE, header_style = "plain",
                 xlsx_rules = "crosstab", sheet = sheet)
  note <- .xt_mask_note(o$smallcells, o$mode, o$masktext)
  if (!inference$test_available && !sc$derived) note <- c(paste0(
    "Fisher's exact test could not be computed for this table (", inference$test_unavailable_reason,
    "); no approximation was substituted."), note)
  tt <- tt_table(body = body, header = list(c(sample$descriptor, sample$column$text, "Total")),
                 rows = rows, cols = cols, title = title, footnote = c(footnote, note), style = style,
                 stored = stored, command = "crosstab", layout = layout,
                 meta = list(sheet = sheet, axes = list(row = sample$row, column = sample$column),
                   percentage_mode = o$percent, publication = list(row_totals = rs, column_totals = cs,
                     percentages = pct, percentage_linked = sc$pct_linked,
                     row_suppression = as.integer(sc$rowmask), column_suppression = as.integer(sc$colmask),
                     total_suppression = as.integer(sc$totalmask), inference_masked = sc$derived),
                   native_source = list(version = "2.5.1", pin = "712044f83ce6dd7bb4ca4237f3f6ffbc8aea5129",
                     command = "crosstab", spearman_engine = "Stata17 spearman.ado 4.2.8; t(N-2)")))
  tt
}

.xlsx_layout_crosstab <- function(x) {
  cells <- .tt_cells(x); K <- ncol(cells); xK <- K + 1L
  last <- nrow(cells) + 1L; foot <- nzchar(x$footnote)
  total <- last + as.integer(foot)
  grid <- matrix("", total, xK); grid[1L, 1L] <- x$title
  grid[seq.int(2L, last), -1L] <- cells
  if (foot) grid[total, 2L] <- x$footnote
  written <- matrix(TRUE, total, xK)
  # Native writes only B for the footer, outside the body font range.
  # Merged children retain workbook defaults; margin A is not written.
  if (foot) {
    written[total, ] <- FALSE
    written[total, 2L] <- TRUE
  }
  style <- x$style; hb <- .border_code(style$hborder)
  totalrow <- which(x$rows$crosstab_role == "total") + 2L
  first <- totalrow + 1L
  rules <- new.env(parent = emptyenv())
  rules$rows <- list()
  add <- function(...) rules$rows[[length(rules$rows) + 1L]] <- .rule(...)
  add("width", 1, 1, 1, 1, value = 1)
  labels <- c(x$header[[1L]]$text[1L], x$body[x$rows$crosstab_role == "count", 1L])
  add("width", 1, 1, 2, 2, value = max(12, ceiling(max(.blen(labels)) * .85) + 2))
  for (j in seq.int(3L, xK)) add("width", 1, 1, j, j, value = 14)
  add("font", 1, last, 1, xK, value = style$fontsize)
  add("font", 1, 1, 1, xK, value = style$fontsize + 2)
  add("merge", 1, 1, 1, xK); add("bold", 1, 1, 1, 1, code = 1)
  add("wrap", 1, 1, 1, 1, code = 1); add("halign", 1, 1, 1, 1, code = 1)
  add("valign", 1, 1, 1, 1, code = 2)
  add("top", 2, 2, 2, xK, code = hb); add("bottom", 2, 2, 2, xK, code = hb)
  add("bold", 2, 2, 2, xK, code = 1); add("halign", 2, 2, 2, xK, code = 2)
  if (style$headershade) add("fill", 2, 2, 2, xK, color = style$headercolor)
  add("halign", 3, last, 3, xK, code = 2)
  add("top", totalrow, totalrow, 2, xK, code = hb)
  add("bottom", totalrow, totalrow, 2, xK, code = hb)
  add("top", first, first, 2, xK, code = hb)
  for (r in seq.int(first, last)) {
    add("merge", r, r, 2, xK); add("halign", r, r, 2, 2, code = 1)
    add("valign", r, r, 2, 2, code = 2)
  }
  add("bottom", last, last, 2, xK, code = hb)
  if (style$borderstyle != "academic") {
    vb <- if (style$borderstyle == "medium") 2L else 1L
    add("left", 2, last, 2, 2, code = vb); add("right", 2, last, xK, xK, code = vb)
    add("right", 2, totalrow, 2, 2, code = vb)
  }
  if (is.finite(style$boldp) && style$boldp != -1) for (r in which(!is.na(x$rows$p) & x$rows$p < style$boldp)) {
    add("bold", r + 2L, r + 2L, 2, xK, code = 1)
  }
  if (style$zebra && totalrow >= 4L) for (r in seq.int(4L, totalrow, by = 2L)) {
    add("fill", r, r, 2, xK, color = style$zebracolor)
  }
  if (foot) {
    add("merge", total, total, 2, xK); add("halign", total, total, 2, 2, code = 1)
    add("valign", total, total, 2, 2, code = 2); add("wrap", total, total, 2, 2, code = 1)
    add("font", total, total, 2, 2, value = max(style$fontsize - 2, 6))
    add("italic", total, total, 2, 2, code = 1)
  }
  .xlsx_expand_footnotes(list(grid = grid, written = written, rules = do.call(rbind, rules$rows)))
}
