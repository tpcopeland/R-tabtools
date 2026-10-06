# Shared render specification for the interop converters (task 6.1):
# as_flextable.tt_table() and tt_as_gt() read the same per-cell style
# state the Excel writer produces, so Word, HTML, and Quarto tables carry the
# house style without a second copy of its rules. The workbook's title row (row 1), gutter column (A), and
# footnote row become the caption, nothing, and a footer line.

# Excel border names to line widths in points (Excel draws thin, medium,
# and thick lines 1, 2, and 3 pixels wide at 96 dpi).
.tt_border_pt <- c(thin = 0.75, medium = 1.5, thick = 2.25)

# Excel column width (characters of the default font's widest digit, 7
# pixels at Calibri 11) to pixels as Excel converts it (7w + 5), plus the 4
# pixels of cell padding each side the converters set, which Excel's own
# cells barely have.
.tt_width_px <- function(w) round(7 * w + 5 + 8)

# "FFRRGGBB" (openxlsx2 ARGB) to "#RRGGBB"; NA stays NA.
.tt_argb_hex <- function(x) {
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- paste0("#", substring(x[ok], nchar(x[ok]) - 5L))
  out
}

# Leading blanks as no-break spaces, so the literal indents (3 in table1_tc,
# 2 in regtab) survive HTML whitespace collapsing and Word's cell trimming.
.tt_nbsp_indent <- function(x) {
  lead <- nchar(x) - nchar(sub("^ +", "", x))
  has <- !is.na(x) & lead > 0L
  x[has] <- paste0(strrep("\u00a0", lead[has]), substring(x[has], lead[has] + 1L))
  x
}

# Merged ranges "C7:E7" to integer (r1, c1, r2, c2).
.tt_parse_merges <- function(refs) {
  if (!length(refs)) return(data.frame(r1 = integer(), c1 = integer(), r2 = integer(), c2 = integer()))
  one <- function(a) {
    list(r = as.integer(sub("^[A-Z]+", "", a)),
         c = vapply(strsplit(sub("[0-9]+$", "", a), ""), function(ch) {
           Reduce(function(acc, v) acc * 26L + v, match(ch, LETTERS), 0L)
         }, 0L))
  }
  ends <- strsplit(refs, ":", fixed = TRUE)
  a <- one(vapply(ends, `[`, "", 1L))
  b <- one(vapply(ends, function(e) e[length(e)], ""))
  data.frame(r1 = a$r, c1 = a$c, r2 = b$r, c2 = b$c)
}

#' Render specification of a tt_table
#'
#' Header and body cells with the per-cell style the workbook gets, in the
#' table's own coordinates (header row 1-2, body row 1..n, column 1 = label).
#'
#' @param x A tt_table.
#' @param merged Whether the target merges cells (flextable). A merged
#'   range shows its anchor cell, so the anchor takes the range's right and
#'   bottom borders; gt, which cannot merge body cells, keeps them where the
#'   workbook rules put them.
#' @return A list: `header`, `body` (each a list of matrices `text`, `bold`,
#'   `italic`, `size`, `color`, `fill`, `halign`, `valign`, `wrap`, `top`,
#'   `bottom`, `left`, `right`), `merges` (data frame `part`, `i1`, `i2`, `j1`,
#'   `j2`), `widths` (Excel units), `title`, `footnote`, `font`, `fontsize`,
#'   `title_size`, `footnote_size`.
#' @keywords internal
#' @noRd
.tt_render_spec <- function(x, merged = TRUE) {
  .tt_check_table(x)
  x <- .tt_blank_text(x)
  if (!nrow(x$body)) {
    cli::cli_abort("Nothing to convert: the table has no body rows.", call = NULL)
  }
  rules <- x$layout$xlsx_rules
  if (identical(rules, "none") ||
      (rules %in% c("regression", "descriptive", "stratetab", "comptab", "hrcomptab") && ncol(x$body) < 2L)) {
    return(.tt_plain_spec(x))
  }
  nb <- nrow(x$body)
  # Worksheet rows of the header and the body: table1_tc/regtab put two
  # header rows at 2-3 and the body from row 4; puttab its header (if any)
  # at row 2 and the body below it; stacktab its first composite row at
  # row 2 (Phase 7a review F4).
  nh <- length(x$header)
  lay <- switch(rules,
                regression = .xlsx_layout_regtab(x),
                descriptive = .xlsx_layout_table1(x),
                puttab = .xlsx_layout_puttab(x),
                stacktab = .xlsx_layout_stacktab(x),
                stratetab = .xlsx_layout_stratetab(x),
                comptab = .xlsx_layout_comptab(x),
                hrcomptab = .xlsx_layout_hrcomptab(x))
  hdr_rows <- switch(rules, regression = , descriptive = , stratetab = , comptab = , hrcomptab = 2:3,
                     if (nh) seq.int(2L, length.out = nh) else integer())
  body_rows <- switch(rules, regression = , descriptive = , stratetab = , comptab = , hrcomptab = seq_len(nb) + 3L,
                      seq_len(nb) + 1L + nh)
  g <- lay$grid
  st <- .xlsx_apply_rules(lay$rules, nrow(g), ncol(g), x$style)
  first <- min(c(hdr_rows, body_rows))
  last <- max(body_rows)
  hdr_last <- if (length(hdr_rows)) max(hdr_rows) else first - 1L
  cc <- 2:ncol(g)
  mg <- .tt_parse_merges(st$merges)
  if (merged) {
    # Word keeps one border per shared horizontal edge and flextable writes
    # it from the upper cell's bottom (the lower cell's own top is dropped).
    # So every rule the workbook draws as the top of a row after the first
    # table row (the rule under regtab's model labels, table1_tc's rule
    # between header and body, the rules above stats/addrow/random-effects
    # rows, stacktab's section rules) is also given to the cell above as its
    # bottom, the heavier of the two winning. The top stays, so HTML output
    # is unchanged.
    for (r in seq.int(first + 1L, length.out = max(last - first, 0L))) {
      st$bottom[r - 1L, ] <- .tt_heavier(st$bottom[r - 1L, ], st$top[r, ])
    }
  }
  # A merged range shows its anchor cell: give the anchor the right border of
  # the range's last column and the bottom border of its last row, which is
  # where the workbook rules put them.
  for (k in seq_len(nrow(mg) * merged)) {
    m <- mg[k, ]
    st$right[m$r1, m$c1] <- st$right[m$r1, m$c2]
    st$bottom[m$r1, m$c1] <- st$bottom[m$r2, m$c1]
  }
  # gt cannot merge body cells, but a header label merged down both header
  # rows is one column label there too, so it takes the rule under the
  # range's last row (stratetab draws it on B3 of its B2:B3 merge).
  if (!merged) {
    for (k in which(mg$r1 < mg$r2 & mg$c1 == mg$c2 & mg$r2 <= hdr_last)) {
      st$bottom[mg$r1[k], mg$c1[k]] <- st$bottom[mg$r2[k], mg$c1[k]]
    }
  }
  part <- function(rr) {
    pick <- function(M) M[rr, cc, drop = FALSE]
    text <- pick(g)
    text[] <- .tt_nbsp_indent(text)
    list(text = text, bold = pick(st$bold), italic = pick(st$italic),
         size = pick(st$size), color = .tt_mat_hex(pick(st$fcolor)),
         fill = .tt_mat_hex(pick(st$fill)), halign = pick(st$halign),
         valign = pick(st$valign), wrap = pick(st$wrap), top = pick(st$top),
         bottom = pick(st$bottom), left = pick(st$left), right = pick(st$right))
  }
  # Title and footnote merges become the caption and the footer line; a
  # one-cell range merges nothing.
  keep <- mg$r1 >= first & mg$r2 <= last & mg$c1 >= 2L & !(mg$r1 == mg$r2 & mg$c1 == mg$c2)
  mg <- mg[keep, , drop = FALSE]
  in_head <- mg$r2 <= hdr_last
  merges <- data.frame(part = ifelse(in_head, "header", "body"),
                       i1 = ifelse(in_head, mg$r1 - first + 1L, mg$r1 - body_rows[1] + 1L),
                       i2 = ifelse(in_head, mg$r2 - first + 1L, mg$r2 - body_rows[1] + 1L),
                       j1 = mg$c1 - 1L, j2 = mg$c2 - 1L, stringsAsFactors = FALSE)
  header <- if (length(hdr_rows)) part(hdr_rows) else .tt_blank_part(ncol(x$body), x$style$fontsize)
  widths <- unname(st$widths[as.character(cc)])
  widths[is.na(widths)] <- 8.43
  fs <- x$style$fontsize
  list(header = header, body = part(body_rows), merges = merges, widths = widths,
       title = x$title,
       footnote = if (nrow(g) > last) .tt_footnote_text(g[seq.int(last + 1L, nrow(g)), 2]) else "",
       font = x$style$font, fontsize = fs, title_size = fs + 2,
       footnote_size = max(fs - 2, 6))
}

# One blank, unstyled header row: flextable and gt both need a header, and
# a table without one (puttab noheader) gets this.
.tt_blank_part <- function(nc, fontsize) {
  m <- function(v) matrix(v, 1L, nc)
  list(text = m(""), bold = m(FALSE), italic = m(FALSE), size = m(fontsize),
       color = m(NA_character_), fill = m(NA_character_), halign = m(NA_character_),
       valign = m(NA_character_), wrap = m(FALSE), top = m(NA_character_),
       bottom = m(NA_character_), left = m(NA_character_), right = m(NA_character_))
}

# Elementwise heavier of two border vectors (NA = no rule).
.tt_heavier <- function(a, b) {
  rank <- c(thin = 1L, medium = 2L, thick = 3L)
  ra <- rank[a]
  rb <- rank[b]
  ra[is.na(ra)] <- 0L
  rb[is.na(rb)] <- 0L
  unname(ifelse(rb > ra, b, a))
}

.tt_mat_hex <- function(M) {
  M[] <- .tt_argb_hex(M)
  M
}

# Tables without a workbook layout (a future command before its rule set
# exists): every header row, bold and centred, rules above and below the
# header and below the body.
.tt_plain_spec <- function(x) {
  hdr <- do.call(rbind, lapply(x$header, function(h) h$text))
  # A table without a header row (puttab noheader) gets one blank header
  # row: flextable and gt both need one.
  if (is.null(hdr)) hdr <- matrix("", 1L, ncol(x$body))
  body <- as.matrix(x$body)
  dimnames(hdr) <- dimnames(body) <- NULL
  mk <- function(text, head) {
    n <- nrow(text)
    k <- ncol(text)
    m <- function(v) matrix(v, n, k)
    text[] <- .tt_nbsp_indent(text)
    halign <- m("center")
    if (!head) halign[, 1] <- NA_character_
    list(text = text, bold = m(head), italic = m(FALSE), size = m(x$style$fontsize),
         color = m(NA_character_), fill = m(NA_character_), halign = halign,
         valign = m(if (head) "center" else NA_character_), wrap = m(head),
         top = m(NA_character_), bottom = m(NA_character_), left = m(NA_character_),
         right = m(NA_character_))
  }
  h <- mk(hdr, TRUE)
  b <- mk(body, FALSE)
  h$top[1, ] <- x$style$hborder
  h$bottom[nrow(hdr), ] <- x$style$hborder
  b$bottom[nrow(body), ] <- x$style$hborder
  fs <- x$style$fontsize
  list(header = h, body = b,
       merges = data.frame(part = character(), i1 = integer(), i2 = integer(),
                           j1 = integer(), j2 = integer()),
       widths = rep(12, ncol(body)), title = x$title, footnote = x$footnote,
       font = x$style$font, fontsize = fs, title_size = fs + 2,
       footnote_size = max(fs - 2, 6))
}

# Group the cells of a part by the value of `key` (a character matrix; NA
# cells skipped) and then by identical row sets, so a converter makes one
# call per rectangle of equal style instead of one per cell. Returns a list
# of list(value, i, j).
.tt_style_groups <- function(key) {
  out <- list()
  for (v in unique(key[!is.na(key)])) {
    hit <- !is.na(key) & key == v
    rows_by_col <- lapply(seq_len(ncol(key)), function(j) which(hit[, j]))
    sig <- vapply(rows_by_col, paste, "", collapse = ",")
    for (s in unique(sig[nzchar(sig)])) {
      js <- which(sig == s)
      out[[length(out) + 1L]] <- list(value = v, i = rows_by_col[[js[1]]], j = js)
    }
  }
  out
}

.tt_has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

.tt_need <- function(pkg, fun, version = NULL) {
  if (!.tt_has(pkg)) {
    cli::cli_abort("{.fn {fun}} needs the {.pkg {pkg}} package; install it first.", call = NULL)
  }
  # The Suggests minimum (gtsummary 2.3.0: modify_indent(); stack review
  # item 7).
  if (!is.null(version) && utils::packageVersion(pkg) < version) {
    cli::cli_abort("{.fn {fun}} needs {.pkg {pkg}} {version} or later (installed: {utils::packageVersion(pkg)}).", call = NULL)
  }
}
