# table1_tc layout (plan tasks 2.6, 2.7, 2.10): group columns, cell text,
# rows, descriptor, Dapa/methods, and stored results. Ports the row builder
# of _desctab_collect.ado:866-1260 and the finishing steps of
# desctab.ado:673-1519.

#' Analysis sample and group columns
#'
#' `by` values must be strings or non-negative integers
#' (desctab.ado:415-470); strings are encoded in sorted (C-locale) order and
#' factors keep their level order (R-only). Header labels: the value label,
#' else `by = level`; with `total()` and no value label Stata attaches a
#' temporary label holding only "Total", so the other columns show the bare
#' code (desctab.ado:680-701, verified in Stata 17). String groups are
#' numbered over every level in the column (Stata `encode`s the whole
#' dataset, desctab.ado:435), so `codes` are those positions. An empty
#' string is missing (table1_tc() turns `""` into `NA` first). `labels`
#' overrides the by() variable label in the methods paragraph. `by_full` is
#' the by() column before wt()/fweight dropped records: string levels are
#' numbered over it, as Stata's `encode` numbers them over the whole dataset
#' (verified in Stata 17: with every "a" record at weight 0, the
#' suppression columns are `g_2 g_3`).
#' @keywords internal
#' @noRd
.t1_groups <- function(data, by, total, labels = NULL, by_full = NULL) {
  n <- nrow(data)
  # desctab.ado:412-416: no observations at all is an error, by() or not.
  if (!n) cli::cli_abort("no observations.", call = NULL)
  if (is.null(by)) {
    return(list(gid = rep(1L, n), G = 1L, labels = "Total", codes = 1, note_labels = "Total",
                bylab = NULL, total = FALSE))
  }
  col <- data[[by]]
  .t1_check_column(col, by, "by")
  vallab <- is.character(col) || is.factor(col) || tt_has_value_labels(col)
  if (is.character(col) || is.factor(col)) {
    lev <- level_labels(if (is.null(by_full)) col else by_full)
    gid <- level_index(col, lev)
    codes <- seq_len(nrow(lev))
    labs <- lev$label
    present <- sort(unique(gid[!is.na(gid)]))
    codes <- codes[present]
    labs <- labs[present]
    gid <- match(gid, present)
  } else {
    v <- .t1_values(col)
    obs <- v[!is.na(v)]
    if (any(obs != round(obs)) || any(obs < 0)) {
      cli::cli_abort(paste("{.arg by} variable must be either (i) string, or (ii) numeric and contain only",
                           "non-negative integers, whether or not a value label is attached."), call = NULL)
    }
    codes <- sort(unique(obs))
    gid <- match(v, codes)
    txt <- stata_macro_text(codes)
    vl <- value_labels(col)
    lab_of <- txt
    if (!is.null(vl)) {
      hit <- match(codes, unname(unclass(vl)))
      lab_of[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
    }
    labs <- if (vallab) lab_of else if (total) txt else paste0(by, " = ", txt)
  }
  if (!any(!is.na(gid))) cli::cli_abort("no observations.", call = NULL)
  if (length(codes) < 2L) {
    cli::cli_abort("{.arg by} variable must have at least 2 levels.", call = NULL)
  }
  note_labels <- if (is.character(col) || is.factor(col)) labs else {
    vl <- value_labels(col)
    out <- stata_macro_text(codes)
    if (!is.null(vl)) {
      hit <- match(codes, unname(unclass(vl)))
      out[!is.na(hit)] <- names(vl)[hit[!is.na(hit)]]
    }
    out
  }
  list(gid = gid, G = length(codes), labels = labs, codes = codes, note_labels = note_labels,
       bylab = var_label(col, by, labels), total = total)
}

# Percentage text with the spacelowpercent rule (_desctab_collect.ado:
# 1028-1036 for bin, :1174-1182 for cat levels). o$percsign is the cell
# version, trimmed of blanks (see table1_tc()).
.t1_perc <- function(pct, pfmt, o, cat_level) {
  if (is.na(pct)) return("")
  s <- stata_fmt(pct, pfmt)
  low <- o$spacelowpercent && pct < 10 && !s %in% c("10", "10.0", "10.00")
  if (low) s <- paste0(if (cat_level && o$extraspace) "  " else " ", s)
  paste0(s, o$percsign)
}

# n (%), % (n), % only, with slashN (_desctab_collect.ado:1037-1106).
.t1_count_cell <- function(cnt, den_slash, perc, o) {
  nstr <- stata_fmt(cnt, o$nformat)
  if (o$slashN) nstr <- paste0(nstr, "/", stata_fmt(den_slash, o$nformat))
  if (!o$percent_n && !o$percent) {
    cola <- nstr
    colb <- paste0("(", perc, ")")
  } else {
    cola <- perc
    colb <- ""
  }
  if (o$percent_n && !o$percent) colb <- paste0("(", nstr, ")")
  if (nzchar(colb)) paste(cola, colb) else cola
}

.t1_fmts <- function(spec, o) {
  fmt1 <- if (is.na(spec$fmt1)) o$format else spec$fmt1
  # _desctab_collect.ado:926-934 (2.1.10+): a GSD with no format of its own
  # follows an explicit fmt1; with neither, the GSD default (o$gsdformat,
  # set only when format() was not given) replaces the mean's format.
  fmt2 <- if (!is.na(spec$fmt2)) spec$fmt2 else
    if (identical(spec$type, "contln") && is.na(spec$fmt1) && !is.null(o$gsdformat)) o$gsdformat else fmt1
  pfmt <- if (is.na(spec$fmt1)) o$percformat else spec$fmt1
  list(fmt1 = fmt1, fmt2 = fmt2, pfmt = pfmt)
}

# Rows of one continuous variable.
.t1_cont_rows <- function(spec, v, gp, o, col_masks) {
  # Phase 3: weights and smallcells() build the rows in R/table1_weights.R.
  if (!is.null(o$wx)) return(.t1w_cont_rows(spec, v, gp, o, col_masks))
  f <- .t1_fmts(spec, o)
  cells <- character(length(col_masks))
  nn <- numeric(length(col_masks))
  for (k in seq_along(col_masks)) {
    s <- .t1_cont_stats(v[col_masks[[k]]], spec$type)
    nn[k] <- s$n
    if (is.na(s$a)) next
    cells[k] <- switch(spec$type,
      contn = paste0(stata_fmt(s$a, f$fmt1), o$sdleft, stata_fmt(s$b, f$fmt2), o$sdright),
      contln = paste0(stata_fmt(s$a, f$fmt1), o$gsdleft, stata_fmt(s$b, f$fmt2), o$gsdright),
      conts = paste0(stata_fmt(s$a, f$fmt1), " (", stata_fmt(s$b, f$fmt2), o$iqrmiddle,
                     stata_fmt(s$c, f$fmt2), ")"))
  }
  lab <- spec$label
  if (o$varlabplus) {
    lab <- paste0(lab, ", ", switch(spec$type,
      contn = paste0("mean", o$sdleft, "SD", o$sdright),
      contln = paste0("geometric mean", o$gsdleft, "GSD", o$gsdright),
      conts = paste0("median (Q1", o$iqrmiddle, "Q3)")))
  }
  # `key` is Stata's factor_sep (_desctab_collect.ado:939): the listing
  # draws a rule where it changes.
  list(list(label = lab, cells = cells, type = "var", N = nn, has_stats = TRUE, key = lab))
}

# Rows of one binary variable (_desctab_collect.ado:987-1107).
.t1_bin_rows <- function(spec, v, gp, o, col_masks) {
  if (!is.null(o$wx)) return(.t1w_bin_rows(spec, v, gp, o, col_masks))
  f <- .t1_fmts(spec, o)
  cells <- character(length(col_masks))
  nn <- numeric(length(col_masks))
  for (k in seq_along(col_masks)) {
    x <- v[col_masks[[k]]]
    x <- x[!is.na(x)]
    den <- length(x)
    cnt <- sum(x == 1)
    nn[k] <- den
    pct <- if (den > 0) 100 * cnt / den else NA_real_
    cells[k] <- .t1_count_cell(cnt, den, .t1_perc(pct, f$pfmt, o, FALSE), o)
  }
  lab <- if (o$varlabplus) paste0(spec$label, ", ", o$percfootnote) else spec$label
  list(list(label = lab, cells = cells, type = "var", N = nn, has_stats = TRUE, key = lab))
}

# Rows of one categorical variable: a label row, then one indented row per
# observed level (_desctab_collect.ado:1108-1260).
.t1_cat_rows <- function(spec, code, levlab, gp, o, col_masks) {
  if (!is.null(o$wx)) return(.t1w_cat_rows(spec, code, levlab, gp, o, col_masks))
  f <- .t1_fmts(spec, o)
  nlev <- length(levlab)
  levels <- seq_len(nlev)
  miss_level <- o$missing && any(is.na(code[!is.na(gp$gid)]))
  if (miss_level) levels <- c(levels, NA)
  labs <- c(levlab, if (miss_level) "Missing")
  ng <- gp$G
  K <- length(col_masks)
  cnt <- matrix(0, length(levels), K)
  grpN <- numeric(K)
  for (k in seq_len(K)) {
    x <- code[col_masks[[k]]]
    if (!o$missing) x <- x[!is.na(x)]
    grpN[k] <- length(x)
    for (li in seq_along(levels)) {
      cnt[li, k] <- if (is.na(levels[li])) sum(is.na(x)) else sum(x == levels[li], na.rm = TRUE)
    }
  }
  rowden <- rowSums(cnt[, seq_len(ng), drop = FALSE])
  lab <- if (o$varlabplus) paste0(spec$label, ", ", o$percfootnote2) else spec$label
  # factor_sep is the bare variable label on every row of a categorical
  # block, varlabplus or not (_desctab_collect.ado:1119, :1150).
  out <- list(list(label = lab, cells = rep("", K), type = "cat_header", N = grpN, has_stats = TRUE,
                   key = spec$label))
  for (li in seq_along(levels)) {
    cells <- character(K)
    for (k in seq_len(K)) {
      den <- if (o$catrowperc) rowden[li] else grpN[k]
      pct <- if (den > 0) 100 * cnt[li, k] / den else NA_real_
      den_slash <- if (o$catrowperc) rowden[li] else grpN[k]
      cells[k] <- .t1_count_cell(cnt[li, k], den_slash, .t1_perc(pct, f$pfmt, o, TRUE), o)
    }
    out[[length(out) + 1L]] <- list(label = paste0("   ", labs[li]), cells = cells, type = "level",
                                    N = rep(NA_real_, K), has_stats = FALSE, key = spec$label)
  }
  out
}

# Alternatives joined the way desctab 2.1.10+ joins them (desctab.ado
# :1293-1304, :1343-1349): each row shows exactly one summary, so "A",
# "A or B", "A, B, or C".
.t1_join_alt <- function(x) {
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  if (n == 2L) return(paste(x[1], "or", x[2]))
  paste0(paste(x[-n], collapse = ", "), ", or ", x[n])
}

# Descriptor text in header row 2 (desctab.ado:1192-1307).
.t1_descriptor <- function(has, o) {
  parts <- character()
  if (has$cat) {
    parts <- c(parts, if (o$percent) {
      if (o$catrowperc) "Row %" else "Column %"
    } else if (o$catrowperc) {
      if (o$percent_n) "Row % (No.)" else "No. (Row %)"
    } else if (o$percent_n) "Column % (No.)" else "No. (Column %)")
  }
  if (has$bin && (!has$cat || o$catrowperc)) {
    parts <- c(parts, if (o$percent) "Column %" else if (o$percent_n) "Column % (No.)" else "No. (Column %)")
  }
  # desctab builds the descriptor from the raw delimiters (an empty sdleft
  # gives "MeanSD"), while cells use _desctab_collect's defaults. Since
  # 2.1.10 the geometric-mean caption follows gsdleft()/gsdright() too.
  d <- o$desc %||% o
  if (has$contn) parts <- c(parts, paste0("Mean", d$sdleft, "SD", d$sdright))
  if (has$contln) parts <- c(parts, paste0("Geometric mean", d$gsdleft, "GSD", d$gsdright))
  if (has$conts) parts <- c(parts, paste0("Median (Q1", d$iqrmiddle, "Q3)"))
  .t1_join_alt(parts)
}

# "No. (%)" style wording for Dapa and varlabplus (desctab.ado:362-390).
.t1_percfootnotes <- function(o) {
  n <- if (o$slashN) "No./total" else "No."
  percentage <- "%"
  foot2 <- NULL
  if (o$catrowperc) {
    p2 <- "column %"
    foot2 <- if (o$percent) p2 else if (o$percent_n) paste0(p2, " (", n, ")") else paste0(n, " (", p2, ")")
    percentage <- "row %"
  }
  foot <- if (o$percent) percentage else if (o$percent_n) paste0(percentage, " (", n, ")") else
    paste0(n, " (", percentage, ")")
  list(foot = foot, foot2 = foot2 %||% foot)
}

# Dapa sentence (desctab.ado:1316-1379).
.t1_dapa <- function(has, o, nopvalue) {
  # desctab.ado:356-362: meanSD/gmeanSD from the raw delimiters.
  d <- o$desc %||% o
  meansd <- paste0("mean", d$sdleft, "SD", d$sdright)
  gmean <- paste0("geometric mean", d$gsdleft, "GSD", d$gsdright)
  # desctab.ado:1327-1349 (2.1.10+): joined like the header, and the
  # median follows iqrmiddle().
  ycont <- .t1_join_alt(c(if (has$contn) meansd, if (has$contln) gmean,
                          if (has$conts) paste0("median (Q1", d$iqrmiddle, "Q3)")))
  catbin <- has$cat || has$bin
  ymix <- if (nzchar(ycont) && catbin) {
    paste0(ycont, " for continuous measures, and ", o$percfootnote, " for categorical measures")
  } else if (nzchar(ycont)) ycont else if (catbin) o$percfootnote else ""
  if (o$catrowperc && has$cat && has$bin) ymix <- paste0(ymix, " and ", o$percfootnote2, " for binary measures")
  if (nopvalue) paste0("Data are presented as ", ymix, ". P-values suppressed.")
  else paste0("Data are presented as ", ymix, ".")
}

# R test names in the methods paragraph, in Stata's order (desctab.ado:1385-1407).
# Monte Carlo variants carry their replicate count ("fisher_sim:1e+05").
.t1_methods_tests <- function(used) {
  used <- unique(used)
  reps <- function(prefix) {
    b <- as.numeric(sub("^[^:]*:", "", used[startsWith(used, prefix)]))
    vapply(sort(unique(b)), function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE), "")
  }
  mc <- function(test, prefix) {
    vapply(reps(prefix), function(b) paste0(test, " with a Monte Carlo p-value (", b, " replicates)"), "")
  }
  hit <- c(
    if ("welch_t" %in% used) "Welch's t-test",
    if ("t" %in% used) "Student's t-test",
    if ("paired_t" %in% used) "paired t-test",
    if ("welch_anova" %in% used) "Welch's one-way ANOVA",
    if ("anova" %in% used) "one-way ANOVA",
    if ("wilcoxon" %in% used) "Wilcoxon rank-sum test",
    if ("signrank" %in% used) "Wilcoxon signed-rank test",
    if ("kruskal" %in% used) "Kruskal-Wallis test",
    if ("chisq" %in% used) "Pearson's chi-squared test",
    if ("chisq_yates" %in% used) "Pearson's chi-squared test with Yates' continuity correction",
    mc("Pearson's chi-squared test", "chisq_sim:"),
    if ("fisher" %in% used) "Fisher's exact test",
    mc("Fisher's exact test", "fisher_sim:"))
  paste(hit, collapse = ", ")
}

# Stata matrix row names from row labels (desctab.ado:1497-1530, 2.1.10+):
# the trimmed label with "." and " " as "_", "," removed, and quotes,
# backtick, "$", "\\" and ":" as "_", cut to 32 characters; a name that
# `matrix rownames` would not store unchanged goes through strtoname()
# (.stata_rowname_survives(), .stata_strtoname()), and a repeated name gets
# "_2", "_3", ... within 32 characters. `prev` holds the names so far.
.t1_rowname <- function(lab, i, prev = character()) {
  s <- trimws(lab)
  s <- gsub(".", "_", s, fixed = TRUE)
  s <- gsub(" ", "_", s, fixed = TRUE)
  s <- gsub(",", "", s, fixed = TRUE)
  s <- gsub("[\"'`$\\\\:]", "_", enc2utf8(s), perl = TRUE)
  s <- substr(s, 1L, 32L)
  if (!.stata_rowname_survives(s)) {
    s <- .stata_strtoname(s)
    if (!.stata_rowname_survives(s)) s <- paste0("row", i)
  }
  base <- s
  k <- 1L
  while (s %in% prev) {
    k <- k + 1L
    suf <- paste0("_", k)
    s <- paste0(substr(base, 1L, 32L - nchar(suf)), suf)
  }
  s
}

# Labels desctab treats as structural rather than as variables: the header
# and N rows (" ", "", "N") and the ESS row. A variable whose label is one of
# these gets no missing-summary row (desctab.ado:719) and no r(table) row
# (:1472-1481, which also skips the descriptor text), and a label "N" is
# blanked (:1058) -- all verified in Stata 17 (a variable labelled "N").
.t1_structural_labels <- c(" ", "", "N", "Effective sample size")

#' Assemble the table1_tc tt_table
#' @keywords internal
#' @noRd
.t1_build <- function(data, specs, gp, o, style, title, footnote, sheet, labels) {
  gid <- gp$gid
  G <- gp$G
  total <- o$total != "none" && G > 1L
  col_masks <- lapply(seq_len(G), function(g) !is.na(gid) & gid == g)
  if (total) col_masks <- c(col_masks, list(!is.na(gid)))
  K <- length(col_masks)
  sampleN <- vapply(col_masks, sum, 0)
  # Phase 3: frequency weights count on the weighted scale.
  if (!is.null(o$wx)) sampleN <- vapply(col_masks, function(m) sum(o$wx$d[m]), 0)
  suppress_p <- o$nopvalue
  has_p_col <- G > 1L && !suppress_p
  show_test <- o$test && !suppress_p
  show_stat <- o$statistic && !suppress_p
  has <- list(cat = FALSE, bin = FALSE, contn = FALSE, contln = FALSE, conts = FALSE)
  used <- character()
  simulated <- character()
  blocks <- list()
  varlist <- character()
  for (spec in specs) {
    type <- spec$type
    varlist <- c(varlist, spec$name)
    x <- spec$x
    if (type %in% c("cat", "cate")) {
      lev <- level_labels(.t1_sub(x, !is.na(gid) & !is.na(.t1_values(x))))
      v <- level_index(x, lev)
      rows <- .t1_cat_rows(spec, v, lev$label, gp, o, col_masks)
      has$cat <- TRUE
    } else {
      v <- as.numeric(.t1_values(x))
      rows <- if (type %in% c("bin", "bine")) .t1_bin_rows(spec, v, gp, o, col_masks) else
        .t1_cont_rows(spec, v, gp, o, col_masks)
      if (type %in% c("bin", "bine")) has$bin <- TRUE else has[[type]] <- TRUE
    }
    tst <- list(p = NA_real_, test = "", statistic = "", used = NA_character_, simulated = FALSE)
    if (!suppress_p) {
      tst <- if (identical(o$wx$kind, "fw")) .t1w_test_fw(type, v, gid, o$wx$w, o$missing, o$test_args, var = spec$name)
             else .t1_test(type, v, gid, o$missing, o$test_args, var = spec$name)
    }
    if (!is.na(tst$used)) used <- c(used, tst$used)
    if (isTRUE(tst$simulated)) simulated <- c(simulated, spec$name)
    for (i in seq_along(rows)) {
      rows[[i]]$var <- spec$name
      rows[[i]]$vtype <- type
      # Stata's factor_sep, for builders that do not set it (the weighted
      # ones): the bare label on a categorical block, else the row label.
      if (is.null(rows[[i]]$key)) rows[[i]]$key <- if (type %in% c("cat", "cate")) spec$label else rows[[1]]$label
    }
    smdtype <- o$smdtype %||% "pair"
    pair <- o$smdpair %||% c(1L, 2L)
    smd <- if (!o$smd || G < 2L) NA_real_
           else if (smdtype != "pair") .t1_smd_multi(type, v, gid, G, smdtype, o$wx$w, o$wx$kind %||% "none")
           else if (isTRUE(o$wx$kind %in% c("wt", "fw"))) .t1w_smd(type, v, gid, o$wx$w, o$wx$kind, pair[1], pair[2])
           else .t1_smd(type, v, gid, pair[1], pair[2])
    rows[[1]]$p <- tst$p
    rows[[1]]$test <- tst$test
    rows[[1]]$statistic <- tst$statistic
    rows[[1]]$smd <- smd
    blocks[[length(blocks) + 1L]] <- rows
  }

  # Missing-data summary rows (desctab.ado:713-778): m = column maximum N
  # (the N row) minus the row's N, "m (pct)" with percformat, "0" when none.
  if (o$missingsummary) {
    maxN <- sampleN
    for (b in blocks) for (r in b) maxN <- pmax(maxN, ifelse(is.na(r$N), -Inf, r$N))
    for (bi in seq_along(blocks)) {
      extra <- list()
      for (r in blocks[[bi]]) {
        if (all(is.na(r$N))) next
        if (r$label %in% .t1_structural_labels) next
        m <- maxN - r$N
        if (!any(m > 0)) next
        # desctab.ado:749: the untrimmed percsign (desctab's own copy).
        cells <- ifelse(m > 0, paste0(stata_fmt(m, o$nformat), " (",
                                      stata_fmt(m / maxN * 100, o$percformat), o$percsign_raw %||% o$percsign,
                                      ")"), "0")
        extra[[length(extra) + 1L]] <- list(label = "   Missing", cells = cells, type = "missing_summary",
                                            N = rep(NA_real_, K), has_stats = FALSE, key = r$key,
                                            var = r$var, vtype = r$vtype)
      }
      blocks[[bi]] <- c(blocks[[bi]], extra)
    }
  }

  # N row and header texts.
  ntext <- paste0("N=", stata_fmt(sampleN, o$nformat))
  if (o$headerperc) {
    nnum <- as.numeric(gsub(",", "", stata_fmt(sampleN, o$nformat), fixed = TRUE))
    den <- if (total) nnum[K] else sum(nnum[seq_len(G)])
    if (!is.na(den) && den > 0) {
      # desctab.ado:1176: the untrimmed percsign.
      ntext <- paste0(stata_fmt(sampleN, o$nformat), " (",
                      stata_fmt(stata_round(nnum / den, 0.001) * 100, "%9.1f"), o$percsign_raw %||% o$percsign, ")")
    } else {
      ntext <- stata_fmt(sampleN, o$nformat)
    }
  }
  glab <- c(gp$labels, if (total) "Total")

  # Column order (desctab.ado:958-1053): label, groups (+ Total), Test,
  # Statistic, p-value, SMD; total(before) moves Total before the first group;
  # total(after) moves it immediately before p-value when p-values are shown
  # (so after Test/Statistic; verified in Stata 17), else it stays last among
  # the groups.
  keys <- c("label", paste0("g", seq_len(G)), if (total) "T",
            if (show_test) "test", if (show_stat) "statistic", if (has_p_col) "p", if (o$smd) "smd")
  if (total && o$total == "before") {
    keys <- c("label", "T", setdiff(keys, c("label", "T")))
  } else if (total && o$total == "after" && has_p_col) {
    rest <- setdiff(keys, "T")
    at <- match("p", rest)
    keys <- append(rest, "T", after = at - 1L)
  }
  grp_idx <- function(key) if (key == "T") K else as.integer(sub("^g", "", key))

  header1 <- vapply(keys, function(k) switch(k, label = " ", test = "Test", statistic = "Statistic",
                                                 p = "p-value", smd = .t1_smd_header(o$smdtype, gp, o$smdpair), glab[grp_idx(k)]), "")
  pf <- .t1_percfootnotes(o)
  descriptor <- .t1_descriptor(has, o)
  header2 <- vapply(keys, function(k) switch(k, label = descriptor, test = "", statistic = "", p = "",
                                                 smd = "", ntext[grp_idx(k)]), "")

  body <- list()
  meta_rows <- list()
  add_row <- function(r, type) {
    cells <- vapply(keys, function(k) {
      switch(k, label = r$label,
             test = if (isTRUE(r$has_stats)) r$test %||% "" else "",
             statistic = if (isTRUE(r$has_stats)) r$statistic %||% "" else "",
             p = if (isTRUE(r$has_stats) && !is.null(r$p)) format_p(r$p, o$pdp, o$highpdp) else "",
             smd = if (isTRUE(r$has_stats) && !is.null(r$smd) && !is.na(r$smd)) stata_fmt(abs(r$smd), "%5.3f") else "",
             r$cells[grp_idx(k)])
    }, "")
    # desctab.ado:1058: a row labelled "N" is blanked.
    if (identical(cells[["label"]], "N")) cells[["label"]] <- ""
    body[[length(body) + 1L]] <<- cells
    meta_rows[[length(meta_rows) + 1L]] <<- data.frame(
      type = type, label = cells[["label"]],
      p = if (isTRUE(r$has_stats) && !is.null(r$p)) r$p else NA_real_,
      smd = if (isTRUE(r$has_stats) && !is.null(r$smd)) abs(r$smd) else NA_real_,
      key = r$key %||% r$label, var = r$var %||% NA_character_, vtype = r$vtype %||% NA_character_,
      stringsAsFactors = FALSE)
  }
  # Stata's N row (descriptor in its label cell) is header row 2; the body
  # holds the variable rows.
  for (b in blocks) for (r in b) add_row(r, r$type)
  body <- do.call(rbind, body)
  rows <- do.call(rbind, meta_rows)
  rows$block <- .t1_blocks(rows$key)

  role <- vapply(keys, function(k) switch(k, label = "label", test = "test", statistic = "statistic",
                                              p = "p", smd = "smd", T = "total", "group"), "")
  cols <- data.frame(role = unname(role), model = NA_integer_,
                     console_width = ifelse(role == "smd", 7L, NA_integer_),
                     stringsAsFactors = FALSE)

  # Stored results (desctab.ado:1316-1421, :1460-1519).
  dapa <- .t1_dapa(has, c(o, list(percfootnote = pf$foot, percfootnote2 = pf$foot2)), o$nopvalue)
  stored <- list(Dapa = dapa)
  if (!is.null(gp$bylab) && !suppress_p) {
    tests <- .t1_methods_tests(used)
    m <- paste0("Baseline characteristics were compared between groups defined by ", gp$bylab, ". ", dapa)
    if (nzchar(tests)) m <- paste0(m, " P-values were calculated using ", tests, ".")
    stored$methods <- paste0(m, " A two-sided p-value < 0.05 was considered statistically significant.")
  }
  stored$varlist <- paste(varlist, collapse = " ")
  if (!suppress_p) stored$fisher_simulated <- unique(simulated)
  # desctab.ado:1077-1082, :1472 (2.1.10+): one r(table) row per analysed
  # variable (its own row: never a category level or missing-summary row),
  # except a variable whose key is "N"; no row limit.
  tr <- which(rows$type %in% c("var", "cat_header") & !rows$key %in% c("", "N"))
  cn <- c(if (!suppress_p) "p_value", if (o$smd) "smd")
  if (length(cn) && length(tr)) {
    tab <- cbind(if (!suppress_p) rows$p[tr], if (o$smd) rows$smd[tr])
    rn <- character()
    for (i in seq_along(tr)) rn <- c(rn, .t1_rowname(rows$label[tr[i]], i, rn))
    dimnames(tab) <- list(rn, cn)
    stored$table <- tab
  }
  stored$types <- paste(vapply(specs, function(s) s$type, ""), collapse = " ")

  meta <- list(sheet = sheet, extraspace = isTRUE(o$extraspace))
  # Phase 3: the row records, for small-cell finishing (.t1_finish_pass()).
  if (!is.null(o$wx)) meta$blocks <- blocks
  # desctab.ado (tabtools 2.4.0): the console omits the footnote, so the
  # note naming what the SMD column compares prints above the listing.
  smd_note <- if (o$smd) .t1_smd_note(o$smdtype, gp, o$smdpair) else NULL
  if (!is.null(smd_note)) meta$console_before <- paste("Note:", smd_note)
  # table_row: the body row's row in r(table) (NA when it has none).
  rows$table_row <- NA_integer_
  if (!is.null(stored$table)) rows$table_row[tr] <- seq_along(tr)
  tt_table(body, list(unname(header1), unname(header2)),
           rows = data.frame(type = rows$type, var = rows$var, vtype = rows$vtype, key = rows$key,
                             block = rows$block, table_row = rows$table_row, p = rows$p, smd = rows$smd,
                             stringsAsFactors = FALSE),
           cols = cols, title = title, footnote = footnote, style = style, stored = stored,
           command = "table1_tc", meta = meta)
}

# Console separator groups from Stata's factor_sep keys (desctab.ado:1312,
# `list ..., sepby(factor_sep)`): a rule wherever the key changes, so
# consecutive variables with the same label share one block (verified in
# Stata 17). The two header rows carry keys "" and "N" and are groups -1 and
# -2 in tt_console_lines(); a leading body row keyed "N" (a variable
# labelled "N") continues the N row's group.
.t1_blocks <- function(keys) {
  n <- length(keys)
  if (!n) return(integer())
  prev <- c("N", keys[-n])
  blk <- cumsum(keys != prev)
  as.integer(ifelse(blk == 0L, -2L, blk))
}

# SMD column header and the note naming what it compares (desctab.ado,
# tabtools 2.4.0). A pair SMD over 3+ groups, or a pair chosen with
# smdpair, names the pair in its header, so no sink shows a bare "SMD"
# beside groups it ignores; "Pop. SB" and "Max SMD" fit the column's
# 7-character console width. `pair` holds the two group indices from
# smdpair (NULL: the first two groups).
.t1_smd_header <- function(smdtype, gp, pair = NULL) {
  smdtype <- smdtype %||% "pair"
  if (smdtype == "population") return("Pop. SB")
  if (smdtype == "maxpair") return("Max SMD")
  if (gp$G <= 2L && is.null(pair)) return("SMD")
  lv <- pair %||% c(1L, 2L)
  sprintf("SMD (%s vs %s)", gp$note_labels[lv[1]], gp$note_labels[lv[2]])
}

# The note joins the footnote, so the comparison travels with every export;
# NULL for a two-group pair SMD, whose header says it all.
.t1_smd_note <- function(smdtype, gp, pair = NULL) {
  smdtype <- smdtype %||% "pair"
  G <- gp$G
  if (smdtype == "population") {
    return(paste0("Pop. SB: largest absolute difference between a group mean and the overall mean, ",
                  "in overall-sample SDs, across the ", G, " groups (McCaffrey et al. 2013)."))
  }
  if (smdtype == "maxpair") {
    return(paste0("Max SMD: largest absolute pairwise difference across the ", G,
                  " groups, in the root-mean of the group variances."))
  }
  if (G <= 2L) return(NULL)
  lv <- pair %||% c(1L, 2L)
  sprintf("SMD compares %s vs %s only (%s).", gp$note_labels[lv[1]], gp$note_labels[lv[2]],
          if (is.null(pair)) sprintf("the first two of %d groups", G)
          else sprintf("2 of %d groups, chosen with smdpair()", G))
}

# Automatic notes are separate paragraphs in every sink (Milestone P W02).
.t1_join_note <- function(footnote, note) .tt_append_footnotes(footnote, note)

# smdpair (desctab.ado:592-662, tabtools 2.4.0): the two by() groups the pair
# SMD compares, as group indices. A token names a group by value (a number,
# numeric by() only) or by label (a string by()'s values, or a numeric by()'s
# value-label text; an unlabelled value's label is the value itself). A token
# naming one group by value and a DIFFERENT group by label is refused unless
# `as` is "values" or "labels" (Stata's smdpair(..., values|labels)). Each
# token must name exactly one group, and the two must differ. A logical by()
# takes TRUE/FALSE (or "TRUE"/"FALSE"), read as 1/0.
.t1_resolve_smdpair <- function(smdpair, gp, by_numeric, as = "auto", by_logical = FALSE) {
  as <- match.arg(as, c("auto", "values", "labels"))
  ok_type <- is.character(smdpair) || is.numeric(smdpair) || (is.logical(smdpair) && by_logical)
  if (!ok_type || length(smdpair) != 2L || anyNA(smdpair)) {
    cli::cli_abort("{.arg smdpair} must name exactly two groups of {.arg by}.",
                   class = "tabtools_error_smdpair", call = NULL)
  }
  if (by_logical) {
    smdpair <- vapply(as.list(smdpair), function(t) {
      if (is.logical(t)) return(as.character(as.integer(t)))
      u <- toupper(trimws(as.character(t)))
      if (u == "TRUE") "1" else if (u == "FALSE") "0" else as.character(t)
    }, "")
  }
  out <- vapply(seq_len(2L), function(i) {
    tok <- as.character(smdpair[[i]])
    num <- suppressWarnings(as.numeric(tok))
    vhit <- if (by_numeric && !is.na(num) && as != "labels") which(gp$codes == num) else integer()
    lhit <- if (as != "values" || !by_numeric) which(gp$note_labels == tok) else integer()
    if (length(vhit) && length(lhit) && !identical(vhit, lhit)) {
      vlab <- gp$note_labels[vhit[1]]
      lval <- gp$codes[lhit[1]]
      cli::cli_abort(c(
        "{.arg smdpair}: {.val {tok}} is ambiguous: it is the value of the group labelled {.val {vlab}} and the label of the group with value {lval}.",
        "i" = "Set {.code smdpair_as = \"values\"} or {.code smdpair_as = \"labels\"} to say which is meant."),
        class = c("tabtools_error_smdpair_ambiguous", "tabtools_error_smdpair"), call = NULL)
    }
    hit <- union(vhit, lhit)
    if (length(hit) != 1L) {
      cli::cli_abort("{.arg smdpair}: {.val {tok}} does not name exactly one group of {.arg by}.",
                     class = "tabtools_error_smdpair", call = NULL)
    }
    hit
  }, 1L)
  if (out[1] == out[2]) {
    cli::cli_abort("{.arg smdpair} must name two different groups.",
                   class = "tabtools_error_smdpair", call = NULL)
  }
  out
}
