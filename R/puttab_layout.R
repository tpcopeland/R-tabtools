# puttab layout contracts from Stata 2.5.1: puttab.ado:328-643,
# :800-966, :1110-1164 and :1322-1407. Row roles are stored explicitly.

.puttab_abort <- function(message) {
  cli::cli_abort(message, class = "tabtools_error_layout", call = NULL)
}

.puttab_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .puttab_abort(paste0(arg, " must be TRUE or FALSE."))
  }
}

.puttab_coordinates <- function(x, n, arg) {
  if (is.null(x)) return(integer())
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x != round(x)) || any(x < 1 | x > n)) {
    .puttab_abort(paste0(arg, " coordinates must be whole numbers inside the table (1 to ", n, ")."))
  }
  unique(as.integer(x))
}

.puttab_nformat <- function(x) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    .puttab_abort("nformat must be one Stata %f or %g numeric format.")
  }
  x <- trimws(x)
  m <- regmatches(x, regexec("^%(-?)0?[0-9]*[.]([0-9]+)([fg]c?)$", x))[[1]]
  if (!length(m) || !is.finite(as.double(m[3])) || as.double(m[3]) >= 32) {
    .puttab_abort("nformat must be a Stata %f or %g numeric format with fewer than 32 decimals, such as %12.0fc.")
  }
  paste0("%", m[2], "32.", m[3], m[4])
}

# Parse quoted literal cells without evaluating R/Stata macro-looking text.
# Plain double quotes and Stata compound quotes both keep their contents.
.puttab_quoted <- function(s) {
  rest <- trimws(s)
  out <- character()
  while (nzchar(rest)) {
    compound <- startsWith(rest, "`\"")
    if (!compound && !startsWith(rest, "\"")) .puttab_abort("Expected quoted literal text.")
    opener <- if (compound) 2L else 1L
    closer <- if (compound) "\"'" else "\""
    tail <- substring(rest, opener + 1L)
    pos <- regexpr(closer, tail, fixed = TRUE)[1L]
    if (pos < 1L) .puttab_abort("Unmatched quote in literal text.")
    out <- c(out, substr(tail, 1L, pos - 1L))
    rest <- trimws(substring(tail, pos + nchar(closer)))
  }
  out
}

.puttab_panel_spec <- function(x, panel, panelheader, panelinline, noindent) {
  if (is.null(panel)) {
    if (!is.null(panelheader) || panelinline || noindent) {
      .puttab_abort("panelheader, panelinline and noindent require panel.")
    }
    return(NULL)
  }
  if (!is.data.frame(x) || inherits(x, "tt_table")) .puttab_abort("panel requires a data frame source.")
  if (!is.character(panel) || length(panel) != 1L || is.na(panel) ||
      sum(names(x) == panel) != 1L) .puttab_abort("panel must name one unambiguous source column.")
  if (is.list(x[[panel]]) || is.matrix(x[[panel]])) .puttab_abort("panel must be an ordinary source column.")
  if (panelinline && is.null(panelheader)) .puttab_abort("panelinline requires panelheader.")
  cols <- character()
  literal <- NULL
  if (!is.null(panelheader)) {
    if (is.list(panelheader) && identical(names(panelheader), "text")) {
      literal <- panelheader$text
      if (is.null(literal)) .puttab_abort("Literal panelheader cells cannot be NULL.")
    } else if (is.character(panelheader) && length(panelheader) == 1L &&
               !is.na(panelheader) && grepl("^(`\"|\")", trimws(panelheader))) {
      literal <- .puttab_quoted(panelheader)
    } else {
      if (!is.character(panelheader) || !length(panelheader) || anyNA(panelheader)) {
        .puttab_abort("panelheader must name string columns or give list(text = literal_cells).")
      }
      cols <- if (length(panelheader) == 1L && !panelheader %in% names(x))
        strsplit(trimws(panelheader), "[[:space:]]+")[[1L]] else panelheader
      if (anyDuplicated(cols) || panel %in% cols ||
          any(vapply(cols, function(v) sum(names(x) == v) != 1L, TRUE))) {
        .puttab_abort("panelheader must name distinct, unambiguous source columns and exclude panel.")
      }
      if (any(!vapply(cols, function(v) is.character(x[[v]]) && !is.matrix(x[[v]]), TRUE))) {
        .puttab_abort("panelheader source columns must be character.")
      }
    }
    if (!is.null(literal) && (!is.character(literal) || !length(literal) || anyNA(literal))) {
      .puttab_abort("Literal panelheader cells must be a non-missing character vector.")
    }
  }
  list(panel = panel, columns = cols, literal = literal, exclude = c(panel, cols))
}

.puttab_panelize_source <- function(src, x, spec, inline, noindent, digits) {
  body <- src$body
  n <- nrow(body)
  K <- ncol(body)
  if ((!is.null(spec$literal) && length(spec$literal) != K) ||
      (length(spec$columns) && length(spec$columns) != K)) {
    .puttab_abort(paste0("panelheader must give one string per exported column (", K, ")."))
  }
  original <- .puttab_subset_col(x[[spec$panel]], src$source_rows)
  headings <- .puttab_fmt_col(original, digits, attr(x[[spec$panel]], "format.stata", exact = TRUE))
  # Group by raw values, not rendered labels: equal value labels are distinct panels.
  new <- c(TRUE, !vapply(seq.int(2L, length.out = max(0L, n - 1L)), function(i) {
    .puttab_same_panel(original[i], original[i - 1L])
  }, TRUE))
  ph <- if (!is.null(spec$literal)) matrix(rep(spec$literal, each = n), n, K) else
    if (length(spec$columns)) vapply(spec$columns, function(v) {
      value <- x[[v]][src$source_rows]
      value[is.na(value)] <- ""
      value
    }, character(n)) else NULL
  if (!is.null(ph)) ph <- matrix(ph, n, K)
  out <- matrix("", n * 3L, K)
  r <- 0L
  headed <- FALSE
  hrows <- phrows <- ihrows <- integer()
  for (i in seq_len(n)) {
    if (new[i]) {
      headed <- nzchar(trimws(headings[i]))
      hasph <- !is.null(ph) && any(nzchar(trimws(ph[i, ])))
      if (inline && headed && hasph && nzchar(trimws(ph[i, 1L]))) {
        .puttab_abort("panelinline requires the first panelheader cell to be blank for every headed panel.")
      }
      if (inline && headed && hasph) {
        r <- r + 1L
        out[r, ] <- ph[i, ]
        out[r, 1L] <- trimws(headings[i])
        ihrows <- c(ihrows, r)
      } else {
        if (headed) {
          r <- r + 1L
          out[r, 1L] <- trimws(headings[i])
          hrows <- c(hrows, r)
        }
        if (hasph) {
          r <- r + 1L
          out[r, ] <- ph[i, ]
          phrows <- c(phrows, r)
        }
      }
    }
    r <- r + 1L
    out[r, ] <- body[i, ]
    if (!noindent && headed && nzchar(out[r, 1L])) out[r, 1L] <- paste0("   ", out[r, 1L])
  }
  src$body <- out[seq_len(r), , drop = FALSE]
  src$panels <- list(heading = hrows, header = phrows, inline = ihrows,
                     n_panels = length(hrows) + length(ihrows))
  src
}

.puttab_spans <- function(spec, K, noheader) {
  if (is.null(spec)) return(list())
  if (noheader) .puttab_abort("spanheader requires a header row; remove noheader.")
  if (is.character(spec) && length(spec) == 1L && !is.na(spec)) {
    scan <- .stacktab_scan(spec)
    unquoted <- paste0(scan$ch[!scan$quoted], collapse = "")
    if (startsWith(trimws(unquoted), "\\") || grepl("\\\\[[:space:]]*\\\\", unquoted)) .puttab_abort("spanheader contains an empty entry.")
    pieces <- .stacktab_split(spec)
    if (!length(pieces)) .puttab_abort("spanheader requires at least one span.")
    spec <- lapply(pieces, function(piece) {
      m <- regmatches(piece, regexec("^(.*)[[:space:]]+([0-9]+)(/([0-9]+))?$", piece))[[1L]]
      if (!length(m)) .puttab_abort("spanheader requires quoted text followed by first/last column numbers.")
      text <- .puttab_quoted(m[2])
      if (length(text) != 1L) .puttab_abort("Each spanheader span requires one quoted label.")
      list(text = text, first = as.double(m[3]), last = if (nzchar(m[5])) as.double(m[5]) else as.double(m[3]))
    })
  }
  if (!is.list(spec) || !length(spec)) .puttab_abort("spanheader must be a Stata specification or a list of spans.")
  used <- integer()
  result <- vector("list", length(spec))
  for (i in seq_along(spec)) {
    s <- spec[[i]]
    if (!is.list(s) || !all(c("text", "first") %in% names(s)) ||
        any(!names(s) %in% c("text", "first", "last")) || anyDuplicated(names(s))) {
      .puttab_abort("Each spanheader span needs text, first, and optional last.")
    }
    if (!is.character(s$text) || length(s$text) != 1L || is.na(s$text) || !nzchar(trimws(s$text))) {
      .puttab_abort("spanheader labels must be nonempty literal strings.")
    }
    endpoints <- c(s$first, s$last %||% s$first)
    if (length(endpoints) != 2L) .puttab_abort("spanheader endpoints must be scalar column numbers.")
    .puttab_coordinates(endpoints, K, "spanheader")
    if (endpoints[2] < endpoints[1]) .puttab_abort("spanheader ends before it starts.")
    cols <- seq.int(endpoints[1], endpoints[2])
    if (any(cols %in% used)) .puttab_abort("spanheader spans overlap.")
    used <- c(used, cols)
    result[[i]] <- list(text = s$text, first = as.integer(endpoints[1]), last = as.integer(endpoints[2]))
  }
  result
}

.puttab_layout_rules <- function(x, data_start, last_data) {
  K <- ncol(x$body)
  xK <- K + 1L
  code <- if (identical(x$style$borderstyle, "medium")) 2L else 1L
  rules <- list()
  add <- function(rules, ...) c(rules, list(.rule(...)))
  if (!identical(x$style$borderstyle, "academic")) {
    rules <- add(rules, "left", 2L, last_data, 2L, 2L, code = code)
    rules <- add(rules, "right", 2L, last_data, xK, xK, code = code)
    if (K >= 2L) rules <- add(rules, "right", 2L, last_data, 2L, 2L, code = code)
  }
  for (r in x$meta$puttab_rules$hlines) rules <- add(rules, "top", data_start + r - 1L, data_start + r - 1L, 2L, xK, code = code)
  for (j in x$meta$puttab_rules$vlines) rules <- add(rules, "right", 2L, last_data, j + 1L, j + 1L, code = code)
  for (r in x$meta$puttab_rules$boldrows) rules <- add(rules, "bold", data_start + r - 1L, data_start + r - 1L, 2L, xK, code = 1L)
  panels <- x$meta$puttab_panels
  for (r in union(union(panels$heading, panels$header), panels$inline)) {
    xr <- data_start + r - 1L
    rules <- add(rules, "bold", xr, xr, 2L, xK, code = 1L)
    if (r %in% c(panels$heading, panels$inline)) rules <- add(rules, "top", xr, xr, 2L, xK, code = code)
    if (r %in% c(panels$header, panels$inline)) {
      rules <- add(rules, "bottom", xr, xr, 2L, xK, code = code)
      if (x$style$headershade) rules <- add(rules, "fill", xr, xr, 2L, xK, color = x$style$headercolor)
    }
    if (r %in% panels$heading && K > 1L) rules <- add(rules, "merge", xr, xr, 2L, xK)
  }
  if (length(x$meta$puttab_spans)) {
    rules <- add(rules, "bold", 2L, 2L, 2L, xK, code = 1L)
    rules <- add(rules, "halign", 2L, 2L, 2L, xK, code = 2L)
    if (x$style$headershade) rules <- add(rules, "fill", 2L, 2L, 2L, xK, color = x$style$headercolor)
    for (s in x$meta$puttab_spans) {
      if (s$last > s$first) rules <- add(rules, "merge", 2L, 2L, s$first + 1L, s$last + 1L)
      rules <- add(rules, "bottom", 2L, 2L, s$first + 1L, s$last + 1L, code = 1L)
    }
  }
  rules
}

# A local view for Markdown: no mutation of the table returned to the caller.
.puttab_md_view <- function(x) {
  panels <- x$meta$puttab_panels
  bold <- sort(unique(c(panels$heading, panels$header, panels$inline)))
  if (!length(x$header) && 1L %in% bold) {
    x$header <- list(list(text = as.character(x$body[1L, ])))
    x$body <- x$body[-1L, , drop = FALSE]
    x$rows <- x$rows[-1L, , drop = FALSE]
    bold <- bold[bold > 1L] - 1L
  }
  list(table = x, bold_rows = bold)
}

.puttab_md_header <- function(x) {
  out <- x$header[[length(x$header)]]$text
  for (s in x$meta$puttab_spans) {
    js <- seq.int(s$first, s$last)
    out[js] <- paste0(s$text, ifelse(nzchar(trimws(out[js])), paste0(", ", out[js]), ""))
  }
  out
}

# Ordinary numeric vectors lose label attributes on [, unlike haven vectors.
.puttab_subset_col <- function(x, rows) {
  out <- x[rows]
  attr(out, "labels") <- attr(x, "labels", exact = TRUE)
  out
}

# Raw adjacent values define panels. Display labels never define identity.
.puttab_same_panel <- function(a, b) {
  if (is.numeric(a) && is.numeric(b)) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    if (is.na(a) && is.na(b)) {
      if (requireNamespace("haven", quietly = TRUE)) return(identical(haven::na_tag(a), haven::na_tag(b)))
      return(TRUE)
    }
    return(isTRUE(a == b))
  }
  identical(a, b)
}

# Value labels include tagged missing values; ordinary NA remains unmatched.
.puttab_label_matches <- function(code, lab_codes) {
  hit <- match(code, lab_codes, incomparables = NA)
  missing <- which(is.na(code))
  if (length(missing) && anyNA(lab_codes) && requireNamespace("haven", quietly = TRUE)) {
    hit[missing] <- match(haven::na_tag(code[missing]), haven::na_tag(lab_codes), incomparables = NA)
  }
  hit
}
