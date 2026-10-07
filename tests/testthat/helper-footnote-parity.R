# R-contract oracle for W02, not a Stata-generated expected artifact.
# Annotation constants come from scenario inputs. Additional R paragraphs
# require literal expected text plus fragments verified in authentic native
# fixture text. The bridge must assert declared native tails before removing
# them and compare all remaining table cells/styles strictly.

# Independent scanner: consume exactly one spaced backslash, without using
# package parsing, assembly, rendering or escape helpers.
golden_fn_paragraphs <- function(text) {
  if (is.null(text)) return(character())
  stopifnot(is.character(text), !anyNA(text))
  out <- character()
  token <- paste0(" ", intToUtf8(92L), " ")
  for (s in text) {
    split <- grepl(token, s, fixed = TRUE)
    repeat {
      pos <- regexpr(token, s, fixed = TRUE)[1L]
      if (pos < 0L) {
        piece <- if (split) trimws(s, whitespace = "[ ]") else s
        if (nzchar(piece)) out <- c(out, piece)
        break
      }
      piece <- trimws(substr(s, 1L, pos - 1L), whitespace = "[ ]")
      if (nzchar(piece)) out <- c(out, piece)
      s <- substring(s, pos + 3L)
    }
  }
  out
}

golden_fn_constant <- function(expr) {
  if (is.null(expr)) return(NULL)
  if (is.character(expr)) return(expr)
  if (is.call(expr) && identical(expr[[1L]], as.name("c"))) {
    return(unlist(lapply(as.list(expr)[-1L], golden_fn_constant), use.names = FALSE))
  }
  stop("Footnote oracle requires literal character/NULL annotation constants.", call. = FALSE)
}

golden_fn_user <- function(sc) {
  expressions <- as.list(parse(text = sc$r_call))
  # Only the statically returned expression supplies the outer table's
  # annotation. Earlier append/replacement setup calls cannot overwrite it.
  # There is no traversal into argument ASTs (including missing subscripts).
  final_call <- function(expr) {
    if (!is.call(expr)) stop("Footnote oracle requires an identifiable final scenario command.", call. = FALSE)
    head <- expr[[1L]]
    name <- if (is.symbol(head)) as.character(head) else if (is.call(head) &&
      as.character(head[[1L]]) %in% c("::", ":::")) as.character(head[[3L]]) else ""
    if (identical(name, sc$command)) return(expr)
    if (name == "{") return(final_call(expr[[length(expr)]]))
    if (name %in% c("<-", "=")) return(final_call(expr[[3L]]))
    if (name %in% c("suppressMessages", "suppressWarnings", "invisible", "print",
                    "withCallingHandlers", "local")) return(final_call(expr[[2L]]))
    stop("Footnote oracle requires an identifiable final scenario command.", call. = FALSE)
  }
  if (!length(expressions)) stop("Footnote oracle requires a scenario command.", call. = FALSE)
  command <- final_call(expressions[[length(expressions)]])
  args <- as.list(command)[-1L]
  named <- names(args)
  selected <- args[named %in% c("footnote", if (sc$command == "stacktab") "note")]
  if (length(selected) > 1L) stop("Footnote oracle found conflicting annotation inputs.", call. = FALSE)
  if (!length(selected)) return(NULL)
  golden_fn_constant(selected[[1L]])
}

golden_fn_sources <- function(id) {
  files <- paste0(id, c(".csv", ".md", "_console.txt", "_stored.csv"))
  out <- lapply(files, function(f) {
    p <- golden_path(f)
    if (file.exists(p)) golden_read_lines(p) else character()
  })
  names(out) <- files
  if (requireNamespace("tidyxl", quietly = TRUE)) {
    out$xlsx <- golden_cell_styles(golden_book(id), id)$value
  }
  out
}

golden_footnote_contract <- function(id, automatic = list(), native_sources = NULL,
                                     native_footers = list(), scenario = golden_scenario(id)) {
  user <- golden_fn_paragraphs(golden_fn_user(scenario))
  if (is.null(native_sources)) native_sources <- golden_fn_sources(id)
  native <- unlist(native_sources, use.names = FALSE)
  native <- native[!is.na(native)]
  auto <- character()
  provenance <- paste0("explicit ", id, " scenario annotation")
  for (item in automatic) {
    if (!is.list(item) || !all(c("text", "native", "source") %in% names(item)) ||
        !is.character(item$text) || anyNA(item$text) ||
        !is.character(item$native) || length(item$native) != 1L || !nzchar(item$native) ||
        !is.character(item$source) || length(item$source) != 1L || !nzchar(item$source)) {
      stop("Automatic paragraph oracle requires literal text, native fragment and provenance.", call. = FALSE)
    }
    if (!any(grepl(item$native, native, fixed = TRUE))) {
      stop("Automatic paragraph native fragment is absent from authentic fixture sources.", call. = FALSE)
    }
    auto <- c(auto, golden_fn_paragraphs(item$text))
    provenance <- c(provenance, item$source)
  }
  # Actual generated stars are selected by the scenario flag, never a user
  # paragraph prefix. The authoritative legend spelling is read from native
  # workbook text, whose source adds it last (regtab.ado:2762-2783).
  stars <- character()
  flag <- grepl("(^|[ ,])stars([ ]|$)", scenario$stata_call)
  if (flag) {
    native_xlsx <- native_sources$xlsx
    if (!length(native_xlsx)) stop("Star oracle requires authentic native workbook text.", call. = FALSE)
    rx <- "\\* p<[0-9.]+, \\*\\* p<[0-9.]+, \\*\\*\\* p<[0-9.]+"
    found <- unlist(regmatches(native_xlsx[!is.na(native_xlsx)],
                              gregexpr(rx, native_xlsx[!is.na(native_xlsx)])), use.names = FALSE)
    if (!length(found)) stop("Native workbook has no declared star legend.", call. = FALSE)
    stars <- tail(found, 1L)
    provenance <- c(provenance, "native workbook legend; regtab.ado:2762-2783")
  }
  list(paragraphs = c(user, setdiff(auto, user), setdiff(stars, c(user, auto))),
       user_paragraphs = user, stars = stars, provenance = provenance,
       native_footers = native_footers)
}

golden_fn_md <- function(x, dollar = FALSE) {
  # Character-wise punctuation map, independent of .md_escape's gsub chain.
  punctuation <- c("\\", "|", "*", "_", "`", "<", ">", "&", "[", "]", "~", if (dollar) "$")
  vapply(x, function(s) {
    s <- trimws(s, whitespace = "[ ]")
    chars <- strsplit(s, "", fixed = TRUE)[[1L]]
    s <- paste0(ifelse(chars %in% punctuation, paste0("\\", chars), chars), collapse = "")
    s <- gsub("\r\n|\r|\n", "<br>", s)
    paste0("*", s, "*")
  }, "", USE.NAMES = FALSE)
}

golden_assert_footnote_tail <- function(actual, contract,
                                         sink = c("console", "csv", "markdown", "xlsx", "presentation"),
                                         dollar = FALSE) {
  sink <- match.arg(sink)
  expected <- if (sink == "markdown") golden_fn_md(contract$paragraphs, dollar) else contract$paragraphs
  testthat::expect_identical(unname(actual), expected, label = paste(sink, "complete W02 footer"))
  invisible(contract)
}

golden_assert_native_footnote_tail <- function(actual, contract, sink) {
  if (!sink %in% names(contract$native_footers)) {
    stop("Native footer boundary must be explicitly declared before comparison.", call. = FALSE)
  }
  testthat::expect_identical(unname(actual), contract$native_footers[[sink]],
                             label = paste(sink, "complete authentic native footer"))
  invisible(contract)
}

golden_assert_footnote_styles <- function(cells, rows, columns, fontsize, font, halign = "left") {
  anchors <- cells[cells$row %in% rows & cells$col == min(columns), , drop = FALSE]
  anchors <- anchors[order(anchors$row), , drop = FALSE]
  testthat::expect_identical(as.integer(anchors$row), as.integer(rows))
  testthat::expect_true(all(anchors$italic))
  testthat::expect_identical(anchors$size, rep(max(fontsize - 2, 6), length(rows)))
  testthat::expect_identical(anchors$font, rep(font, length(rows)))
  testthat::expect_true(all(anchors$wrap))
  testthat::expect_identical(anchors$halign, rep(halign, length(rows)))
  testthat::expect_true(all(anchors$valign == "center"))
  for (border in c("border_top", "border_bottom", "border_left", "border_right")) {
    testthat::expect_true(all(is.na(anchors[[border]]) | !nzchar(anchors[[border]])))
  }
  extra <- cells[cells$row %in% rows & cells$col %in% setdiff(columns, min(columns)), , drop = FALSE]
  testthat::expect_true(all(is.na(extra$value) | !nzchar(extra$value)))
  invisible(cells)
}
