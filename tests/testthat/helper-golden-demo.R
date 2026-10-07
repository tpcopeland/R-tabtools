# Parity of the R demo (qa/demo/demo_tabtools.R) with the Stata demo
# (tabtools/demo/demo_tabtools.do), IMPLEMENTATION_PLAN.md Milestone D,
# task D3. The Stata artefacts come from qa/stata/run_demo.do on a `git
# archive` export of the golden baseline (golden/demo/SOURCE.csv);
# golden/demo/manifest.csv lists every Stata sheet, console block and report
# table, with the golden each ported one is compared with (a scenario golden
# when the Stata demo sheet is identical to it, else the Stata demo workbook
# itself) and the reason for each one that is not ported.

golden_demo_path <- function(...) golden_path("demo", ...)

# The demo script and its datasets live in the source repository only
# (qa/demo/, .Rbuildignored with golden/demo/: the datasets include Stata's
# auto and union), so the parity test runs from a source checkout.
golden_demo_script <- function(...) test_path("..", "..", "qa", "demo", ...)

golden_demo_manifest <- function() {
  utils::read.csv(golden_demo_path("manifest.csv"), stringsAsFactors = FALSE,
                  encoding = "UTF-8", na.strings = character(), colClasses = "character")
}

golden_demo_source <- function() {
  s <- utils::read.csv(golden_demo_path("SOURCE.csv"), stringsAsFactors = FALSE,
                       encoding = "UTF-8", na.strings = character(), colClasses = "character")
  stats::setNames(s$value, s$key)
}

# Independently bound publication literals: native desctab.ado primary note and
# the reviewed P.7 R ESS boundary. Neither is assembled by production helpers.
golden_demo_primary_notes <- function() list(
  r = paste0("Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: ",
    "no complementary cells are masked). Unmasked cells and ordinary variable tests are shown as computed; ",
    "effective sample size linked to a masked sample count is withheld. This protects printed counts only."),
  native = paste0("Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: ",
    "no complementary cells are masked, and other cells, totals and tests are shown as computed). ",
    "This protects printed counts only."))

# puttab returns its resolved workbook under stored$file; other writers use xlsx.
golden_demo_destination <- function(value, args) {
  value$stored$xlsx %||% value$stored$file %||% args$xlsx
}

# Native source declarations supply literal user annotations; authenticated
# native tails supply automatic notes. Production note assembly is not an oracle.
golden_demo_publication_contract <- function(id, native_spec = NULL) {
  manifest <- golden_demo_manifest()
  row <- manifest[manifest$kind == "sheet" & manifest$golden == id, , drop = FALSE]
  if (nrow(row) != 1L) stop("Unknown native demo publication: ", id, call. = FALSE)
  spec <- if (is.null(native_spec)) golden_demo_golden(id) else native_spec
  grid <- as.matrix(openxlsx2::read_xlsx(spec$book, sheet = spec$sheet,
    col_names = FALSE, skip_empty_rows = FALSE, skip_empty_cols = FALSE))
  grid[is.na(grid)] <- ""
  styles <- golden_cell_styles(spec$book, spec$sheet)
  phase7 <- golden_demo_phase7_publication(row, grid, styles, spec)
  if (!is.null(phase7)) return(phase7)
  user <- if (nzchar(row$user_footnote)) row$user_footnote else NULL
  native_note <- golden_fn_paragraphs(user)
  automatic <- list()
  native_tail <- styles$value[match(paste0("B", max(styles$row)), styles$address)]
  if (row$command %in% c("table1_tc", "desctab") && (startsWith(native_tail, "Counts below ") || startsWith(native_tail, "Counts from "))) {
    text <- native_tail
    if (identical(id, "demo/demo_table1.xlsx:Small Cells Primary Mode")) {
      notes <- golden_demo_primary_notes()
      if (!identical(native_tail, notes$native)) stop("Native primary footer differs from the declared full literal.", call. = FALSE)
      text <- notes$r
    }
    automatic <- list(list(text = text, native = native_tail,
      source = paste("authenticated native demo tail and reviewed P.7 publication boundary", id)))
    native_note <- native_tail
  }
  call <- paste0(row$command, "(footnote = ", paste(deparse(user), collapse = ""), ")")
  contract <- golden_footnote_contract(id, automatic = automatic,
    native_sources = list(csv = as.vector(grid), xlsx = styles$value),
    scenario = list(command = row$command, r_call = call, stata_call = row$native_command))
  if (length(contract$stars)) {
    native_note <- if (!length(native_note)) contract$stars else {
      last <- tail(native_note, 1L)
      join <- if (grepl("[.;:!?]$", last)) " " else "; "
      c(head(native_note, -1L), paste0(last, join, contract$stars))
    }
  }
  contract$command <- row$command
  contract$native_footers <- list(csv = native_note, markdown = golden_fn_md(native_note),
    console = if (length(automatic) && row$command == "table1_tc") native_note else character(), xlsx = native_note)
  contract$native_grid <- grid
  contract$native_styles <- styles
  contract$native_layout <- golden_sheet_layout(spec$book, spec$sheet)
  contract$grid_end <- nrow(grid) - length(native_note)
  contract$sheet_end <- max(styles$row) - length(native_note)
  contract
}

golden_demo_packages <- c("haven", "survival", "lme4", "geepack", "MASS", "nnet", "pscl",
                          "WeightIt", "marginaleffects", "tidyxl", "openxlsx2")

# Run the R demo once per session into a temporary directory, recording
# every table the tabtools commands return, keyed by "<workbook> <sheet>"
# (or "<markdown file> <function>" for a Markdown-only call). The recorders shadow
# the exported functions in the environment the script runs in; the script
# itself is unchanged, and its console log still names the real calls.
golden_demo_state <- new.env(parent = emptyenv())

golden_demo_run <- function() {
  if (!is.null(golden_demo_state$run)) return(golden_demo_state$run)
  script <- golden_demo_script("demo_tabtools.R")
  if (!file.exists(script)) stop("qa/demo/demo_tabtools.R not found", call. = FALSE)
  out <- tempfile("tabtools-demo-")
  dir.create(out)
  tables <- new.env(parent = emptyenv())
  leaves <- new.env(parent = emptyenv())
  env <- new.env(parent = globalenv())
  ns <- asNamespace("tabtools")
  sink_state <- get(".tt_sink_state", ns)
  old_sinks <- as.list(sink_state)
  old_options <- options()
  on.exit({
    added <- setdiff(names(options()), names(old_options))
    if (length(added)) options(stats::setNames(rep(list(NULL), length(added)), added))
    options(old_options)
    for (key in names(old_sinks)) assign(key, old_sinks[[key]], sink_state)
  }, add = TRUE)
  for (fn in c("table1_tc", "desctab", "regtab", "puttab", "stacktab", "stratetab", "effecttab",
              "comptab", "hrcomptab", "ratetab", "outtab", "crosstab", "corrtab", "survtab")) {
    local({
      name <- fn
      f <- getExportedValue("tabtools", name)
      assign(fn, function(...) {
        res <- withVisible(f(...))
        a <- list(...)
        destination <- golden_demo_destination(res$value, a)
        sheet <- res$value$stored$sheet %||% a$sheet
        markdown <- res$value$stored$markdown %||% a$markdown
        key <- if (!is.null(destination)) {
          paste(basename(destination), sheet)
        } else if (!is.null(markdown)) {
          paste(basename(markdown), name)
        }
        if (!is.null(key)) assign(key, res$value, envir = tables)
        if (res$visible) res$value else invisible(res$value)
      }, envir = env)
    })
  }
  # Cells are scalar/vector publication leaves, never fabricated tt_tables.
  leaf_function <- getExportedValue("tabtools", "tabcell")
  env$tabcell <- function(...) {
    result <- withVisible(leaf_function(...))
    key <- sprintf("L%02d", length(ls(leaves, all.names = TRUE)) + 1L)
    assign(key, result$value, envir = leaves)
    if (result$visible) result$value else invisible(result$value)
  }
  env$out_dir <- out
  env$demo_data_file <- normalizePath(golden_demo_script("demo_tabtools.rds"))
  printed <- utils::capture.output(
    suppressMessages(sys.source(script, envir = env, keep.source = FALSE)))
  golden_demo_state$run <- list(out_dir = out, tables = as.list(tables), leaves = as.list(leaves), printed = printed)
  golden_demo_state$run
}

# "table1_tc.xlsx:T30a" -> the workbook path and sheet of a manifest golden;
# "demo/demo_desctab.xlsx:Events" is a Stata demo workbook in golden/demo/.
golden_demo_golden <- function(spec) {
  at <- regexpr(":", spec, fixed = TRUE)
  list(book = do.call(golden_path, as.list(strsplit(substr(spec, 1L, at - 1L), "/", fixed = TRUE)[[1]])),
       sheet = substring(spec, at + 1L))
}

# ---------------------------------------------------------------------------
# Console log and its Markdown rendering

# A log split into command blocks: key (the command's first line, Stata's
# `noisily ` and `///` dropped, or the R function called), and the lines it
# printed. Stata: a block starts at an echoed `. noisily ...` or `. tabtools
# ...` command (its `> ` continuation lines follow the echo); every other
# echoed command (`. log off demo`, `. keep in 1/6`, comments) ends it.
# R: console() writes each call as `> ` + `+ ` lines, then its output.
golden_demo_stata_key <- function(l) {
  k <- sub("^\\. ", "", l)
  k <- sub("^noisily ", "", k)
  trimws(sub("\\s*///\\s*$", "", k))
}

golden_demo_log_blocks <- function(lines, side = c("stata", "r")) {
  side <- match.arg(side)
  blocks <- list()
  cur <- NULL
  in_echo <- FALSE
  flush <- function() if (!is.null(cur)) blocks[[length(blocks) + 1L]] <<- cur
  for (l in lines) {
    if (side == "stata") {
      if (startsWith(l, ". ")) {
        flush()
        cur <- NULL
        in_echo <- TRUE
        if (grepl("^\\. (noisily |tabtools)", l)) cur <- list(key = golden_demo_stata_key(l), lines = character())
        next
      }
      if (in_echo && startsWith(l, "> ")) next
    } else {
      if (startsWith(l, "> ")) {
        flush()
        cur <- list(key = sub("\\(.*$", "", substring(l, 3L)), lines = character())
        if (cur$key == "print") cur$key <- sub("^print\\(([^(]+)\\(.*$", "\\1", substring(l, 3L))
        in_echo <- TRUE
        next
      }
      if (in_echo && startsWith(l, "+ ")) next
    }
    in_echo <- FALSE
    if (!is.null(cur)) cur$lines <- c(cur$lines, l)
  }
  flush()
  blocks
}

# The Markdown rendering: Stata's logdoc writes each command as a ```stata
# fence and its output as the ``` fence after it; console() writes ```r and
# ``` fences the same way.
golden_demo_md_blocks <- function(lines, side = c("stata", "r")) {
  side <- match.arg(side)
  open <- if (side == "stata") "```stata" else "```r"
  blocks <- list()
  i <- 1L
  n <- length(lines)
  while (i <= n) {
    if (lines[i] == open) {
      j <- i + 1L
      while (j <= n && lines[j] != "```") j <- j + 1L
      code <- lines[seq_len(j - i - 1L) + i]
      i <- j + 1L
      while (i <= n && !nzchar(lines[i])) i <- i + 1L
      out <- character()
      if (i <= n && lines[i] == "```") {
        j <- i + 1L
        while (j <= n && lines[j] != "```") j <- j + 1L
        out <- lines[seq_len(j - i - 1L) + i]
        i <- j + 1L
      }
      key <- if (side == "stata") {
        if (!grepl("^\\. (noisily |tabtools)", code[1])) next
        golden_demo_stata_key(code[1])
      } else {
        k <- sub("\\(.*$", "", code[1])
        if (k == "print") k <- sub("^print\\(([^(]+)\\(.*$", "\\1", code[1])
        k
      }
      blocks[[length(blocks) + 1L]] <- list(key = key, lines = out)
      next
    }
    i <- i + 1L
  }
  blocks
}

# The comparable lines of a block: golden_console_box() (Stata's wrapped
# lines rejoined, the chatter before a listing and the sink messages
# dropped), with each side's output directory removed from file paths
# (forward slashes; R's temporary directory is normalised on Windows too).
golden_demo_console_lines <- function(lines, dirs) {
  golden_demo_strip_dirs(golden_console_box(lines), dirs)
}

# C28 has a deliberate full paragraph difference. Assert every annotation on
# both sides before removing only the boundary after the final closing box.
golden_demo_primary_console <- function(got, want) {
  contract <- golden_demo_publication_contract("demo/demo_table1.xlsx:Small Cells Primary Mode")
  split <- function(lines) {
    edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
    if (length(edge) < 2L) stop("Primary demo footer requires a complete boxed listing.", call. = FALSE)
    end <- tail(edge, 1L)
    foot <- lines[golden_footer_rows(length(lines), end)]
    list(body = lines[seq_len(end)], tail = foot[nzchar(trimws(foot))])
  }
  g <- split(got); w <- split(want)
  golden_assert_footnote_tail(g$tail, contract, "console")
  golden_assert_native_footnote_tail(w$tail, contract, "console")
  list(got = g$body, want = w$body)
}

golden_demo_strip_dirs <- function(out, dirs) {
  for (d in unique(c(dirs, gsub("\\", "/", dirs, fixed = TRUE)))) {
    out <- gsub(paste0(d, "/"), "", out, fixed = TRUE)
  }
  out
}

# The Markdown rendering of a console block: both sides right-trimmed (the
# converters differ in trailing blanks only), each side's output
# directories removed. Up to logdoc 1.1.7 Stata's console_output.md split
# or trimmed lines the log wrapped at its linesize, and the comparison
# accepted those renderings a pinned number of times per block (manifest
# column md_pin; ~/Stata-Dev/_take_action/2026-09-26-logdoc-wrapped-output.md).
# logdoc 1.1.8 (Stata-Tools 1255176d, the 2.1.14 goldens; task C8) keeps
# wrapped lines whole in their output fence, so the lines must match as
# they are.
golden_demo_md_compare <- function(got, want, mask, got_dirs, want_dirs) {
  rt <- function(x) sub("\\s+$", "", x)
  got <- rt(golden_demo_console_lines(got, got_dirs))
  want <- rt(golden_demo_console_lines(want, want_dirs))
  golden_compare_console(got, want, mask)
}

# ---------------------------------------------------------------------------
# Markdown report

# The report split at its "### " headings: one element per table, named by
# the heading, holding its lines up to the next heading.
golden_demo_report_tables <- function(lines) {
  at <- which(startsWith(lines, "### "))
  if (!length(at)) return(list())
  ends <- c(at[-1L] - 1L, length(lines))
  stats::setNames(lapply(seq_along(at), function(k) lines[at[k]:ends[k]]), sub("^### ", "", lines[at]))
}

# Independent complete literals: pinned corrtab/crosstab/desctab annotations,
# and the reviewed R-only survtab reverse publication boundary. No production
# note helper or observed R paragraph supplies expected text.
golden_demo_phase7_notes <- function(command, item) {
  empty <- list(R = character(), native = character())
  if (command == "corrtab" && item != "Correlation Spear") {
    note <- "* p<0.05, ** p<0.01, *** p<0.001"
    return(list(R = note, native = note))
  }
  if (command == "crosstab" && item %in% c("Small Cells Primary", "Small Cells Complement")) {
    native <- "Counts below 5 are shown as <5; complementary cells are shown as ≥5 to prevent exact reconstruction."
    return(list(native = native, R = paste0(native,
      " Percentages are withheld when their count or selected denominator is suppressed. ",
      "Tests and association estimates are withheld when primary counts are protected.")))
  }
  if (command == "crosstab" && item == "Small Cells Primary Mode") {
    return(list(native = paste0("Counts from 1 to 4 are shown as <5 without a percentage (primary suppression only: ",
      "no complementary cells are masked, and totals and tests are shown as computed). This protects printed counts only."),
      R = paste0("Counts from 1 to 4 are shown as <5. Dependent percentages are withheld. ",
        "Primary protection masks no complementary counts; released totals and computed tests or association estimates may permit reconstruction.")))
  }
  if (command == "desctab" && item == "Small Cells Primary Mode") {
    notes <- golden_demo_primary_notes()
    return(list(R = notes$r, native = notes$native))
  }
  if (command == "survtab" && item == "Cumul Incidence") {
    return(list(native = character(), R = paste0("reverse reports 1 - Kaplan-Meier, which equals cumulative incidence only with a single event type ",
      "(no competing risks). With competing events, use a competing-risks estimator (Aalen-Johansen).")))
  }
  empty
}

golden_demo_phase7_publication <- function(row, grid, styles, spec) {
  if (!row$command %in% c("corrtab", "crosstab", "survtab") &&
      !(row$command == "desctab" && row$item == "Small Cells Primary Mode")) return(NULL)
  notes <- golden_demo_phase7_notes(row$command, row$item)
  user <- golden_fn_paragraphs(if (nzchar(row$user_footnote)) row$user_footnote else NULL)
  paragraphs <- if (row$command == "corrtab") c(notes$R, user) else c(user, notes$R)
  native <- if (row$command == "corrtab") c(notes$native, user) else c(user, notes$native)
  end <- max(styles$row) - length(native)
  actual <- if (length(native)) styles$value[match(paste0("B", seq.int(end + 1L,
    length.out = length(native))), styles$address)] else character()
  if (!identical(unname(actual), native)) stop("Native demo footer differs from complete independently declared paragraphs.")
  reverse <- row$command == "survtab" && row$item == "Cumul Incidence"
  if (reverse && (any(grepl("reverse reports", grid, fixed = TRUE)) ||
      !identical(dim(grid), c(8L, 5L)))) stop("Native reverse demo must have its exact eight-row grid and no warning footer.")
  list(paragraphs = paragraphs, user_paragraphs = user, stars = character(),
    provenance = "Independent full demo literals; native pin712044f8 and reviewed P7 R publication boundary",
    native_footers = list(csv = native, markdown = golden_fn_md(native), console = native, xlsx = native),
    command = row$command, native_grid = grid, native_styles = styles,
    native_layout = golden_sheet_layout(spec$book, spec$sheet),
    grid_end = nrow(grid) - length(native), sheet_end = end, R_only_reverse = reverse)
}

golden_demo_phase7_console <- function(got, want, item) {
  mapping <- list(C18 = c("corrtab", "Correlation"), C52 = c("corrtab", "Correlation"),
    C22 = c("crosstab", "Small Cells Primary"), C25 = c("crosstab", "Small Cells Complement"))
  selected <- mapping[[item]]
  notes <- if (is.null(selected)) list(R = character(), native = character()) else
    golden_demo_phase7_notes(selected[1L], selected[2L])
  if (identical(item, "C41")) {
    # Native ratetab prints its cluster-count diagnostic after the box.
    # R retains diagnostics in the table ledger; require each complete surface.
    notes <- list(R = character(), native = "(ratetab: treated: 6 clusters of region)")
  }
  split <- function(lines) {
    edge <- which(grepl("^\\s*\\+-+\\+\\s*$", lines))
    if (length(edge) < 2L) stop("New demo console boundary requires its complete boxed listing.")
    end <- tail(edge, 1L)
    tail <- lines[golden_footer_rows(length(lines), end)]
    list(body = lines[seq_len(end)], tail = tail[nzchar(trimws(tail))])
  }
  g <- split(got); w <- split(want)
  testthat::expect_identical(unname(g$tail), notes$R, info = paste(item, "complete R annotations"))
  testthat::expect_identical(unname(w$tail), notes$native, info = paste(item, "complete native annotations"))
  list(got = g$body, want = w$body)
}

golden_demo_leaf_contract <- function(item) {
  ids <- sprintf("C%02d", 33:39)
  i <- match(item, ids)
  if (is.na(i)) stop("Unknown demo scalar leaf identity.")
  list(text = c("1.09 (1.02, 1.17)", "1.091 (1.017 to 1.171)", "1.10 (1.00, 1.22)",
    "<0.001", "<5", "2,149/6,066 (35.4)", "3,452 (2,032, 5,018)")[i],
    form = c("est", "est", "est", "p", "np", "enp", "iqr")[i],
    source = c("model", "model", "contrast", rep("explicit", 4L))[i],
    native_source = c("e()", "e()", "lincom", rep(NA_character_, 4L))[i],
    key = sprintf("L%02d", i))
}

golden_demo_leaf_console <- function(got, want, item, leaves) {
  c <- golden_demo_leaf_contract(item)
  leaf <- leaves[[c$key]]
  testthat::expect_s3_class(leaf, "tt_cell")
  testthat::expect_identical(as.character(leaf), c$text, info = item)
  p <- attr(leaf, "provenance", exact = TRUE)
  testthat::expect_identical(p$rows$form, c$form, info = item)
  testthat::expect_identical(p$rows$source, c$source, info = item)
  testthat::expect_identical(p$rows$native_source, c$native_source, info = item)
  # R's public character print wraps the same cell in [1] and quotes. Assert
  # that full wrapper before comparing the actual publication value itself.
  testthat::expect_identical(got[nzchar(trimws(got))], paste0("[1] ", encodeString(c$text, quote = '"')), info = item)
  testthat::expect_identical(want[nzchar(trimws(want))], c$text, info = item)
  list(got = as.character(leaf), want = want[nzchar(trimws(want))])
}
