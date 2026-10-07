# Explicit historical cases retain their native bytes and independent lineage.
# Current headers/notes differ; every remaining CSV and console body cell stays
# compared. No current table is rewritten into a legacy table.
p3_2114_publication <- function(tt, want, native_console, id) {
  pairs <- list(W6 = c("Primary", "Secondary", "Tertiary"), S18 = c("Control", "Low", "High"))
  groups <- pairs[[id]]
  if (is.null(groups)) stop("Unknown named 2.1.14 SMD backcompat case.", call. = FALSE)
  pair <- paste(groups[1:2], collapse = " vs ")
  note <- paste0("SMD compares ", pair, " only (the first two of 3 groups).")
  current <- paste0("SMD (", pair, ")")
  testthat::expect_identical(tt$meta$console_before, paste("Note:", note))
  testthat::expect_identical(golden_fn_paragraphs(tt$footnote), note)
  testthat::expect_identical(native_console[1L], paste0("Note: SMD computed for first two groups only (", pair, ")"))
  got <- golden_as_cells(tt)
  testthat::expect_identical(got[1L, ncol(got)], current)
  testthat::expect_identical(want[1L, ncol(want)], "SMD")
  testthat::expect_identical(got[1L, -ncol(got)], want[1L, -ncol(want)])
  testthat::expect_identical(got[nrow(got), 1L], note)
  testthat::expect_true(all(got[nrow(got), -1L] == ""))
  testthat::expect_identical(nrow(got), nrow(want) + 1L)
  testthat::expect_identical(ncol(got), ncol(want))
  # Parse the independently known column starts, retaining label indentation.
  # Only the declared SMD header/automatic note and resulting display widths
  # differ across versions; rules/order and all displayed body values survive.
  console_body <- function(lines, smd_header, foot) {
    lines <- golden_console_box(lines)
    rules <- which(golden_is_rule_line(lines))
    testthat::expect_true(length(rules) >= 2L)
    testthat::expect_identical(lines[golden_footer_rows(length(lines), tail(rules, 1L))], foot)
    box <- lines[seq.int(rules[1L], tail(rules, 1L))]
    header <- box[2L]
    labels <- c(if (id == "S18") "Total", groups, if (id == "S18") "p-value", smd_header)
    positions <- vapply(labels, function(label) regexpr(label, header, fixed = TRUE)[1L], 1L)
    testthat::expect_true(all(positions > 0L))
    starts <- c(regexpr("|", header, fixed = TRUE)[1L] + 2L, positions)
    ends <- c(starts[-1L] - 1L, max(gregexpr("|", header, fixed = TRUE)[[1L]]) - 1L)
    testthat::expect_identical(trimws(substring(header, starts[-1L], ends[-1L])), labels)
    rows <- box[!golden_is_rule_line(box)][-1L]
    values <- t(vapply(rows, function(row) {
      cells <- substring(row, starts, ends)
      cells[1L] <- sub("\\s+$", "", cells[1L])
      cells[-1L] <- trimws(cells[-1L])
      cells
    }, character(length(starts))))
    list(cells = values, rules = which(golden_is_rule_line(box)))
  }
  gc <- console_body(utils::capture.output(print(tt)), current, note)
  wc <- console_body(native_console, "SMD", character())
  testthat::expect_identical(gc$rules, wc$rules)
  # Both distinct SMD headers have already been asserted literally. This common
  # classifier row retains the existing p mask without substituting either
  # version's SMD text for the other; all body SMD cells remain unmasked.
  classifier <- want[1L, ]
  classifier[length(classifier)] <- ""
  list(got = rbind(classifier, got[seq.int(2L, nrow(got) - 1L), , drop = FALSE]),
       want = rbind(classifier, want[-1L, , drop = FALSE]),
       got_console = rbind(classifier, gc$cells), want_console = rbind(classifier, wc$cells))
}
