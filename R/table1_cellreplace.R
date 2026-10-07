# Structured R records, native exact selectors: desctab.ado:1522-1607
# (2.5.1). Unlike native, protected publication targets cannot be overwritten.
.t1_cellreplace_check <- function(x) {
  bad <- function() cli::cli_abort(
    "{.arg cellreplace} must be a list of records with exactly {.val row}, {.val column}, and {.val text}.",
    class = "tabtools_error_cellreplace", call = NULL)
  scalar_text <- function(z) is.character(z) && length(z) == 1L &&
    is.null(dim(z)) && !is.na(z)
  if (is.null(x)) return(list())
  if (!is.list(x) || inherits(x, "data.frame")) bad()
  for (r in x) {
    if (!is.list(r) || inherits(r, "data.frame") || length(r) != 3L ||
        is.null(names(r)) || anyNA(names(r)) || anyDuplicated(names(r)) ||
        !setequal(names(r), c("row", "column", "text"))) bad()
    if (!scalar_text(r$row) || !nzchar(.t1_selector_trim(r$row)) || !scalar_text(r$text)) bad()
    c <- r$column
    if (is.character(c)) {
      if (!scalar_text(c) || !nzchar(.t1_selector_trim(c))) bad()
    } else if (!is.numeric(c) || is.complex(c) || !is.null(dim(c)) || length(c) != 1L ||
               !is.finite(c) || c != floor(c) || c < 1 || c > .Machine$integer.max) bad()
  }
  x
}

.t1_selector_trim <- function(x) trimws(x, whitespace = "[ ]")

.t1_cellreplace_mapping_error <- function() {
  cli::cli_abort("The selected cell has no exact publication-companion mapping.",
                 class = "tabtools_error_cellreplace_companion", call = NULL)
}

# Coordinates in this internal view: descriptor row 1, body rows 2..n+1;
# columns are physical body columns. Numeric public selectors exclude label.
.t1_cellreplace_targets <- function(tt, records) {
  if (!inherits(tt, "tt_table") || !identical(tt$command, "table1_tc")) {
    cli::cli_abort("{.arg cellreplace} supports descriptive Table 1 objects only.",
                   class = "tabtools_error_cellreplace", call = NULL)
  }
  labels <- .t1_selector_trim(c(tt$header[[2L]]$text[1L], tt$body[[1L]]))
  columns <- seq_len(ncol(tt$body))[-1L]
  header <- .t1_selector_trim(tt$header[[1L]]$text[columns])
  alias <- if (is.null(tt$cols$label)) header else .t1_selector_trim(tt$cols$label[columns])
  targets <- vector("list", length(records))
  for (i in seq_along(records)) {
    r <- records[[i]]
    row <- which(labels == .t1_selector_trim(r$row))
    if (length(row) != 1L) {
      cli::cli_abort("{.arg cellreplace} row {.val {r$row}} must match exactly one row.",
                     class = "tabtools_error_cellreplace_selector", call = NULL)
    }
    if (is.numeric(r$column)) {
      if (r$column > length(columns)) {
        cli::cli_abort("{.arg cellreplace} column is outside the table's value columns.",
                       class = "tabtools_error_cellreplace_selector", call = NULL)
      }
      column <- columns[as.integer(r$column)]
    } else {
      value <- .t1_selector_trim(r$column)
      hit <- which(header == value | alias == value)
      if (length(hit) != 1L) {
        cli::cli_abort("{.arg cellreplace} header {.val {r$column}} must match exactly one column.",
                       class = "tabtools_error_cellreplace_selector", call = NULL)
      }
      column <- columns[hit]
    }
    if (.t1_cellreplace_protected(tt, row, column)) {
      cli::cli_abort("{.arg cellreplace} cannot replace a protected publication cell or withheld component.",
                     class = "tabtools_error_cellreplace_protected", call = NULL)
    }
    targets[[i]] <- list(row = row, column = column, text = r$text)
  }
  targets
}

.t1_cellreplace_protected <- function(tt, row, column) {
  if (tt$stored$smallcells$threshold == 0L) return(FALSE)
  k <- .t1_col_k(tt)[column]
  body_row <- row - 1L
  if (is.na(k)) {
    return(body_row > 0L && tt$cols$role[column] %in% c("p", "smd", "test", "statistic") &&
             isTRUE(tt$meta$derived_rows[body_row]))
  }
  crude <- !is.null(tt$cols$pass) && identical(tt$cols$pass[column], "crude")
  codes <- if (crude) tt$meta$crude_codes else tt$meta$row_codes
  linked <- if (crude) tt$meta$crude_linked_cells else tt$meta$linked_cells
  if (body_row == 0L) {
    sample <- if (crude) tt$meta$crude_sample_codes else tt$meta$sample_codes
    header_linked <- if (crude) tt$meta$crude_header_linked else tt$meta$header_linked
    return(sample[k] > 0L || isTRUE(header_linked[k]))
  }
  if (length(codes) != nrow(tt$body) || length(linked) != nrow(tt$body) ||
      length(codes[[body_row]]) < k || length(linked[[body_row]]) < k) {
    .t1_cellreplace_mapping_error()
  }
  codes[[body_row]][k] > 0L || isTRUE(linked[[body_row]][k])
}

.t1_cellreplace_ledger <- function(tt, ids, metrics = NULL, exclusions = FALSE) {
  ledger <- tt$meta$sample_accounting
  if (is.null(ledger) || !all(ids %in% ledger$populations$id)) .t1_cellreplace_mapping_error()
  m <- ledger$measures
  hit <- m$population_id %in% ids & m$status == "available"
  if (!is.null(metrics)) hit <- hit & m$metric %in% metrics
  m$value[hit] <- NA_real_
  m$status[hit] <- "unavailable"
  m$reason[hit] <- "replaced_by_cellreplace"
  ledger$measures <- m
  if (exclusions) {
    e <- ledger$exclusions
    hit <- e$population_id %in% ids & e$status == "available"
    e$n[hit] <- NA_real_
    e$status[hit] <- "unavailable"
    e$basis[hit] <- paste0(e$basis[hit], "; publication unavailable after cellreplace")
    ledger$exclusions <- e
  }
  .tt_validate_sample_accounting(ledger)
  tt$meta$sample_accounting <- ledger
  tt
}

.t1_cellreplace_invalidate <- function(tt, target, gp) {
  row <- target$row - 1L
  column <- target$column
  role <- tt$cols$role[column]
  k <- .t1_col_k(tt)[column]
  pass <- if (is.null(tt$cols$pass)) "" else tt$cols$pass[column]
  prefix <- if (!is.na(pass) && nzchar(pass)) paste0(pass, "/") else ""
  # With no by(), the builder names its sole g1 value column Total. Explicit
  # by() requires >=2 groups, so G==1 is the published aggregate, not group/1.
  scope <- if (!is.na(k)) if (gp$G == 1L || k > gp$G) "table" else paste0("group/", k) else NULL
  if (row == 0L) {
    if (!is.null(scope)) tt <- .t1_cellreplace_ledger(tt, paste0(prefix, scope), "reported_n")
    return(tt)
  }
  if (identical(tt$rows$type[row], "ess")) {
    if (!is.null(scope) && !identical(pass, "crude")) {
      tt <- .t1_cellreplace_ledger(tt, paste0(prefix, scope), "effective_n")
    }
    return(tt)
  }
  spec <- tt$meta$cellreplace_spec[row]
  owner <- which(tt$meta$cellreplace_spec == spec & tt$rows$type %in% c("var", "cat_header"))
  if (length(spec) != 1L || is.na(spec) || length(owner) != 1L) .t1_cellreplace_mapping_error()
  fields <- if (role == "p") "p" else if (role == "smd") "smd" else c("p", "smd")
  tr <- tt$rows$table_row[owner]
  for (field in fields) {
    tt$rows[[field]][owner] <- NA_real_
    stored_field <- if (field == "p") "p_value" else "smd"
    if (!is.null(tt$stored$table) && !is.na(tr) && stored_field %in% colnames(tt$stored$table)) {
      tt$stored$table[tr, stored_field] <- NA_real_
    }
  }
  if (!is.null(scope)) {
    ids <- paste0(prefix, "variable/", spec, "/", unique(c(scope, "table")))
    tt <- .t1_cellreplace_ledger(tt, ids, exclusions = TRUE)
  }
  tt
}

.t1_cellreplace <- function(tt, records, gp) {
  if (!length(records)) return(tt)
  targets <- .t1_cellreplace_targets(tt, records)
  # Validate each companion path against a local copy before applying text.
  # A later invalid/protected selection never exports a partial replacement.
  for (target in targets) tt <- .t1_cellreplace_invalidate(tt, target, gp)
  for (target in targets) {
    if (target$row == 1L) tt$header[[2L]]$text[target$column] <- target$text else
      tt$body[target$row - 1L, target$column] <- target$text
  }
  tt$stored$n_cellreplace <- as.integer(length(targets))
  tt$meta$cellreplace <- targets
  validate_tt_table(tt)
  tt
}
