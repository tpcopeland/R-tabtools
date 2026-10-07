test_that("replacement is literal, counted per record and affects the publication grid", {
  t1p_local()
  base <- t1p_call(smd = TRUE)
  text <- "quote \" | $value `code` \\path"
  records <- c(t1p_replace("age", "A", "first"), t1p_replace(" age ", " A ", text))
  t <- t1p_call(smd = TRUE, cellreplace = records)
  expect_identical(unname(t$body[5, 2]), text)
  expect_identical(t$stored$n_cellreplace, 2L)
  expect_identical(t$stored$raw$table, base$stored$table)
  expect_identical(t$stored$raw$sample_accounting, base$meta$sample_accounting)
  expect_identical(t$body[, -2], base$body[, -2])
  expect_identical(t$stored$table[c(1, 3), ], base$stored$table[c(1, 3), ])
  expect_true(all(is.na(t$stored$table[2, ])))
  expect_true(is.na(t$rows$p[5]) && is.na(t$rows$smd[5]))
  expect_identical(t$rows$p[-5], base$rows$p[-5])
  expect_identical(t$rows$smd[-5], base$rows$smd[-5])
  expect_null(t$meta$cellreplace_spec)
  expect_false(any(vapply(t$meta$cellreplace, function(z) "old" %in% names(z), logical(1))))
  expect_identical(t1p_call(cellreplace = NULL), t1p_call(cellreplace = list()))
  expect_null(t1p_call(cellreplace = list())$stored$raw)
  expect_error(as_forest_data(t), "must be a table returned")
})

test_that("numeric selectors are value positions and character numbers are exact headers", {
  t1p_local()
  d <- t1p_data()
  d$g <- factor(as.character(d$g), levels = c("A", "B"), labels = c("2", "1"))
  a <- table1_tc(d, by = "g", vars = c(age = "contn"),
                cellreplace = t1p_replace("age", 2L, "POSITION"))
  b <- table1_tc(d, by = "g", vars = c(age = "contn"),
                cellreplace = t1p_replace("age", "2", "HEADER"))
  expect_identical(unname(a$body[1, 3]), "POSITION")
  expect_false(identical(a$body[1, 2], "POSITION"))
  expect_identical(unname(b$body[1, 2]), "HEADER")
  for (total in c("before", "after")) {
    t <- t1p_call(total = total, cellreplace = t1p_replace("age", 1L, "FIRST"))
    expect_identical(unname(t$body[5, 2]), "FIRST")
    expect_identical(t$cols$role[2], if (total == "before") "total" else "group")
  }
  t <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE,
               cellreplace = t1p_replace("age", 3L, "THIRD"))
  expect_identical(unname(t$body[6, 4]), "THIRD")
  expect_identical(t$cols$pass[4], "weighted")
})

test_that("selectors trim spaces, preserve case and refuse ambiguous rows and headers", {
  t1p_local()
  d <- t1p_data()
  attr(d$age, "label") <- "Age"
  expect_identical(unname(t1p_call(d, cellreplace = t1p_replace(" Age ", " A ", "OK"))$body[5, 2]), "OK")
  for (row in c("age", "\tAge", "missing", " ")) {
    cls <- if (row == " ") "tabtools_error_cellreplace" else "tabtools_error_cellreplace_selector"
    expect_error(t1p_call(d, cellreplace = t1p_replace(row, "A")), class = cls)
  }
  expect_error(t1p_call(d, cellreplace = t1p_replace("Age", "a")),
               class = "tabtools_error_cellreplace_selector")
  attr(d$other, "label") <- "Age"
  expect_error(t1p_call(d, cellreplace = t1p_replace("Age", 1L)),
               class = "tabtools_error_cellreplace_selector")
  d$g <- factor(d$g, labels = c("Total", "B"))
  expect_error(t1p_call(d, total = "after", cellreplace = t1p_replace("category", "Total")),
               class = "tabtools_error_cellreplace_selector")
  expect_error(t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE,
                        cellreplace = t1p_replace("age", "A")),
               class = "tabtools_error_cellreplace_selector")
})

test_that("replacement accepts exactly three structured scalar fields", {
  t1p_local()
  invalid <- list("age|A|x", data.frame(row = "age", column = "A", text = "x"),
    list(c(row = "age", column = "A", text = "x")),
    list(list(row = "age", column = "A")),
    list(list(row = "age", column = "A", text = "x", extra = 1)),
    list(stats::setNames(list("age", "A", "x"), c("row", "row", "text"))))
  for (x in invalid) expect_error(t1p_call(cellreplace = x), class = "tabtools_error_cellreplace")
  for (field in c("row", "text")) for (value in list(NULL, NA_character_, c("a", "b"), 1, matrix("a"))) {
    x <- t1p_replace("age", "A")
    x[[1]][field] <- list(value)
    expect_error(t1p_call(cellreplace = x), class = "tabtools_error_cellreplace")
  }
  for (column in list(0, -1, 1.5, Inf, NA_real_, TRUE, 1+0i, matrix(1), c(1, 2), "")) {
    expect_error(t1p_call(cellreplace = t1p_replace("age", column)), class = "tabtools_error_cellreplace")
  }
  expect_error(t1p_call(cellreplace = t1p_replace("age", 99L)),
               class = "tabtools_error_cellreplace_selector")
  expect_identical(unname(t1p_call(cellreplace = t1p_replace("age", 1, ""))$body[5, 2]), "")
})

test_that("category children invalidate only owning variable publication companions", {
  t1p_local()
  base <- t1p_call(smd = TRUE, missingsummary = TRUE)
  t <- t1p_call(smd = TRUE, missingsummary = TRUE, cellreplace = t1p_replace("Low", "A"))
  expect_true(all(is.na(t$stored$table[1, ])))
  expect_identical(t$stored$table[-1, ], base$stored$table[-1, ])
  expect_true(is.na(t$rows$p[1]) && is.na(t$rows$smd[1]))
  m <- base$meta$sample_accounting$measures
  ids <- c("variable/1/group/1", "variable/1/table")
  wanted <- paste(m$population_id[m$population_id %in% ids & m$status == "available"],
                  m$metric[m$population_id %in% ids & m$status == "available"], sep = ":")
  expect_setequal(t1p_ledger_changed(base, t), wanted)
  changed <- t$meta$sample_accounting$measures$population_id %in% ids & m$status == "available"
  expect_true(all(is.na(t$meta$sample_accounting$measures$value[changed])))
  expect_true(all(t$meta$sample_accounting$measures$reason[changed] == "replaced_by_cellreplace"))
  e <- t$meta$sample_accounting$exclusions
  before <- base$meta$sample_accounting$exclusions
  hit <- e$population_id %in% ids & before$status == "available"
  expect_true(all(is.na(e$n[hit])))
  expect_true(all(e$status[hit] == "unavailable"))
  expect_identical(e[!hit, ], before[!hit, ])
  expect_identical(t$stored$raw$sample_accounting, base$meta$sample_accounting)
  # The missing-summary child belongs to the same category variable.
  missing <- which(base$rows$type == "missing_summary")[1]
  expect_false(is.na(missing))
  r <- t1p_call(smd = TRUE, missingsummary = TRUE,
               cellreplace = t1p_replace(base$body[missing, 1], "A"))
  expect_true(all(is.na(r$stored$table[1, ])))
  expect_identical(r$stored$table[-1, ], base$stored$table[-1, ])
})

test_that("p and SMD replacements clear exactly their publication numeric companion", {
  t1p_local()
  base <- t1p_call(smd = TRUE, test = TRUE, statistic = TRUE)
  for (role in c("p", "smd", "test", "statistic")) {
    column <- which(base$cols$role == role)
    literal <- if (role == "p") "0.00001" else if (role == "smd") "99" else "Literal"
    t <- t1p_call(smd = TRUE, test = TRUE, statistic = TRUE,
                 cellreplace = t1p_replace("age", as.integer(column - 1L), literal))
    expect_identical(unname(t$body[5, column]), literal)
    fields <- if (role == "p") "p_value" else if (role == "smd") "smd" else c("p_value", "smd")
    expect_true(all(is.na(t$stored$table[2, fields])))
    keep <- setdiff(colnames(base$stored$table), fields)
    if (length(keep)) expect_identical(t$stored$table[2, keep], base$stored$table[2, keep])
    expect_identical(t$stored$table[c(1, 3), ], base$stored$table[c(1, 3), ])
    expect_identical(t$meta$sample_accounting, base$meta$sample_accounting)
    if (role %in% c("p", "smd")) {
      rendered <- tabtools:::.tt_render_spec(t)
      expect_false(rendered$body$bold[5, column])
    }
  }
})

test_that("descriptor N and ESS affect scoped ledger slots without variable inference", {
  t1p_local()
  base <- t1p_call(smd = TRUE, total = "before")
  descriptor <- base$header[[2]]$text[1]
  t <- t1p_call(smd = TRUE, total = "before", cellreplace = t1p_replace(descriptor, 1L))
  expect_identical(t$stored$table, base$stored$table)
  expect_identical(t$rows, base$rows)
  expect_identical(t1p_ledger_changed(base, t), "table:reported_n")
  expect_identical(unname(t$header[[2]]$text[2]), "Not reportable")
  weighted <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE)
  for (col in c(1L, 3L)) {
    r <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE,
                 cellreplace = t1p_replace(weighted$header[[2]]$text[1], col))
    expect_identical(t1p_ledger_changed(weighted, r),
      paste0(if (col == 1L) "crude" else "weighted", "/group/1:reported_n"))
    expect_identical(r$stored$table, weighted$stored$table)
  }
  r <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE,
               cellreplace = t1p_replace(weighted$body[1, 1], 3L))
  expect_identical(t1p_ledger_changed(weighted, r), "weighted/group/1:effective_n")
  expect_identical(r$stored$table, weighted$stored$table)
  crude <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE,
                   cellreplace = t1p_replace(weighted$body[1, 1], 1L))
  expect_identical(crude$meta$sample_accounting, weighted$meta$sample_accounting)
})

test_that("crude and weighted body replacements redact only their owned ledger scope", {
  t1p_local()
  base <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE)
  for (col in c(1L, 3L)) {
    t <- t1p_call(wt = "w", wtn = TRUE, wtcompare = TRUE, smd = TRUE,
                 cellreplace = t1p_replace("Low", col))
    prefix <- if (col == 1L) "crude/" else "weighted/"
    changed <- t1p_ledger_changed(base, t)
    expect_true(length(changed) > 0L)
    expect_true(all(startsWith(changed, paste0(prefix, "variable/1/"))))
    expect_true(all(grepl("/(group/1|table):", changed)))
    expect_true(all(is.na(t$stored$table[1, ])))
    expect_identical(t$stored$table[-1, ], base$stored$table[-1, ])
  }
})

test_that("protected, linked and invalid targets fail before sinks or session history mutate", {
  directory <- t1p_local()
  book <- file.path(directory, "publication.xlsx")
  md <- file.path(directory, "publication.md")
  csv <- file.path(directory, "publication.csv")
  puttab(data.frame(x = "UNCHANGED"), xlsx = book, markdown = md, csv = csv)
  bytes <- lapply(c(book, md, csv), ss_bytes)
  state <- as.list(tabtools:::.tt_sink_state, all.names = TRUE)
  for (selector in list(t1p_replace("Low", "A"), t1p_replace("Mid", "A"),
                       t1p_replace("age", "Absent"))) {
    # A safe earlier entry must never be partially exported on failure.
    expect_error(t1p_call(smallcells = 5, cellreplace = c(t1p_replace("age", "A"), selector),
                          xlsx = book, markdown = md, csv = csv))
    expect_identical(lapply(c(book, md, csv), ss_bytes), bytes)
    expect_identical(as.list(tabtools:::.tt_sink_state, all.names = TRUE), state)
  }
  strict <- t1p_call(smallcells = 5, smd = TRUE)
  # Mid/A has code zero but its withheld percent still makes it protected.
  expect_error(t1p_call(smallcells = 5, smd = TRUE, cellreplace = t1p_replace("Mid", "A")),
               class = "tabtools_error_cellreplace_protected")
  for (column in which(strict$cols$role %in% c("p", "smd"))) expect_error(
    t1p_call(smallcells = 5, smd = TRUE, cellreplace = t1p_replace("category", column - 1L)),
    class = "tabtools_error_cellreplace_protected")
  safe <- t1p_call(smallcells = 5, cellreplace = t1p_replace("age", "A"))
  expect_null(safe$stored$raw)
  zero <- table1_tc(t1p_data(), by = "g", vars = c(age = "contn"), smallcells = 3,
                    cellreplace = t1p_replace("age", "A"))
  expect_null(zero$stored$raw)
})

test_that("literal replacement reaches frames, direct sinks and composition", {
  directory <- t1p_local()
  literal <- "quote \" $value `code` \\path"
  t <- t1p_call(smallcells = 5, smallcells_mode = "primary", smd = TRUE,
               cellreplace = t1p_replace("age", "A", literal))
  frame <- as.data.frame(t)
  expect_true(any(as.matrix(frame) == literal))
  expect_identical(attr(frame, "sample_accounting"), t$meta$sample_accounting)
  expect_false(any(grepl("raw", names(attributes(frame)))))
  csv <- file.path(directory, "table.csv")
  md <- file.path(directory, "table.md")
  book <- file.path(directory, "table.xlsx")
  tt_write_csv(t, csv)
  tt_write_markdown(t, md)
  tt_write_xlsx(t, book)
  expect_true(any(as.matrix(utils::read.csv(csv, header = FALSE, check.names = FALSE)) == literal))
  escaped <- tabtools:::.md_escape(literal)
  expect_match(ss_text(md), escaped, fixed = TRUE)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(book))), "Table 1")
  expect_true(any(openxlsx2::wb_to_df(openxlsx2::wb_load(book), sheet = "Table 1", col_names = FALSE) == literal, na.rm = TRUE))
  console <- paste(capture.output(print(t)), collapse = "\n")
  expect_match(console, literal, fixed = TRUE)
  exported <- puttab(t)
  expect_true(any(as.matrix(exported$body) == literal))
  expect_false(any(grepl("raw", names(exported$meta))))
  composite <- stacktab(list(t), xlsx = file.path(directory, "stack.xlsx"), sheet = "S")
  expect_true(any(as.matrix(composite$body) == literal))
  expect_false(any(grepl("raw", names(composite$meta))))
})

test_that("missing publication ownership refuses replacement before writing", {
  directory <- t1p_local()
  original <- tabtools:::.t1_build
  testthat::local_mocked_bindings(.t1_build = function(...) {
    tt <- original(...)
    tt$meta$cellreplace_spec[tt$rows$var == "age"] <- NA_integer_
    tt
  }, .package = "tabtools")
  path <- file.path(directory, "refused.csv")
  expect_error(t1p_call(cellreplace = t1p_replace("age", "A"), csv = path),
               class = "tabtools_error_cellreplace_companion")
  expect_false(file.exists(path))
})

test_that("no-by Total selections own aggregate ledger slots", {
  t1p_local()
  d <- t1p_data()
  base <- table1_tc(d, vars = c(age = "contn"), wt = "w", wtn = TRUE)
  n <- table1_tc(d, vars = c(age = "contn"), wt = "w", wtn = TRUE,
                 cellreplace = t1p_replace(base$header[[2]]$text[1], "Total"))
  expect_identical(t1p_ledger_changed(base, n), "table:reported_n")
  ess <- table1_tc(d, vars = c(age = "contn"), wt = "w", wtn = TRUE,
                   cellreplace = t1p_replace("Effective sample size", 1L))
  expect_identical(t1p_ledger_changed(base, ess), "table:effective_n")
  body <- table1_tc(d, vars = c(age = "contn"), wt = "w", wtn = TRUE,
                    cellreplace = t1p_replace("age", "Total"))
  m <- base$meta$sample_accounting$measures
  hit <- m$population_id == "variable/1/table" & m$status == "available"
  expect_setequal(t1p_ledger_changed(base, body), paste(m$population_id[hit], m$metric[hit], sep = ":"))
  expect_null(body$stored$table)
})
