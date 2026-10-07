# Explicit R endpoint improvement beside immutable native 2.5.1 T36.
# Native 2.5.6 inline scaling uses 2^1023 (missing) at weight 8e307;
# its genuine T36 returns four missing SMDs. R uses the bounded 2^1022
# helper. Only Price/Mileage differ from the historical full fixture.
# Expected raw values come from independent analytic-weight formulas on
# all original auto.dta records, never from the R table producer/formatter.
golden_t36_smd <- c(1.3946699947292704, 0.59312020277361166,
                    1.756081052448329, 1.1136276863730883)
golden_t36_console <- c(
  "  +-----------------------------------------------------------------------------------------------------------+",
  "  |                                                                       Domestic      Foreign       SMD     |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | No. (Column %), Mean±SD, Geometric mean (×/GSD), or Median (Q1, Q3)   N=52          N=22                  |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | Effective sample size                                                 ESS=1         ESS=22                |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | Price                                                                 3799±0        6385±2622     1.395   |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | Mileage (mpg)                                                         22 (22, 22)   24 (21, 28)   0.593   |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | Repair record 1978                                                                                1.756   |",
  "  |    1                                                                  2 (4)         0 (0)                 |",
  "  |    2                                                                  8 (17)        0 (0)                 |",
  "  |    3                                                                  27 (56)       3 (14)                |",
  "  |    4                                                                  9 (19)        9 (43)                |",
  "  |    5                                                                  2 (4)         9 (43)                |",
  "  |-----------------------------------------------------------------------------------------------------------|",
  "  | Headroom (in.)                                                        3 (×/1.00)    3 (×/1.22)    1.114   |",
  "  +-----------------------------------------------------------------------------------------------------------+",
  ""
)
golden_t36_csv <- c(
  " ,Domestic,Foreign,SMD",
  "\"No. (Column %), Mean±SD, Geometric mean (×/GSD), or Median (Q1, Q3)\",N=52,N=22,",
  "Effective sample size,ESS=1,ESS=22,",
  "Price,3799±0,6385±2622,1.395",
  "Mileage (mpg),\"22 (22, 22)\",\"24 (21, 28)\",0.593",
  "Repair record 1978,,,1.756",
  "   1,2 (4),0 (0),",
  "   2,8 (17),0 (0),",
  "   3,27 (56),3 (14),",
  "   4,9 (19),9 (43),",
  "   5,2 (4),9 (43),",
  "Headroom (in.),3 (×/1.00),3 (×/1.22),1.114"
)
golden_t36_md <- c(
  "| No. (Column %), Mean±SD, Geometric mean (×/GSD), or Median (Q1, Q3) | Domestic (N=52) | Foreign (N=22) | SMD |",
  "| --- | --- | --- | --- |",
  "| Effective sample size | ESS=1 | ESS=22 |  |",
  "| Price | 3799±0 | 6385±2622 | 1.395 |",
  "| Mileage (mpg) | 22 (22, 22) | 24 (21, 28) | 0.593 |",
  "| Repair record 1978 |  |  | 1.756 |",
  "| &nbsp;&nbsp;&nbsp;1 | 2 (4) | 0 (0) |  |",
  "| &nbsp;&nbsp;&nbsp;2 | 8 (17) | 0 (0) |  |",
  "| &nbsp;&nbsp;&nbsp;3 | 27 (56) | 3 (14) |  |",
  "| &nbsp;&nbsp;&nbsp;4 | 9 (19) | 9 (43) |  |",
  "| &nbsp;&nbsp;&nbsp;5 | 2 (4) | 9 (43) |  |",
  "| Headroom (in.) | 3 (×/1.00) | 3 (×/1.22) | 1.114 |"
)
golden_t36_sources <- c(
  "fixtures/auto.dta" = "ae448e328577f02cda945f525c3cf8e61722fe7a56686761376aa6dea99752a6",
  "T36.csv" = "a8ca5b410c415c45a040ad55f25cc395a5e2658972f0b817ce793ca1c5b00539",
  "T36.md" = "cfa91cf0a795049198ba5f643754b1455741a34880ebe1a61221f65ee2feb9f7",
  "T36_console.txt" = "b9344f09690f831309dfe85e76f48171a10b4c8cd10133c5696967ea1c69fdd1",
  "T36_stored.csv" = "5addcf5649cb641f8735aa4ae778beab664dbc7afa19f2f3701826da2ce15de4",
  "table1_tc.xlsx" = "ea08a7db1c16dbbfe623d2fc1caa5c4f7939f697a382ee63ae92e3243e237d59"
)

golden_patch_t36_endpoint <- function(tt) {
  for (name in names(golden_t36_sources)) {
    testthat::expect_identical(digest::digest(file = golden_path(name), algo = "sha256", serialize = FALSE),
      golden_t36_sources[[name]], info = paste("immutable T36 native/input artifact", name))
  }
  testthat::expect_identical(tt$command, "table1_tc")
  testthat::expect_identical(dim(tt$body), c(10L, 4L))
  testthat::expect_identical(tt$cols$role, c("label", "group", "group", "smd"))
  testthat::expect_identical(tt$body[[1L]][c(2L, 3L)], c("Price", "Mileage (mpg)"))
  testthat::expect_identical(tt$rows$key[c(2L, 3L)], c("Price", "Mileage (mpg)"))
  testthat::expect_identical(tt$rows$table_row[c(2L, 3L)], c(1L, 2L))
  testthat::expect_identical(dimnames(tt$stored$table),
    list(c("Price", "Mileage_(mpg)", "Repair_record_1978", "Headroom_(in_)"), "smd"))
  testthat::expect_identical(dim(tt$stored$table), c(4L, 1L))
  testthat::expect_equal(unname(tt$stored$table[, 1L]), golden_t36_smd, tolerance = 1e-12)
  testthat::expect_equal(tt$rows$smd[c(2L, 3L, 4L, 10L)], golden_t36_smd, tolerance = 1e-12)
  testthat::expect_true(all(is.na(tt$rows$smd[c(1L, 5L:9L)])))
  want <- golden_read_cells("T36")
  testthat::expect_identical(dim(want), c(12L, 4L))
  testthat::expect_identical(want[4L:5L, 4L], c("", ""))
  want[4L:5L, 4L] <- c("1.395", "0.593")
  testthat::expect_identical(golden_as_cells(tt), want, info = "complete independently qualified R grid")
  testthat::expect_identical(format(tt), head(golden_t36_console, -1L), info = "whole R formatted console without print separator")
  testthat::expect_identical(utils::capture.output(print(tt)), head(golden_t36_console, -1L),
    info = "whole printed R console, no chatter trimming")
  before <- serialize(tt, NULL)
  peer <- tt
  peer$body[2L:3L, 4L] <- ""
  peer$rows$smd[2L:3L] <- NA_real_
  peer$stored$table[1L:2L, 1L] <- NA_real_
  # Assert the complete projection, so later-source fields/provenance are
  # neither dropped nor rewritten. This copy is a comparator peer only.
  restored <- peer
  restored$body[2L:3L, 4L] <- tt$body[2L:3L, 4L]
  restored$rows$smd[2L:3L] <- tt$rows$smd[2L:3L]
  restored$stored$table[1L:2L, 1L] <- tt$stored$table[1L:2L, 1L]
  testthat::expect_identical(serialize(restored, NULL), before)
  out <- withr::local_tempdir(pattern = "tabtools-t36-endpoint-")
  for (ext in c("csv", "md")) {
    path <- file.path(out, paste0("original.", ext))
    if (ext == "csv") tabtools::tt_write_csv(tt, path) else tabtools::tt_write_markdown(tt, path)
    literal <- if (ext == "csv") golden_t36_csv else golden_t36_md
    testthat::expect_identical(golden_bytes(path), charToRaw(paste0(paste(literal, collapse = "\n"), "\n")),
      info = paste("whole original R", ext, "bytes"))
  }
  original_book <- file.path(out, "original.xlsx")
  peer_book <- file.path(out, "peer.xlsx")
  tabtools::tt_write_xlsx(tt, original_book, sheet = "T36")
  tabtools::tt_write_xlsx(peer, peer_book, sheet = "T36")
  a <- golden_cell_styles(original_book, "T36")
  b <- golden_cell_styles(peer_book, "T36")
  ix <- match(c("E5", "E6"), a$address)
  iy <- match(c("E5", "E6"), b$address)
  testthat::expect_false(anyNA(c(ix, iy)))
  testthat::expect_identical(a$value[ix], c("1.395", "0.593"))
  testthat::expect_identical(b$value[iy], c("", ""))
  style_fields <- c("address", "row", "col", "value", "bold", "italic", "font", "size",
    "number_format", "font_color", "halign", "valign", "wrap", "border_top",
    "border_bottom", "border_left", "border_right", "fill")
  testthat::expect_identical(names(a), append(style_fields, "format_id", after = 9L))
  testthat::expect_identical(names(b), append(style_fields, "format_id", after = 9L))
  # All four finite SMDs retain the exact declared bold/bisque style. The
  # comparator peer has only its Price/Mileage SMD texts and styles blanked.
  smd_a <- match(c("E5", "E6", "E7", "E13"), a$address)
  smd_b <- match(c("E5", "E6", "E7", "E13"), b$address)
  testthat::expect_false(anyNA(c(smd_a, smd_b)))
  testthat::expect_identical(a$bold[smd_a], rep(TRUE, 4L))
  testthat::expect_identical(a$fill[smd_a], rep("FFFFEBCD", 4L))
  testthat::expect_identical(b$bold[smd_b], c(FALSE, FALSE, TRUE, TRUE))
  testthat::expect_identical(b$fill[smd_b], c("", "", "FFFFEBCD", "FFFFEBCD"))
  a$value[ix] <- ""
  a$bold[ix] <- FALSE
  a$fill[ix] <- ""
  # format_id is a workbook-local lookup index; every resolved style field,
  # including number_format, plus every cell/address remains compared.
  testthat::expect_identical(a[style_fields], b[style_fields],
    info = "complete resolved original workbook styles with two literal endpoint cells qualified")
  testthat::expect_identical(golden_sheet_layout(original_book, "T36"), golden_sheet_layout(peer_book, "T36"),
    info = "complete original merges, widths and row heights")
  testthat::expect_identical(serialize(tt, NULL), before, info = "original result unchanged by qualification and sinks")
  peer
}
