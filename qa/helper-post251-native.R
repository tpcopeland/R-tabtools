# Native 2.5.6 selected regressions, separate from the full 2.5.1 corpus.
p256_root <- function() {
  root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
  if (!nzchar(root)) root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
  root <- normalizePath(root, mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION")) || !dir.exists(file.path(root, "tests", "testthat")))
    stop("Expected complete QA source checkout")
  root
}
p256_auth <- function(family) {
  base <- Sys.getenv("TABTOOLS_POST251_FIXTURES",
    unset = file.path(p256_root(), "qa", "data", "post251-256"))
  directory <- normalizePath(file.path(base, family), mustWork = TRUE)
  expected <- p256_expected[[family]]
  if (is.null(expected)) stop("Unknown selected native corpus")
  paths <- list.files(directory, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  if (!identical(sort(paths), sort(names(expected)))) stop("Selected native corpus inventory differs")
  for (name in names(expected)) {
    actual <- digest::digest(file = file.path(directory, name), algo = "sha256", serialize = FALSE)
    if (!identical(actual, unname(expected[[name]]))) stop(paste("Selected native artifact changed:", name))
  }
  read <- function(name) jsonlite::fromJSON(file.path(directory, name), simplifyVector = FALSE)
  receipt <- read("native-receipt.json"); closure <- read("source-provenance.json"); clean <- read("cleanup.json")
  pin <- "4eecca4d09d61df0cfe773fc2024b015920b5fe6"
  recipe <- if (family == "disclosure") "46d85df46c54ed447db341214882c20041e0f2c894fb7e9d7f52e100a2d3fa48" else
    "1cf7e058e22f0d1e287abf245f114c69b7085116a2b57d06a358422a3da3250a"
  if (!identical(receipt$source_pin, pin) || !identical(closure$pin, pin) ||
      !identical(closure$version, "2.5.6") || length(closure$files) != 363L ||
      !identical(receipt$recipe_sha256, recipe) ||
      !identical(receipt$source_provenance_sha256, unname(expected[["source-provenance.json"]])) ||
      !isTRUE(clean$absent) || clean$exit_status != 0L) stop("Selected native source/recipe/cleanup identity differs")
  out_names <- sub("^out/", "", names(expected)[startsWith(names(expected), "out/")])
  if (!identical(sort(names(receipt$files)), sort(out_names))) stop("Native receipt inventory differs")
  for (name in out_names) if (!identical(receipt$files[[name]], unname(expected[[paste0("out/", name)]])))
    stop("Native receipt does not bind authentic output bytes")
  oracle <- read("literal-oracles.json")
  if (!identical(oracle$source_pin, pin) ||
      !identical(oracle$capture_log_sha256, unname(expected[["capture.log"]]))) stop("Literal native oracle provenance differs")
  log <- readLines(file.path(directory, "capture.log"), warn = FALSE, encoding = "UTF-8")
  if (sum(log == "ROOT_NATIVE_256_CAPTURE_COMPLETED") != 1L || any(grepl("^r\\([0-9]+\\);$", log)))
    stop("Native runtime failed or lacks completion sentinel")
  # The constant hashes above bind all original log lines, artifacts, and the
  # complete source closure before any DTA/CSV/XLSX or oracle values are loaded.
  list(directory = directory, oracle = oracle, receipt = receipt)
}
p256_helpers <- function() {
  e <- new.env(parent = globalenv())
  for (name in c("helper-golden.R", "helper-golden-footnotes.R", "helper-golden-tidy.R"))
    sys.source(file.path(p256_root(), "tests", "testthat", name), envir = e)
  e
}
p256_case <- function(proof, id) {
  case <- proof$oracle$cases[[id]]
  if (is.null(case) || case$command_rc != 0L) stop("Missing genuine native case")
  case
}
p256_vector <- function(x) as.vector(unlist(x, use.names = FALSE))
p256_matrix <- function(x) {
  matrix(as.double(p256_vector(x$values)), nrow = as.integer(x$nrow), ncol = as.integer(x$ncol), byrow = TRUE,
    dimnames = list(as.character(p256_vector(x$rownames)), as.character(p256_vector(x$colnames))))
}
p256_clear <- function(env) {
  keys <- paste0("tabtools.", getFromNamespace(".tt_option_keys", "tabtools"))
  old_options <- stats::setNames(lapply(keys, getOption), keys)
  state <- getFromNamespace(".tt_sink_state", "tabtools")
  old <- as.list(state, all.names = TRUE)
  withr::defer({
    options(old_options)
    rm(list = ls(state, all.names = TRUE), envir = state)
    list2env(old, state)
  }, envir = env)
  tabtools::tabtools_options(clear = TRUE)
}
p256_input <- function(data, proof, id) {
  native <- haven::read_dta(file.path(proof$directory, "out", paste0(id, "-input.dta")))
  testthat::expect_identical(names(data), names(native), info = paste(id, "complete original input column order"))
  testthat::expect_identical(nrow(data), nrow(native), info = id)
  for (name in names(native)) {
    # Native storage widths and display formats are source metadata, retained
    # in the immutable DTA. R's literal builder supplies the same full values.
    testthat::expect_identical(as.double(data[[name]]), as.double(native[[name]]), info = paste(id, name, "all input values"))
    testthat::expect_null(attr(native[[name]], "labels", exact = TRUE), info = paste(id, name, "no invented value labels"))
    testthat::expect_null(attr(native[[name]], "label", exact = TRUE), info = paste(id, name, "no invented variable label"))
    testthat::expect_false(any(haven::is_tagged_na(native[[name]])), info = paste(id, name, "ordinary source missingness"))
  }
}
p256_table1_frame <- function(tt, proof, id) {
  d <- haven::read_dta(file.path(proof$directory, "out", paste0(id, "-frame.dta")))
  testthat::expect_identical(names(d)[1L], "factor")
  expected_names <- c("factor", colnames(tt$stored$suppression))
  if (is.null(tt$stored$suppression)) {
    groups <- sort(unique(as.double(haven::read_dta(file.path(proof$directory, "out", paste0(id, "-input.dta")))$g)))
    expected_names <- c("factor", paste0("g_", groups), if (!is.null(tt$stored$table)) "smd_str")
  }
  # DS05 explicitly requests total(before); publication order is independent
  # of the analytically keyed suppression matrix (g_1 through g_5, then g_T).
  if (identical(id, "DS05")) expected_names <- c("factor", "g_T", "g_1", "g_2", "g_3", "g_4", "g_5")
  testthat::expect_identical(names(d), expected_names, info = paste(id, "native frame identities"))
  publication <- unname(as.matrix(as.data.frame(tt)))
  native <- unname(as.matrix(d))
  testthat::expect_identical(publication[1L, 1L], " ", info = paste(id, "R literal label header"))
  testthat::expect_identical(native[1L, 1L], "", info = paste(id, "native serialized empty label header"))
  # Qualify this single known serialization position, preserving every
  # other value, row and column in the complete frame comparison.
  native[1L, 1L] <- " "
  testthat::expect_identical(publication, native, info = paste(id, "complete frame cells"))
  testthat::expect_identical(attr(d$factor, "label", exact = TRUE), "Factor")
  for (name in names(d)[-1L]) {
    testthat::expect_identical(attr(d[[name]], "label", exact = TRUE), d[[name]][1L], info = paste(id, name, "frame label"))
    testthat::expect_null(attr(d[[name]], "labels", exact = TRUE), info = paste(id, name, "frame value labels"))
  }
}
p256_stored <- function(tt, case, fields) {
  for (name in fields) {
    want <- case$scalars[[name]]
    if (!is.null(want)) testthat::expect_equal(tt$stored[[name]], as.double(want), tolerance = 1e-12, info = paste(name, "literal native scalar"))
    else testthat::expect_identical(tt$stored[[name]], case$macros[[name]], info = paste(name, "literal native macro"))
  }
}
p256_strict_note <- "Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count."
p256_native_primary <- "Counts from 1 to 2 are shown as <3 without a percentage (primary suppression only: no complementary cells are masked). This protects printed counts only."
p256_R_primary <- "Counts from 1 to 2 are shown as <3 without a percentage (primary suppression only: no complementary cells are masked). Unmasked cells and ordinary variable tests are shown as computed; effective sample size linked to a masked sample count is withheld. This protects printed counts only."
p256_console <- function(tt, case, kind, note = NULL) {
  native <- as.character(p256_vector(case$native_console))
  if (kind == "disclosure") {
    want_native <- if (identical(note, p256_R_primary)) p256_native_primary else p256_strict_note
    testthat::expect_identical(tail(native, 1L), want_native, info = "complete native publication annotation")
    native[length(native)] <- note
    expected <- c(native, "")
  } else if (kind == "weight") {
    if (is.null(note)) expected <- native else {
      testthat::expect_identical(native[1L], paste("Note:", note), info = "complete native SMD prebox annotation")
      expected <- c(native, note, "")
    }
  } else expected <- native
  testthat::expect_identical(sum(grepl("^  \\+-+\\+$", native)), 2L, info = "complete native box")
  testthat::expect_identical(capture.output(print(tt)), expected, info = "complete publication console array including paragraph separators")
}
p256_sinks <- function(tt, paths, proof, id, h, note = NULL) {
  for (ext in c("csv", "md")) {
    got <- paths[[ext]]; want <- file.path(proof$directory, "out", paste0(id, ".", ext))
    if (!identical(note, p256_R_primary)) testthat::expect_identical(h$golden_bytes(got), h$golden_bytes(want), info = paste(id, ext, "all bytes"))
    else {
      # Independent serialization of the deliberately stronger R paragraph.
      # Every native byte before that exact footer record remains required.
      native <- readLines(want, warn = FALSE, encoding = "UTF-8")
      if (ext == "csv") {
        cells <- h$golden_read_cells_file(want)
        native_footer <- paste(c(p256_native_primary, rep("", ncol(cells)-1L)), collapse = ",")
        R_footer <- paste(c(p256_R_primary, rep("", ncol(cells)-1L)), collapse = ",")
      } else {
        encode <- function(s) paste0("*", paste(vapply(strsplit(s, "", fixed = TRUE)[[1L]], function(ch) if (ch == "<") paste0(intToUtf8(92L), ch) else ch, ""), collapse = ""), "*")
        native_footer <- encode(p256_native_primary); R_footer <- encode(p256_R_primary)
      }
      testthat::expect_identical(tail(native, 1L), native_footer, info = "exact authentic primary footer")
      native[length(native)] <- R_footer
      testthat::expect_identical(h$golden_bytes(got), charToRaw(paste0(paste(native, collapse = "\n"), "\n")), info = paste(id, ext, "complete stronger paragraph serialization"))
    }
  }
  want <- file.path(proof$directory, "out", paste0(id, ".xlsx"))
  if (!identical(note, p256_R_primary)) {
    testthat::expect_identical(h$golden_compare_styles(paths$xlsx, "Table", want, "Table", got_width_offset = h$golden_r_width_offset), character(), info = paste(id, "all workbook cells/styles/merges/widths/heights"))
  } else {
    peer <- tt; peer$footnote <- p256_native_primary
    peer_path <- file.path(dirname(paths$xlsx), "publication-peer.xlsx")
    tabtools::tt_write_xlsx(peer, peer_path, sheet = "Table")
    testthat::expect_identical(h$golden_compare_styles(peer_path, "Table", want, "Table", got_width_offset = h$golden_r_width_offset), character(), info = paste(id, "native peer all workbook attributes"))
    g <- h$golden_cell_styles(paths$xlsx, "Table"); w <- h$golden_cell_styles(peer_path, "Table")
    testthat::expect_identical(g$address, w$address, info = "complete original/peer workbook address order")
    where <- which(g$value == p256_R_primary)
    testthat::expect_length(where, 1L)
    testthat::expect_identical(w$value[where], p256_native_primary)
    g$value[where] <- p256_native_primary
    testthat::expect_identical(g, w, info = "only independently asserted paragraph value differs; all styles strict")
    testthat::expect_identical(h$golden_sheet_layout(paths$xlsx, "Table"), h$golden_sheet_layout(peer_path, "Table"), info = "original complete footer and body geometry")
  }
  before <- tt
  tabtools::tt_write_csv(tt, file.path(dirname(paths$csv), "later.csv"))
  tabtools::tt_write_markdown(tt, file.path(dirname(paths$md), "later.md"))
  testthat::expect_identical(h$golden_bytes(file.path(dirname(paths$csv), "later.csv")), h$golden_bytes(paths$csv))
  testthat::expect_identical(h$golden_bytes(file.path(dirname(paths$md), "later.md")), h$golden_bytes(paths$md))
  testthat::expect_identical(tt, before, info = "sinks preserve raw/canonical/publication metadata")
}

p256_expected <- list(
  "disclosure" = c(
    "capture.log" = "fda6729fc9dfb751880b1c8baa07ccad10f571ccca6abc7130640357e8b7d29f",
    "cleanup.json" = "5f2d1572e9f261698c0bef24c8a56d9b3c62b89fa86fcf0c6752cf380f8f0be7",
    "literal-oracles.json" = "6a8ae7e7e844c573c86341b6cb8c35e4c3937f12cfd67415264e868bc1eaba63",
    "native-receipt.json" = "af51f055042def494e7efc56afaa9336ca60c3fe0f2381c2b3cd17f1d4e239e9",
    "out/DS01-counts.csv" = "b94adbf7277d23bb6e7dfce1e21d79a2ac73d88f7d58cad6b5297f1d91549535",
    "out/DS01-frame.dta" = "ae684ba63b3260a3b765e24f53ef2ec02596f48681cc70d49db9291c9bd663a7",
    "out/DS01-input.dta" = "73d32a44500024482e7c97c4933b4ca16400b6491772e47a27a07ff6ed193c33",
    "out/DS01.csv" = "b01305ace5ca391cf4fed7235daeebba8f52d7e1dfaa9ae80ceca413f48f8733",
    "out/DS01.md" = "d91002a7f0dee3636461b0f5848a00751732aa1bb7e73d79991f90edb772fc62",
    "out/DS01.xlsx" = "114cced17e4956546f7d4de2679813d578acce15287ba1eb63d2775e258858a3",
    "out/DS02-counts.csv" = "e955b2d7ce3086a6d1c3a8cbf54b301af5be5ffc0408069792f7f84a4e6bc39b",
    "out/DS02-frame.dta" = "b1e0a8ece211e9094a5a10522895665780513acbd3aa941a06d5398ecad9657c",
    "out/DS02-input.dta" = "a159babbd0f2b841d820600299c4d63c5098a724cb048f3cffd74b453603fb4d",
    "out/DS02.csv" = "258204fe8cddceb6afddab22dcd4cba30a6dc574e36426455858e557e5b6ccbc",
    "out/DS02.md" = "83cc7549c80629fd7a940ebf8cc73fe4521796eee262049179f67ec01e10afb3",
    "out/DS02.xlsx" = "433c4e768a465f2d240115f57ba2fe5c10d80097eb528b989c2962cdcff725df",
    "out/DS03-counts.csv" = "a3a68b2c6474910cce7c285f6f0635bf0565753fb5d9e86d699c270ba69bb8fe",
    "out/DS03-frame.dta" = "9cc7c09d825b6d3c5ebf9fa6d1df31d58d55dbbcab8853b0d9844663986676d4",
    "out/DS03-input.dta" = "b739b38a49cf58c8daddc486c2e7f17f2ef4b8e4215d0a747ba7fa77da861219",
    "out/DS03.csv" = "aa6ceddfcabee9939d8fb8a4d91e0b78edfcc23516e2e14a779d54ef05578f9f",
    "out/DS03.md" = "02a9593d45edafd698de08c2509b4ee9a763cc61396b3c244cfa509426461331",
    "out/DS03.xlsx" = "0564eb8e05fee6fa528acea7c7916a0820be0009975f187a38dfeb63e187a7cb",
    "out/DS04-counts.csv" = "c32a98d4f3bca7a5d6e85cfd1f6935006eea8fd395f0e0d4f46a1004c7636024",
    "out/DS04-frame.dta" = "153607e6fb929c12319e6bd275625903267fba6839877b101a46d046954b5aa9",
    "out/DS04-input.dta" = "1b8b6f29cf75047255150daacdc8606d3c975dfcc9a06ace63d71ab141570106",
    "out/DS04.csv" = "8a02b34f2af5b7f44b919c4fdf1cd783b8735eaa8f02c3e36640b4ff9aa41553",
    "out/DS04.md" = "9012dfa28fc51774fde67edf78e9d5ac927248a5386c05a4b0a82923e79450e4",
    "out/DS04.xlsx" = "b3e6b6aee821eda88bd6caae274de41f5273eb3d9b33830debcb0f371f1cf0cf",
    "out/DS05-counts.csv" = "ec61d5d52a60f1f1d329fa0293ae0a42029cee35183b1f803cc48513f4f7fc5f",
    "out/DS05-frame.dta" = "5139b90287d59bb5c9ee89da8a554cc5a3e5308df48c6268eb4123f19e566dda",
    "out/DS05-input.dta" = "92c5aa5a2c102e9c02d199070d7bd9e172268adfd686cb5d2fb7556ed186f3af",
    "out/DS05.csv" = "c5c494c7f1f8e59057cf9508bc9fdf157243995a1f7653d1f5e7dc71f8854700",
    "out/DS05.md" = "f9981cd952c5b9a6041debe9a5f6bc0f8d79dcb53326571b79ff7cf4c6a26b30",
    "out/DS05.xlsx" = "66a4697e5733b4396a79ce7670c77234c9b1ef1629381605c4e288d00ae6a8ed",
    "out/DS06-counts.csv" = "1d6ffdc431c079208de2fa88de9b6764c543d7416517bfa30dc5cd784dd1707b",
    "out/DS06-frame.dta" = "9f8bf5aa2dfbe683091075eb8b0fcf97dfb85dab1e063df5cc6ee6a79ad478d1",
    "out/DS06-input.dta" = "e5928ddbc32b4b6d86fe967b721d866e654963ac35bb16235ff1200b6f739f2d",
    "out/DS06.csv" = "7c826765111bc2b934144f962b332389f6d4bda4b7d82ee60b85c1d1125c3c0e",
    "out/DS06.md" = "0f8d7404306bb31c4d194b9cd32bb3c5af30b7d28792fd79f63d69da12887fb5",
    "out/DS06.xlsx" = "404e2424a1026965d642050a32a50b53b00861fc750756eca4b58e55fb734487",
    "out/DS07-counts.csv" = "1a40a88651b6a30efb76ce2fe1ee5a1b76159979c616cd6de6fb92b9af35f8c8",
    "out/DS07-frame.dta" = "4eb1c7c395b9af3538c9c5b6417e34d2b85ab97b61d1023df028b7e0d6b64268",
    "out/DS07-input.dta" = "47e710140d4f4d2368eb691ab1ca3da2145e9373eae72e4245af78f2926ffba0",
    "out/DS07.csv" = "1a4254cafe6b746b5bb2ae7bae9b83eba075af39cc4125fca7d9071c89ed01f8",
    "out/DS07.md" = "56a51dffd7767cc022e0f779ca5324a05d05a33aa847b470a1e4746a620cdcb4",
    "out/DS07.xlsx" = "865f378865554e921cfb330ebe71d4b0c9254cf6dfa440f5775b5157bed52810",
    "out/DS08-counts.csv" = "f1b916bb1057c15907626fbf1fd7e3ab742f97e664ba74f9cf3c5d160491aecd",
    "out/DS08-frame.dta" = "95ec72ad96424e7b42061a6f097e1ce762acbf7e719c9baa328fdea3cff6e837",
    "out/DS08-input.dta" = "e5928ddbc32b4b6d86fe967b721d866e654963ac35bb16235ff1200b6f739f2d",
    "out/DS08.csv" = "500864af175e7863c6b6e87cbcfd14fca1ae10a6a39838b915a849bb235cfcf0",
    "out/DS08.md" = "9fd50774cbc7cde16381ec301d1b1288c1c69de9c783a832234e5ed49538a7db",
    "out/DS08.xlsx" = "e2691cad046f5d32f4c1d7f1c2f995e1838c6d9dcec17ff7351c55757ed15485",
    "source-provenance.json" = "9e8d70e642db5592845cf5561c8c470fdd72425cb5ef254f2ecb268c2a90a01b"
  ),
  "weight-unit" = c(
    "capture.log" = "d88912d28a7fd31ac683930ba8c07a9ef1042808b61e9bedda277528352b3052",
    "cleanup.json" = "7ac1613f7121e73e7489b19a9c56cd9d42c58958813b44c3cd3166a4167b4cd0",
    "literal-oracles.json" = "5cb8c7d6fdff88d07ac08a6479b84de475d617b3c7f4a26d6452214c39f7e498",
    "native-receipt.json" = "58387bd10a956c76cda88832769cf0a7b85d13b4b51fa8f750bc47ce47463f89",
    "out/UL01-frame.dta" = "bbe1b51c3652134c0717709321ec904e21ede2671271ca5ae7845ec6814d724f",
    "out/UL01-input.dta" = "267ee263a30483b8c44ac5f8b8279cbe6a47e3f784b6e40c019d183979ddacc4",
    "out/UL01-saved.dta" = "c798442c05288e7e376a21b8ae356722d3385237693240623b1140bd2c341b71",
    "out/UL01.csv" = "d0db88c00d26d0c85c21045b92b51351bef26af3f9dc5d90617992fe287a59d1",
    "out/UL01.md" = "e84ca58aad5afb23d2f929b421125fd14fcf842e437254986353f8cb3a74ddc6",
    "out/UL01.xlsx" = "e53b30080efd93ee6281a18704ef533dc98d96cb806171b112a5b7f83d847900",
    "out/UL02-frame.dta" = "18497e991e9ff7ceb43800f60c624548872ae1cb79d2eef04cb0c178a481a6c4",
    "out/UL02-input.dta" = "03eca2e5d63909df9f1a9de2340143dff414f855699f96e2d5753c6850afbeda",
    "out/UL02-saved.dta" = "c74ebd88d44afbbc3506dc59493f1ffd185c91cf81193a3743e396317e3a5bca",
    "out/UL02.csv" = "92ab12bd6549a5881f46846cb9630922150477253abfe220f68de81b440ea27c",
    "out/UL02.md" = "ed01f24d4f76566984a7b4fc966b7bad6c716738427eef965a0d139ba9c1d6bd",
    "out/UL02.xlsx" = "5a11f7ab7304574ac2d87b10aa76f790e52c2c68c23939d0af1b727f6080f91f",
    "out/UL03-frame.dta" = "8d2aaa4f1b206cb2bb510a3d13437a4bd33bceadb7588375d0348f9756b329ed",
    "out/UL03-input.dta" = "7352af7563f6231ea1d94f641f369c2b1c1a6b5cb03ade339af2c405cd0e6b1f",
    "out/UL03-saved.dta" = "dbfa27879d5274a5c566274d5e87a692b582b039939b8a2fd8dc9734ba120e86",
    "out/UL03.csv" = "cb1b519c014621e9298e5efb5f6b24c8061904b4e356ce19fa1b95c5b315160a",
    "out/UL03.md" = "cf9ec85caddb28aae4b2b7f8bd410819dc8c012eea93ac02d3670411b6f0499f",
    "out/UL03.xlsx" = "6985a5f35bdffde924f17db2e7653c1a23949b3acc0e7ea73881ff26d4e6a84b",
    "out/UL04-frame.dta" = "8e259efd8dd508308540912e8d2ba4120edb8c78d90a69885795c89d0f4c7012",
    "out/UL04-input.dta" = "4f82e6db25e6ccda925838182a83bb3b682d2fcc326e45b120f77af6dc154c63",
    "out/UL04-saved.dta" = "4f14ec4a42b88f3c2ca2124a56d931966bc4727353da46187e21d24d0dd48b6a",
    "out/UL04.csv" = "d7247c9b24d03fa984d1ccee1b0f10276c42d1c5bd91c416f0951750beed86cc",
    "out/UL04.md" = "419e48dce4ee80be6ca932b37f336de35f91ba75a8d4904f050681009895bcdf",
    "out/UL04.xlsx" = "b1c06102a3270a2df806246bc6ead01050d45cfa05c87fe53bcca85315a30580",
    "out/WX01-huge-frame.dta" = "8cd0384a313a68db90c507ec6f285d12cac9ae909c807fdcd6fe38c6547ea7b1",
    "out/WX01-huge-input.dta" = "292cc677c716b9c486ab1b98a8862f5e3f470d84aba7e81992cd9de6cf12633c",
    "out/WX01-huge.csv" = "0a1f848539d248bff8cbca4fce8d5b57070e1534139fb7847314c042aa77dd77",
    "out/WX01-huge.md" = "eca4fa451fe76e8e5c9b29532f1c710e54250f7a61b740962a45f7ca761c4906",
    "out/WX01-huge.xlsx" = "2d1b233a3e9855d00e32ee5e1b4da4121d61f69630054bd7ea103a2ea054059e",
    "out/WX01-unit-frame.dta" = "d4c72be530b09da4879dbba922e571515884128a29dcfbba504513f9883581a1",
    "out/WX01-unit-input.dta" = "365d9d8344ac241cad7da51e81a0ad46ba61a97519f7ae86d9e93a9927b83e1e",
    "out/WX01-unit.csv" = "0a1f848539d248bff8cbca4fce8d5b57070e1534139fb7847314c042aa77dd77",
    "out/WX01-unit.md" = "eca4fa451fe76e8e5c9b29532f1c710e54250f7a61b740962a45f7ca761c4906",
    "out/WX01-unit.xlsx" = "2d1b233a3e9855d00e32ee5e1b4da4121d61f69630054bd7ea103a2ea054059e",
    "out/WX02-huge-frame.dta" = "fc25b9676332fa2dc3444ef5b49bd29a2511c0c1063916c788cafed4f96e49b4",
    "out/WX02-huge-input.dta" = "5093232d4bcc367f121a405e716416be29b25ff72d8f3d3aa4a2a215d66a3b88",
    "out/WX02-huge.csv" = "5c07bc6e2a35282e15fec758f674189e4dd7f0b7138a3b31a99f0f0d9a0ded6f",
    "out/WX02-huge.md" = "e769b8324b1edeaf27f27ef2a05cb9982af9217c6c0f3eac7d2981cd02c27da6",
    "out/WX02-huge.xlsx" = "d2341b6de28509762b396105135f9518f64255229da42b97acd9240ba53cc865",
    "out/WX02-unit-frame.dta" = "f5656035aa6e3204e785e0a569c39fed3441c20c827390568f5434ca268c9cbc",
    "out/WX02-unit-input.dta" = "e1d64c671bcd39533698f3ffc5c01e4a7215ae0b7f2feda685f838fef8227f00",
    "out/WX02-unit.csv" = "5c07bc6e2a35282e15fec758f674189e4dd7f0b7138a3b31a99f0f0d9a0ded6f",
    "out/WX02-unit.md" = "e769b8324b1edeaf27f27ef2a05cb9982af9217c6c0f3eac7d2981cd02c27da6",
    "out/WX02-unit.xlsx" = "d2341b6de28509762b396105135f9518f64255229da42b97acd9240ba53cc865",
    "out/WX03-huge-frame.dta" = "7c3258ea3015b42a2cdd71114f80cfbff37cb66ec473bec69bdfdc81ff1834c2",
    "out/WX03-huge-input.dta" = "0cc8e7b9a7ef64a999c11200d1597b46767e0be588f0292912fc873487591b4b",
    "out/WX03-huge.csv" = "d3518e8099896e8621e53a81f2d7d71524522ddf9b3a039a4374003818a109bc",
    "out/WX03-huge.md" = "a6ccbe83e1bfa5c0ab154a6e1fc27c267fe76abace6fbb8b72ad957c694536f3",
    "out/WX03-huge.xlsx" = "d3f7d118789c353cd1b3ef41909c35c2660a3667669eea40da056a59653bb7f9",
    "out/WX03-unit-frame.dta" = "bb5596a5558db12655622c1f2c90c5f9e7fc3ae1d9394c4d4556187d78536615",
    "out/WX03-unit-input.dta" = "6a3dde8a67649141624db4cd74acdf7fd41b4fa97cda33f3ee45b8e2737cdb77",
    "out/WX03-unit.csv" = "d3518e8099896e8621e53a81f2d7d71524522ddf9b3a039a4374003818a109bc",
    "out/WX03-unit.md" = "a6ccbe83e1bfa5c0ab154a6e1fc27c267fe76abace6fbb8b72ad957c694536f3",
    "out/WX03-unit.xlsx" = "d3f7d118789c353cd1b3ef41909c35c2660a3667669eea40da056a59653bb7f9",
    "out/WX04-huge-frame.dta" = "7826d793c557f77960c850c21b216741d580a05c2ecac7fe48a6ff9c3078ae5e",
    "out/WX04-huge-input.dta" = "2d70039e09cc5be36a3338b8ec9587eb981a314692ca2aba2a5fd2ec7e831ffd",
    "out/WX04-huge.csv" = "b818ba0841c14553775ccf19de5c8f523734f9530e54e14ef6f6d29140b7e14d",
    "out/WX04-huge.md" = "19a40336abc494723a4920b332da67722c5cf828759e7408ef0e6d5489dd58c1",
    "out/WX04-huge.xlsx" = "ef48f1c3783e48189f7e69b80956da61888095e6389d1ea125f0317fee2254da",
    "out/WX04-unit-frame.dta" = "57463df3fbe2bfcc53212b20384b2f0994e2b1c8266f1ab31eb6b819432c1449",
    "out/WX04-unit-input.dta" = "82d2b4a2bc3daa634e2b05ad619b78fbb21ecfa4af76d04d79ff7e26a2838587",
    "out/WX04-unit.csv" = "b818ba0841c14553775ccf19de5c8f523734f9530e54e14ef6f6d29140b7e14d",
    "out/WX04-unit.md" = "19a40336abc494723a4920b332da67722c5cf828759e7408ef0e6d5489dd58c1",
    "out/WX04-unit.xlsx" = "ef48f1c3783e48189f7e69b80956da61888095e6389d1ea125f0317fee2254da",
    "source-provenance.json" = "9e8d70e642db5592845cf5561c8c470fdd72425cb5ef254f2ecb268c2a90a01b"
  )
)
