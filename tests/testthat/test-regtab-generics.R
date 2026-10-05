# The regtab extension generics (R/regtab_generics.R) after the Phase 5a/5b
# merge: one gate, one definition per generic.

s3_methods <- function() {
  m <- getNamespaceInfo(asNamespace("tabtools"), "S3methods")
  data.frame(generic = m[, 1], class = m[, 2], stringsAsFactors = FALSE)
}

test_that("every class with a Phase 5 method declares tt_regtab_adapter()", {
  m <- s3_methods()
  hooks <- c("tt_coef", "tt_vcov", "tt_wald_df", "tt_regtab_rows", "tt_regtab_ancillary_rows",
             "tt_regtab_trailing_rows", "tt_model_stats")
  # coxph and negbin are Phase 4 classes; their methods only add Fine-Gray
  # handling and nbreg's lnalpha/alpha rows.
  cls <- setdiff(unique(m$class[m$generic %in% hooks]), c("default", "coxph", "negbin"))
  declared <- m$class[m$generic == "tt_regtab_adapter" & m$class != "default"]
  expect_setequal(declared, cls)
})

test_that("the model gate is an allow-list of first classes (Phase 5 review P0-10)", {
  ok <- names(tabtools:::.rt_supported_classes)
  expect_setequal(ok, c("lm", "glm", "negbin", "coxph", "polr", "clm", "multinom", "zeroinfl", "hurdle",
                        "survreg", "tobit", "crr", "lmerMod", "glmerMod", "glmmTMB", "geeglm",
                        "svyglm", "glm_weightit", "clogit", "lme"))
  # Every allowed class beyond Phase 4's lm/glm/negbin/coxph reaches an
  # adapter (lmerMod/glmerMod through merMod, tobit through survreg).
  m <- s3_methods()
  declared <- m$class[m$generic == "tt_regtab_adapter" & m$class != "default"]
  via <- c(lmerMod = "merMod", glmerMod = "merMod", tobit = "survreg")
  p5 <- setdiff(ok, c("lm", "glm", "negbin", "coxph"))
  expect_true(all(ifelse(p5 %in% names(via), via[p5], p5) %in% declared))
})

test_that("each generic has exactly one default method and one definition in R/", {
  m <- s3_methods()
  gens <- c("tt_regtab_adapter", "tt_coef", "tt_vcov", "tt_wald_df", "tt_regtab_rows",
            "tt_regtab_ancillary_rows", "tt_regtab_trailing_rows", "tt_model_stats")
  for (g in gens) {
    expect_true(is.function(get(g, asNamespace("tabtools"))), label = g)
    expect_identical(sum(m$generic == g & m$class == "default"), 1L, label = g)
  }
  # Source check (a duplicate definition is no git conflict, and the later
  # one silently wins at load time). Only where the sources are at hand.
  src <- test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  defs <- unlist(lapply(list.files(src, "[.]R$", full.names = TRUE), function(f) {
    x <- readLines(f, warn = FALSE)
    sub(" <- function.*", "", grep("^[A-Za-z._][A-Za-z0-9._]* <- function", x, value = TRUE))
  }))
  expect_identical(unique(defs[duplicated(defs)]), character())
})

test_that("tt_vcov() has one signature for both families", {
  # Milestone 5w: cluster became a formal of the exported generic; the 5w
  # review (P2-3) added `complete` after the dots.
  expect_identical(names(formals(tabtools::tt_vcov)), c("fit", "vce", "cluster", "...", "complete"))
  expect_identical(names(formals(tabtools:::tt_wald_df)), c("fit", "vce", "cluster", "..."))
  m <- s3_methods()
  for (g in c("tt_vcov", "tt_wald_df")) {
    for (cl in m$class[m$generic == g]) {
      f <- utils::getS3method(g, cl, envir = asNamespace("tabtools"))
      expect_identical(names(formals(f))[1:3], c("fit", "vce", "cluster"), label = paste(g, cl))
      expect_true("..." %in% names(formals(f)), label = paste(g, cl))
    }
  }
})
