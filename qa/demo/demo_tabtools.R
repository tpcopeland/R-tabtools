# R twin of the authenticated Stata tabtools 2.5.1 demo, pin712044f8.
# All103 source headings appear in order. The actual runtime manifest records
# 102 native sheets in16 books,56 console commands and4 report tables.
# Implemented examples compare against their own native demo artifacts;
# Phase 7B supplies the implemented command and layout examples below;
# their new comparison enrollment remains pending independent review/execution.
# Source-only datasets come from the accepted eight native snapshots. Run this
# script in a private environment with optional out_dir/demo_data_file values.

local({
# **# Setup ----
library(tabtools)
missing_pkgs <- Filter(function(pkg) !requireNamespace(pkg, quietly = TRUE),
                       c("haven", "survival", "lme4", "geepack", "MASS", "nnet", "pscl",
                         "WeightIt", "marginaleffects"))
if (length(missing_pkgs)) {
  stop("demo_tabtools.R needs the package(s) ", paste(missing_pkgs, collapse = ", "),
       ": install.packages(c(", paste0("\"", missing_pkgs, "\"", collapse = ", "), "))")
}

# Stata writes into tabtools/demo; R writes into out_dir, set in the
# environment the script runs in (only there: an out_dir elsewhere on the
# search path is not used).
out_dir <- get0("out_dir", envir = environment(), inherits = TRUE,
                ifnotfound = file.path(tempdir(), "tabtools_demo"))
# The demo's datasets (qa/make_demo_data.R): demo_data_file, set in the
# environment the script runs in as out_dir is, else qa/demo/demo_tabtools.rds
# relative to the working directory (the root of the repository).
demo_data_file <- get0("demo_data_file", envir = environment(), inherits = TRUE,
                       ifnotfound = file.path("qa", "demo", "demo_tabtools.rds"))
if (!file.exists(demo_data_file)) {
  stop("demo_tabtools.R needs its datasets, qa/demo/demo_tabtools.rds: run it from the root of ",
       "the tabtools repository, or set demo_data_file to the file's path")
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

xlsx_table1        <- file.path(out_dir, "demo_table1.xlsx")
xlsx_desctab       <- file.path(out_dir, "demo_desctab.xlsx")
xlsx_regtab        <- file.path(out_dir, "demo_regtab.xlsx")
xlsx_regtab_models <- file.path(out_dir, "demo_regtab_models.xlsx")
xlsx_comptab       <- file.path(out_dir, "demo_comptab.xlsx")
xlsx_effecttab     <- file.path(out_dir, "demo_effecttab.xlsx")
xlsx_stratetab     <- file.path(out_dir, "demo_stratetab.xlsx")
xlsx_hrcomptab     <- file.path(out_dir, "demo_hrcomptab.xlsx")
xlsx_puttab        <- file.path(out_dir, "demo_puttab.xlsx")
xlsx_stacktab      <- file.path(out_dir, "demo_stacktab.xlsx")
xlsx_ratetab       <- file.path(out_dir, "demo_ratetab.xlsx")
xlsx_outtab        <- file.path(out_dir, "demo_outtab.xlsx")
xlsx_tabcell       <- file.path(out_dir, "demo_tabcell.xlsx")
xlsx_corrtab       <- file.path(out_dir, "demo_corrtab.xlsx")
xlsx_crosstab      <- file.path(out_dir, "demo_crosstab.xlsx")
xlsx_survtab       <- file.path(out_dir, "demo_survtab.xlsx")
markdown_report    <- file.path(out_dir, "demo_markdown_report.md")
console_log        <- file.path(out_dir, "console_output.log")
console_md         <- file.path(out_dir, "console_output.md")

# The demo runs with no session defaults, as the Stata demo does; the
# session's tabtools_options() come back at the end.
user_tabtools_options <- tabtools_options()
on.exit({
  tabtools_options(clear = TRUE)
  if (length(user_tabtools_options)) do.call(tabtools_options, user_tabtools_options)
}, add = TRUE)
tabtools_options(clear = TRUE)

# Erase prior demo artifacts before regenerating the full documentation set.
unlink(c(xlsx_table1, xlsx_desctab, xlsx_regtab, xlsx_regtab_models, xlsx_comptab, xlsx_effecttab,
         xlsx_stratetab, xlsx_hrcomptab, xlsx_puttab, xlsx_stacktab, markdown_report, console_log,
         console_md, xlsx_ratetab, xlsx_outtab, xlsx_tabcell, xlsx_corrtab, xlsx_crosstab, xlsx_survtab))

# Stata logs the console sections with `log using console_output.log` and
# `log on demo` / `log off demo`. console() is the R twin: it runs one call,
# shows what R shows at the prompt (a visible result is printed; messages
# are kept), and appends the call and that output to console_output.log
# (and, as Markdown, to console_output.md).
console <- function(expr) {
  call <- substitute(expr)
  msgs <- character()
  printed <- utils::capture.output({
    res <- withCallingHandlers(
      withVisible(eval(call, parent.frame())),
      message = function(m) {
        msgs <<- c(msgs, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      })
    if (res$visible) print(res$value)
  })
  out <- c(printed, msgs)
  code <- deparse(call, width.cutoff = 72L)
  writeLines(out)
  append <- function(path, lines) {
    con <- file(path, open = "a", encoding = "UTF-8")
    on.exit(close(con))
    writeLines(lines, con)
  }
  append(console_log, c(paste0(c("> ", rep("+ ", length(code) - 1L)), code), out))
  append(console_md, c("```r", code, "```", "", if (length(out)) c("```", out, "```", "")))
  invisible(res$value)
}

# The workbook checks of the Stata demo (import excel ..., allstring): the
# text of every cell of a sheet, as a character matrix.
sheet_text <- function(xlsx, sheet) {
  d <- openxlsx2::read_xlsx(xlsx, sheet = sheet, col_names = FALSE, skip_empty_rows = FALSE,
                            skip_empty_cols = FALSE)
  m <- as.matrix(d)
  m[is.na(m)] <- ""
  m
}

# **# Build analysis dataset ----
# Stata:
#   use _data/cohort.dta, clear
#   merge 1:1 id using _data/treatment.dta, nogen keep(match)
#   merge 1:1 id using _data/comorbidities.dta, nogen keep(master match) nolabel
#   merge 1:1 id using _data/outcomes.dta, nogen keep(master match)
#   (comorbidities filled with 0, cv_event, female labels, follow_up)
#   stset follow_up, failure(cv_event)
#   quietly logit treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   predict double ps, pr
#   gen double iptw = cond(treated, 1/ps, 1/(1-ps))
#   set seed 20260324
#   gen double crp = exp(rnormal(1.5, 0.8))
#   gen byte prior_hosp = rpoisson(1.8)
#   gen byte rare_event = runiform() < 0.03
#   gen byte smoking = ...   (Never/Former/Current, ~10% missing)
#
# R cannot redraw Stata's rnormal()/rpoisson()/runiform() streams, so the
# demo reads the dataset Stata built, identical draw for draw
# (demo_tabtools.do:130-191; qa/demo/demo_tabtools.rds also holds the
# demo's other datasets). Value-labelled variables arrive as
# haven_labelled columns, which the tabtools functions read directly.
demo_data <- readRDS(demo_data_file)
analysis <- demo_data$cohort

# The IPTW weights in R: the same logistic propensity model, fitted with
# glm(), gives Stata's weights (to the fit's convergence tolerance). The
# tables below use the stored analysis$iptw, Stata's own, so they match its
# cells exactly; this only shows that R reproduces them.
ps <- fitted(glm(treated ~ index_age + female + factor(education) + diabetes +
                   hypertension + anxiety + prior_cvd, family = binomial, data = analysis))
iptw <- ifelse(analysis$treated == 1, 1 / ps, 1 / (1 - ps))
stopifnot(isTRUE(all.equal(iptw, analysis$iptw, tolerance = 1e-6, check.attributes = FALSE)))

# Stata's i.education and i.civil_status: factors that keep the value labels
# and codes (tt_as_factor()), so regression tables show "Primary",
# "Secondary", ... and Stata's level keys work in keep()/drop().
analysis <- tt_as_factor(analysis, vars = c("education", "civil_status"))

# **# Console: consolidated display log ----
# Stata: log using console_output.log, replace text name(demo) nomsg
# R: console() above appends to console_output.log and console_output.md.

# **# Console: tabtools set/get/list/detail ----
# Stata:
#   tabtools set font Calibri
#   tabtools set fontsize 11
#   tabtools set borderstyle thin
#   tabtools get
#   tabtools set clear
console(tabtools_options(font = "Calibri"))
console(tabtools_options(fontsize = 11))
console(tabtools_options(borderstyle = "thin"))
console(tabtools_options())
console(tabtools_options(clear = TRUE))

# Stata: noisily tabtools
#        noisily tabtools, detail
# R: the command list is the package help index, help(package = "tabtools").

# **# Console: table1_tc display ----
# Stata:
#   noisily table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ civil_status cat \ ///
#            diabetes bin \ hypertension bin \ anxiety bin \ prior_cvd bin)
console(table1_tc(analysis, by = "treated",
                  vars = c(index_age = "contn %5.1f", female = "bin",
                           education = "cat", income_quintile = "cat",
                           born_abroad = "bin", civil_status = "cat",
                           diabetes = "bin", hypertension = "bin", anxiety = "bin", prior_cvd = "bin")))

# **# Console: table1_tc nopvalue + smd ----
# Stata:
#   noisily table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ diabetes bin \ hypertension bin) ///
#       nopvalue smd
console(table1_tc(analysis, by = "treated",
                  vars = c(index_age = "contn %5.1f", female = "bin",
                           education = "cat", income_quintile = "cat",
                           born_abroad = "bin", diabetes = "bin", hypertension = "bin"),
                  nopvalue = TRUE, smd = TRUE))

# **# Console: desctab display ----
# Stata:
#   noisily desctab education cv_event, ///
#       vars(education cat \ cv_event bin)
console(desctab(analysis, vars = c(education = "cat", cv_event = "bin")))

# **# Console: survtab RMST + difference ----
# Stata:
#   noisily survtab, times(365 730 1095 1460) by(treated) ///
#       rmst(1460) difference median timeunit(days)
console(survtab(demo_data$cohort, time = "follow_up", event = "cv_event",
                times = c(365, 730, 1095, 1460), by = "treated",
                rmst = 1460, difference = TRUE, median = TRUE, timeunit = "days"))

# **# Console: regtab display ----
# Stata:
#   collect clear
#   quietly collect: logistic treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   noisily regtab, coef("OR") noint
ps_model <- glm(treated ~ index_age + female + education + diabetes + hypertension +
                  anxiety + prior_cvd, family = binomial, data = analysis)
console(regtab(ps_model, coef = "OR", nointercept = TRUE))

# **# Console: regtab compact display ----
# Stata: (the same logistic model)
#   noisily regtab, coef("OR") noint compact
console(regtab(ps_model, coef = "OR", nointercept = TRUE, compact = TRUE))

# **# Console: regtab nopvalue display ----
# Stata: (the same logistic model)
#   noisily regtab, coef("OR") noint nopvalue
console(regtab(ps_model, coef = "OR", nointercept = TRUE, nopvalue = TRUE))

# **# Console: regtab multinomial logit display ----
# Stata:
#   quietly collect: mlogit education index_age female diabetes hypertension, ///
#       baseoutcome(1)
#   noisily regtab, stats(n ll aic bic r2)
# R: nnet::multinom() uses the first level (Primary) as the base outcome.
mlogit_model <- nnet::multinom(education ~ index_age + female + diabetes + hypertension,
                               data = analysis, trace = FALSE)
console(regtab(mlogit_model, stats = c("n", "ll", "aic", "bic", "r2")))

# **# Console: regtab zero-inflated display ----
# Stata:
#   clear
#   set seed 20260601
#   set obs 1500
#   gen byte treatment = runiform() < 0.45
#   gen double age_z = rnormal()
#   ... (event_count: Poisson counts with structural zeros)
#   quietly collect: zip event_count treatment age_z female, inflate(zero_risk female)
#   quietly collect: zinb event_count treatment age_z female, inflate(zero_risk female)
#   noisily regtab, stats(n aic bic ll) models("ZIP" \ "ZINB")
# R: the data Stata drew (demo_data$zip); pscl::zeroinfl() with the
# inflation model after the "|".
zip_data <- demo_data$zip
zip_model <- pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female,
                            data = zip_data, dist = "poisson")
zinb_model <- pscl::zeroinfl(event_count ~ treatment + age_z + female | zero_risk + female,
                             data = zip_data, dist = "negbin")
console(regtab(zip_model, zinb_model, stats = c("n", "aic", "bic", "ll"), models = c("ZIP", "ZINB")))

# **# Console: regtab hurdle display ----
# Stata (no R fitter for churdle, Cragg's hurdle model; regtab() takes the
# estimates as a data frame instead, see ?regtab):
#   quietly collect: churdle linear annual_cost dose_intensity, ///
#       select(participation_score) ll(0)
#   noisily regtab, stats(n ll aic bic r2)

# **# Console: corrtab display ----
# Stata:
#   noisily corrtab index_age crp prior_hosp, ///
#       star(0.05 0.01 0.001)
console(corrtab(demo_data$cohort, c("index_age", "crp", "prior_hosp"), star = c(.05, .01, .001)))

# **# Console: crosstab display ----
# Stata:
#   noisily crosstab treated female, or label
console(crosstab(demo_data$cohort, "treated", "female", or = TRUE, label = TRUE))

# **# Console: smallcells() disclosure control ----
# Stata builds three small tables from frequencies:
#   input byte group byte category int frequency
#   0 0 2
#   0 1 3
#   1 0 3
#   1 1 2
#   end
#   expand frequency
#   (value labels Control/Treatment and Absent/Present)
# R: one row per person with rep(); haven::labelled() attaches the value
# labels to the 0/1 codes, as Stata's label values does.
small_table <- function(counts, var, labels, label) {
  d <- data.frame(group = haven::labelled(rep(c(0, 0, 1, 1), counts),
                                          c(Control = 0, Treatment = 1), label = "Study group"),
                  x = haven::labelled(rep(c(0, 1, 0, 1), counts), labels, label = label))
  names(d)[2] <- var
  d
}
sc_primary    <- small_table(c(2, 3, 3, 2), "category", c(Absent = 0, Present = 1), "Characteristic")
sc_complement <- small_table(c(2, 8, 6, 4), "category", c(Absent = 0, Present = 1), "Characteristic")
sc_binary     <- small_table(c(48, 2, 47, 3), "rare_ae", c(No = 0, Yes = 1), "Rare adverse event")

# ## Primary suppression only: table1_tc
# Stata: noisily table1_tc category, by(group) vars(category cat) ///
#            total(after) smallcells(5)
console(table1_tc(sc_primary, by = "group", vars = c(category = "cat"),
                  total = "after", smallcells = 5))

# ## Primary suppression only: desctab
# Stata: noisily desctab category, by(group) vars(category cat) ///
#            total(after) smallcells(5)
console(desctab(sc_primary, by = "group", vars = c(category = "cat"),
                total = "after", smallcells = 5))

# ## Primary suppression only: crosstab
# Stata: noisily crosstab group category, label smallcells(5)
console(crosstab(sc_primary, "group", "category", label = TRUE, smallcells = 5))

# ## Complementary suppression: table1_tc
# Stata: (frequencies 2 8 6 4)
#   noisily table1_tc category, by(group) vars(category cat) ///
#       total(after) smallcells(5)
console(table1_tc(sc_complement, by = "group", vars = c(category = "cat"),
                  total = "after", smallcells = 5))

# ## Complementary suppression: desctab
# Stata: noisily desctab category, by(group) vars(category cat) ///
#            total(after) smallcells(5)
console(desctab(sc_complement, by = "group", vars = c(category = "cat"),
                total = "after", smallcells = 5))

# ## Complementary suppression: crosstab
# Stata: noisily crosstab group category, label smallcells(5)
console(crosstab(sc_complement, "group", "category", label = TRUE, smallcells = 5))

# ## Binary variable suppression: table1_tc
# Stata: (rare_ae, frequencies 48 2 47 3)
#   noisily table1_tc rare_ae, by(group) vars(rare_ae bin) ///
#       total(after) smallcells(5)
console(table1_tc(sc_binary, by = "group", vars = c(rare_ae = "bin"),
                  total = "after", smallcells = 5))

# ## Binary variable suppression: desctab
# Stata: noisily desctab rare_ae, by(group) vars(rare_ae bin) ///
#            total(after) smallcells(5)
console(desctab(sc_binary, by = "group", vars = c(rare_ae = "bin"),
                total = "after", smallcells = 5))

# **# Console: smallcells(#, primary) and cellreplace() ----
# Native 2/8/6/4 counts; no source RNG is redrawn.
console(table1_tc(sc_complement, by = "group", vars = c(category = "cat"),
                  total = "after", smallcells = 5, smallcells_mode = "primary"))
console(table1_tc(analysis, by = "treated", vars = c(education = "cat", civil_status = "cat"),
                  cellreplace = list(list(row = "Widowed", column = "SNRI", text = "Not reported"))))

# **# Console: tabtools session settings ----
# The native query publishes Stata globals; R publishes named options.
# Semantics are checked here and inherited workbook publication below.
console(tabtools_options(smallcells = 5, smallcells_mode = "primary"))
console(tabtools_options())
stopifnot(identical(tabtools_options()$smallcells, 5L),
          identical(tabtools_options()$smallcells_mode, "primary"))
console(tabtools_options(smallcells = NULL, smallcells_mode = NULL))
stopifnot(is.null(tabtools_options()$smallcells), is.null(tabtools_options()$smallcells_mode))

# **# Console: tabcell single cells ----
cell_model <- glm(cv_event ~ treated + index_age + female, family = binomial, data = analysis)
console(tabcell("est", model = cell_model, term = "treated", eform = TRUE))
console(tabcell("est", model = cell_model, term = "treated", eform = TRUE,
                format = "%5.3f", sep = " to "))
# Native lincom treated + female: a normal coefficient-scale contrast.
cell_contrast <- list(estimate = sum(coef(cell_model)[c("treated", "female")]),
  std.error = sqrt(sum(vcov(cell_model)[c("treated", "female"), c("treated", "female")])),
  df = Inf, conf.level = .95, effect_scale = "coefficient", native_source = "lincom")
console(tabcell("est", contrast = cell_contrast, eform = TRUE))
console(tabcell("p", p = .0004))
console(tabcell("np", n = 3, d = 40, mincell = 5))
console(tabcell("enp", e = 2149, n = 6066))
followup_quartiles <- quantile(analysis$follow_up, c(.25, .5, .75), type = 2, na.rm = TRUE, names = FALSE)
console(tabcell("iqr", median = followup_quartiles[2], q1 = followup_quartiles[1],
                q3 = followup_quartiles[3], format = "%6.0fc"))

# **# Console: ratetab incidence rates ----
# Explicit person-years reproduce the native stset scale(365.25).
rate_data <- demo_data$cohort
rate_data$person_years <- rate_data$follow_up / 365.25
console(ratetab(rate_data, c("treated", "education"), events = "cv_event", exposure = "person_years",
                outlabels = "CV events", explabels = c("Treatment", "Education")))
console(ratetab(rate_data, "treated", events = "cv_event", exposure = "person_years",
                ci = "cluster", cluster = "region", outlabels = "CV events", cformat = "%5.2f", sep = " to "))

# **# Console: outtab binary outcomes by exposure ----
console(outtab(analysis, c("cv_event", "selfharm", "fracture", "gi_bleed"), exposure = "treated",
               models = list(~1, ~index_age + female + education + diabetes + hypertension),
               modellabels = c("Crude", "Adjusted"), estimator = "modified_poisson", ratiolabel = "RR"))

# **# Console: regtab 2.3 layout options ----
layout_crude <- glm(cv_event ~ treated, family = binomial, data = analysis)
layout_adjusted <- glm(cv_event ~ treated + index_age + female + education + diabetes + hypertension,
                       family = binomial, data = analysis)
console(regtab(layout_crude, layout_adjusted, cformat = "%5.3f", sep = " to ",
               nointercept = TRUE, models = c("Crude", "Adjusted")))
console(regtab(layout_crude, layout_adjusted, transpose = TRUE, keep = "treated", stats = "n",
               nopvalue = TRUE, models = c("Crude", "Adjusted")))
console(regtab(layout_crude, layout_adjusted, nointercept = TRUE, nopvalue = TRUE,
               cellnote = list(list(row = "Diabetes", model = 1L, text = "Not in model")),
               models = c("Crude", "Adjusted")))

fit_data <- tt_as_factor(demo_data$cohort[demo_data$cohort$index_age >= 80, ], vars = "region")
fit_data$person_years <- fit_data$follow_up / 365.25
fit_crude <- survival::coxph(survival::Surv(person_years, gi_bleed) ~ region, data = fit_data, ties = "breslow", model = TRUE, x = TRUE)
counts_crude <- tt_fitcount(fit_crude, events = "gi_bleed", people = "id", exposure = "person_years", terms = TRUE, data = fit_data)
fit_adjusted <- survival::coxph(survival::Surv(person_years, gi_bleed) ~ region + treated + female,
                              data = fit_data, ties = "breslow", model = TRUE, x = TRUE)
counts_adjusted <- tt_fitcount(fit_adjusted, events = "gi_bleed", people = "id", exposure = "person_years", terms = TRUE, data = fit_data)
console(regtab(fit_crude, fit_adjusted, fitcounts = list(counts_crude, counts_adjusted),
               stats = c("events", "people", "exposure"), exposurelabel = "Person-years", mincount = 15,
               models = c("Crude", "Adjusted")))

# **# Console: puttab + stacktab export pipeline ----
# Emit two styled estimate blocks with puttab, then assemble them into one
# composite sheet with stacktab (vstack, column merge, section labels).
# Stata:
#   clear
#   input str22 term str10 ahr str16 ci
#   "Any HRT"        "0.82" "(0.69, 0.98)"
#   "Former smoker"  "1.14" "(0.97, 1.34)"
#   "Current smoker" "1.46" "(1.21, 1.77)"
#   end
#   label variable term "Exposure"
#   label variable ahr "aHR"
#   label variable ci "95% CI"
pipe_xlsx <- file.path(out_dir, "_pipeline_parts.xlsx")
unlink(pipe_xlsx)
block_primary <- data.frame(term = c("Any HRT", "Former smoker", "Current smoker"),
                            ahr = c("0.82", "1.14", "1.46"),
                            ci = c("(0.69, 0.98)", "(0.97, 1.34)", "(1.21, 1.77)"))
attr(block_primary$term, "label") <- "Exposure"
attr(block_primary$ahr, "label") <- "aHR"
attr(block_primary$ci, "label") <- "95% CI"

# Stata: noisily puttab term ahr ci using "`_pipe_xlsx'", sheet("Block Primary") ///
#            title("Source block: Primary HRT exposure model") varlabels
console(puttab(block_primary, xlsx = pipe_xlsx, sheet = "Block Primary",
               title = "Source block: Primary HRT exposure model", varlabels = TRUE))

# Stata:
#   clear
#   input str22 term str10 ahr str16 ci
#   "Low dose"  "0.91" "(0.74, 1.12)"
#   "High dose" "0.73" "(0.58, 0.92)"
#   end
#   (the same variable labels)
block_dose <- data.frame(term = c("Low dose", "High dose"), ahr = c("0.91", "0.73"),
                         ci = c("(0.74, 1.12)", "(0.58, 0.92)"))
attr(block_dose$term, "label") <- "Exposure"
attr(block_dose$ahr, "label") <- "aHR"
attr(block_dose$ci, "label") <- "95% CI"

# Stata: noisily puttab term ahr ci using "`_pipe_xlsx'", sheet("Block Dose") ///
#            title("Source block: Estrogen dose-response model") varlabels
console(puttab(block_dose, xlsx = pipe_xlsx, sheet = "Block Dose",
               title = "Source block: Estrogen dose-response model", varlabels = TRUE))

# Stata:
#   noisily stacktab using "`_pipe_xlsx'", sheet("Composite") ///
#       blocks(sheet(Block Primary) rows(2/5) cols(B-D) label(Any HRT use) \ ///
#              sheet(Block Dose) rows(2/4) cols(B-D) label(By estrogen dose)) ///
#       columnmerge(B+C as "aHR (95% CI)") ///
#       display ///
#       title("Hormone therapy and recurrent events") ///
#       note("aHR = adjusted hazard ratio; CI = confidence interval.")
# R: blocks() is the same Stata-style string.
hrt_blocks <- paste("sheet(Block Primary) rows(2/5) cols(B-D) label(Any HRT use) \\",
                    "sheet(Block Dose) rows(2/4) cols(B-D) label(By estrogen dose)")
console(stacktab(hrt_blocks, xlsx = pipe_xlsx, sheet = "Composite",
                 columnmerge = "B+C as \"aHR (95% CI)\"",
                 display = TRUE,
                 title = "Hormone therapy and recurrent events",
                 note = "aHR = adjusted hazard ratio; CI = confidence interval."))
unlink(pipe_xlsx)

# **# Console: Markdown export report ----
# ## Markdown report export
# Stata:
#   noisily table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ diabetes bin \ hypertension bin) ///
#       title("Table 1. Baseline Characteristics") ///
#       markdown("`markdown_report_export'")
# R: a table written to a file returns invisibly; print() shows it, as
# Stata does.
console(print(table1_tc(analysis, by = "treated",
                        vars = c(index_age = "contn %5.1f", female = "bin",
                                 diabetes = "bin", hypertension = "bin"),
                        title = "Table 1. Baseline Characteristics",
                        markdown = markdown_report)))

# Stata:
#   noisily crosstab treated female, or label ///
#       title("Table 2. Treatment by Sex") ///
#       markdown("`markdown_report_export'") mdappend
#   noisily corrtab index_age crp prior_hosp, star(0.05 0.01 0.001) ///
#       title("Table 3. Correlation Matrix") ///
#       markdown("`markdown_report_export'") mdappend
console(print(crosstab(demo_data$cohort, "treated", "female", or = TRUE, label = TRUE,
                        title = "Table 2. Treatment by Sex", markdown = markdown_report, mdappend = TRUE)))
console(print(corrtab(demo_data$cohort, c("index_age", "crp", "prior_hosp"), star = c(.05, .01, .001),
                       title = "Table 3. Correlation Matrix", markdown = markdown_report, mdappend = TRUE)))

# Stata:
#   preserve
#   keep id index_age treated female cv_event
#   keep in 1/6
#   noisily puttab id index_age treated female cv_event, ///
#       varlabels ///
#       title("Table 4. First Six Analysis Records") ///
#       markdown("`markdown_report_export'") mdappend
#   restore
console(puttab(analysis, vars = c("id", "index_age", "treated", "female", "cv_event"),
               subset = 1:6, varlabels = TRUE,
               title = "Table 4. First Six Analysis Records",
               markdown = markdown_report, mdappend = TRUE))

# Stata:
#   noisily display as text "Markdown report written to tabtools/demo/demo_markdown_report.md"
#   noisily display as text "The file contains multiple GitHub-Flavored Markdown tables appended in one report."
#   noisily type "`markdown_report'"
console(writeLines(paste("Markdown report written to", markdown_report)))
console(writeLines("The file contains multiple GitHub-Flavored Markdown tables appended in one report."))
console(writeLines(readLines(markdown_report, encoding = "UTF-8")))

# **# Verify Markdown report content ----
# Stata: file read ... assert the four "### Table N" headings and a "| --- |"
# row. Tables 2 and 3 come from crosstab and corrtab, which R does not have.
report <- readLines(markdown_report, encoding = "UTF-8")
stopifnot(any(grepl("### Table 1. Baseline Characteristics", report, fixed = TRUE)),
          any(grepl("### Table 4. First Six Analysis Records", report, fixed = TRUE)),
          any(grepl("| --- |", report, fixed = TRUE)))

# **# Sheet 1: Table 1 -- Baseline Characteristics ----
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ civil_status cat \ ///
#            diabetes bin \ hypertension bin \ anxiety bin \ prior_cvd bin) ///
#       title("Table 1. Baseline Characteristics by Treatment Group") ///
#       excel("`xlsx_table1'") sheet("Table 1")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", income_quintile = "cat",
                   born_abroad = "bin", civil_status = "cat",
                   diabetes = "bin", hypertension = "bin", anxiety = "bin", prior_cvd = "bin"),
          title = "Table 1. Baseline Characteristics by Treatment Group",
          xlsx = xlsx_table1, sheet = "Table 1")

# **# Sheet 2: Table 1 with Total -- Adds a total column ----
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ diabetes bin \ hypertension bin) ///
#       total(after) ///
#       title("Table 1. Baseline Characteristics (with Total)") ///
#       excel("`xlsx_table1'") sheet("Table 1 Total")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", income_quintile = "cat",
                   born_abroad = "bin", diabetes = "bin", hypertension = "bin"),
          total = "after",
          title = "Table 1. Baseline Characteristics (with Total)",
          xlsx = xlsx_table1, sheet = "Table 1 Total")

# **# Sheet 3: Table 1 Weighted -- IPTW-weighted descriptives ----
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ diabetes bin \ hypertension bin) ///
#       wt(iptw) ///
#       title("Table 1. Weighted Baseline Characteristics (IPTW)") ///
#       excel("`xlsx_table1'") sheet("Table 1 Weighted")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", income_quintile = "cat",
                   born_abroad = "bin", diabetes = "bin", hypertension = "bin"),
          wt = "iptw",
          title = "Table 1. Weighted Baseline Characteristics (IPTW)",
          xlsx = xlsx_table1, sheet = "Table 1 Weighted")

# **# Sheet 4: Table 1 WtCompare -- Crude vs weighted side-by-side ----
# Demonstrates: wtcompare shows unweighted and IPTW-weighted columns together
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            diabetes bin \ hypertension bin \ anxiety bin \ prior_cvd bin) ///
#       wt(iptw) wtcompare smd ///
#       title("Table 1. Crude vs IPTW-Weighted Baseline Characteristics") ///
#       footnote("Crude and IPTW-weighted statistics shown side-by-side with SMD.") ///
#       excel("`xlsx_table1'") sheet("Table 1 WtCompare")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", income_quintile = "cat",
                   diabetes = "bin", hypertension = "bin", anxiety = "bin", prior_cvd = "bin"),
          wt = "iptw", wtcompare = TRUE, smd = TRUE,
          title = "Table 1. Crude vs IPTW-Weighted Baseline Characteristics",
          footnote = "Crude and IPTW-weighted statistics shown side-by-side with SMD.",
          xlsx = xlsx_table1, sheet = "Table 1 WtCompare")

# **# Sheet 5: Table 1 Stats -- SMD + test + statistic + boldp + zebra ----
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ crp contln %5.1f \ ///
#            prior_hosp conts \ female bin \ ///
#            education cat \ income_quintile cat \ ///
#            born_abroad bin \ diabetes bin \ hypertension bin \ ///
#            anxiety bin \ prior_cvd bin) ///
#       smd test statistic boldp(0.05) zebra ///
#       footnote("SMD = standardized mean difference. Bold p-values indicate p < 0.05.") ///
#       title("Table 1. Baseline Characteristics with Balance Diagnostics") ///
#       excel("`xlsx_table1'") sheet("Table 1 Stats")
# R's tests are R's conventional ones (Welch t test, ...), so the test
# names, statistics and some p-values differ from Stata's.
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", crp = "contln %5.1f",
                   prior_hosp = "conts", female = "bin",
                   education = "cat", income_quintile = "cat",
                   born_abroad = "bin", diabetes = "bin", hypertension = "bin",
                   anxiety = "bin", prior_cvd = "bin"),
          smd = TRUE, test = TRUE, statistic = TRUE, boldp = 0.05, zebra = TRUE,
          footnote = "SMD = standardized mean difference. Bold p-values indicate p < 0.05.",
          title = "Table 1. Baseline Characteristics with Balance Diagnostics",
          xlsx = xlsx_table1, sheet = "Table 1 Stats")

# **# Sheet 6: Table 1 Formats -- Alternative formatting options ----
# Demonstrates: conts, contln, bine, cate, missing, percent_n, headerperc,
#               highlight(), varlabplus, nospacelowpercent
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ crp contln %5.1f \ ///
#            prior_hosp conts \ female bin \ ///
#            rare_event bine \ smoking cate \ ///
#            education cat \ civil_status cat) ///
#       missing percent_n headerperc varlabplus nospacelowpercent ///
#       highlight(0.05) ///
#       title("Table 1. Alternative Formatting Options Demo") ///
#       footnote("Yellow rows indicate p < 0.05. Missing values shown as separate category.") ///
#       excel("`xlsx_table1'") sheet("Table 1 Formats")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", crp = "contln %5.1f",
                   prior_hosp = "conts", female = "bin",
                   rare_event = "bine", smoking = "cate",
                   education = "cat", civil_status = "cat"),
          missing = TRUE, percent_n = TRUE, headerperc = TRUE, varlabplus = TRUE,
          spacelowpercent = FALSE,
          highlight = 0.05,
          title = "Table 1. Alternative Formatting Options Demo",
          footnote = "Yellow rows indicate p < 0.05. Missing values shown as separate category.",
          xlsx = xlsx_table1, sheet = "Table 1 Formats")

# **# Sheet 7: Table 1 Missing -- Missing data summary per variable ----
# Demonstrates: missingsummary adds a missing-count row below each variable
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ crp contln %5.1f \ ///
#            prior_hosp conts \ female bin \ ///
#            smoking cate \ education cat \ ///
#            rare_event bine) ///
#       missingsummary ///
#       title("Table 1. Baseline with Missing Data Summary") ///
#       footnote("Missing row shows n (%) of missing values per variable.") ///
#       excel("`xlsx_table1'") sheet("Table 1 Missing")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", crp = "contln %5.1f",
                   prior_hosp = "conts", female = "bin",
                   smoking = "cate", education = "cat",
                   rare_event = "bine"),
          missingsummary = TRUE,
          title = "Table 1. Baseline with Missing Data Summary",
          footnote = "Missing row shows n (%) of missing values per variable.",
          xlsx = xlsx_table1, sheet = "Table 1 Missing")

# **# Sheet 8: Table 1 Custom -- Custom symbols and display options ----
# Demonstrates: slashN, catrowperc, iqrmiddle(), sdleft(), sdright(), pdp(), highpdp()
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ crp contln %5.1f \ ///
#            prior_hosp conts \ female bin \ ///
#            education cat \ smoking cate) ///
#       slashN catrowperc ///
#       iqrmiddle(" to ") sdleft(" [") sdright("]") ///
#       pdp(4) highpdp(3) ///
#       missing ///
#       title("Table 1. Custom Symbol Formatting Demo") ///
#       footnote("SD shown as mean [SD]. IQR uses 'to' separator. Row % for categoricals.") ///
#       excel("`xlsx_table1'") sheet("Table 1 Custom")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", crp = "contln %5.1f",
                   prior_hosp = "conts", female = "bin",
                   education = "cat", smoking = "cate"),
          slashN = TRUE, catrowperc = TRUE,
          iqrmiddle = " to ", sdleft = " [", sdright = "]",
          pdp = 4, highpdp = 3,
          missing = TRUE,
          title = "Table 1. Custom Symbol Formatting Demo",
          footnote = "SD shown as mean [SD]. IQR uses 'to' separator. Row % for categoricals.",
          xlsx = xlsx_table1, sheet = "Table 1 Custom")

# **# Sheet 9: Table 1 explicit compact styling ----
# Demonstrates: explicit font, size, academic borders, and zebra formatting
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ diabetes bin \ hypertension bin \ ///
#            anxiety bin \ prior_cvd bin) ///
#       smd test ///
#       font(Arial) fontsize(9) borderstyle(academic) zebra ///
#       title("Table 1. Baseline Characteristics (Compact Style)") ///
#       excel("`xlsx_table1'") sheet("Table 1 Compact")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", diabetes = "bin", hypertension = "bin",
                   anxiety = "bin", prior_cvd = "bin"),
          smd = TRUE, test = TRUE,
          font = "Arial", fontsize = 9, borderstyle = "academic", zebra = TRUE,
          title = "Table 1. Baseline Characteristics (Compact Style)",
          xlsx = xlsx_table1, sheet = "Table 1 Compact")

# **# Sheet 10: Logistic -- Single propensity score model ----
# Stata:
#   collect clear
#   collect: logistic treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   regtab, xlsx("`xlsx_regtab'") sheet("Logistic") frame(_demo_logistic) ///
#       title("Table 2. Propensity Score Model (Logistic Regression)") ///
#       coef("OR") noint models("Logistic")
# R has no frames: the returned table is the frame (as.data.frame() of it).
logistic_tab <- regtab(ps_model, xlsx = xlsx_regtab, sheet = "Logistic",
                       title = "Table 2. Propensity Score Model (Logistic Regression)",
                       coef = "OR", nointercept = TRUE, models = "Logistic")

# **# Sheet 11: Regtab Compact -- Estimate and CI in one column ----
# Stata: (the same logistic model)
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab Compact") ///
#       title("Table 2a. Compact Propensity Score Model") ///
#       coef("OR") noint compact models("Compact")
regtab(ps_model, xlsx = xlsx_regtab, sheet = "Regtab Compact",
       title = "Table 2a. Compact Propensity Score Model",
       coef = "OR", nointercept = TRUE, compact = TRUE, models = "Compact")

# **# Sheet 12: Regtab NoPvalue -- P-value columns suppressed ----
# Stata: (the same logistic model)
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab NoPvalue") ///
#       title("Table 2b. Propensity Score Model without P-values") ///
#       coef("OR") noint nopvalue models("No p-value")
regtab(ps_model, xlsx = xlsx_regtab, sheet = "Regtab NoPvalue",
       title = "Table 2b. Propensity Score Model without P-values",
       coef = "OR", nointercept = TRUE, nopvalue = TRUE, models = "No p-value")

# **# Sheet 13: Multi-Model -- Nested logistic models ----
# Stata:
#   collect clear
#   collect: logistic treated index_age female
#   collect: logistic treated index_age female i.education ///
#       diabetes hypertension
#   collect: logistic treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   regtab, xlsx("`xlsx_regtab'") sheet("Multi-Model") ///
#       title("Table 3. Propensity Score Models -- Nested Comparison") ///
#       coef("OR") models("Demographics \ + Comorbidities \ Full Model") ///
#       stats(n aic bic) noint
demographics <- glm(treated ~ index_age + female, family = binomial, data = analysis)
comorbidities <- glm(treated ~ index_age + female + education + diabetes + hypertension,
                     family = binomial, data = analysis)
regtab(demographics, comorbidities, ps_model, xlsx = xlsx_regtab, sheet = "Multi-Model",
       title = "Table 3. Propensity Score Models -- Nested Comparison",
       coef = "OR", models = c("Demographics", "+ Comorbidities", "Full Model"),
       stats = c("n", "aic", "bic"), nointercept = TRUE)

# **# Sheet 14: Cox Model -- Survival analysis ----
# Stata:
#   collect clear
#   collect: stcox treated index_age female i.education ///
#       diabetes hypertension anxiety
#   regtab, xlsx("`xlsx_regtab'") sheet("Cox Model") frame(_demo_cox) ///
#       title("Table 4. Cox Proportional Hazards Model") ///
#       coef("HR") stats(n ll) noint models("Cox PH")
# R: stset's follow_up and cv_event go into Surv(); ties = "breslow" is
# stcox's default tie handling (coxph's default is Efron).
cox_model <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age + female +
                               education + diabetes + hypertension + anxiety,
                             data = analysis, ties = "breslow")
cox_tab <- regtab(cox_model, xlsx = xlsx_regtab, sheet = "Cox Model",
                  title = "Table 4. Cox Proportional Hazards Model",
                  coef = "HR", stats = c("n", "ll"), nointercept = TRUE, models = "Cox PH")

# **# Sheet 15: Mixed Model -- Random effects with relabel + ICC ----
# Stata:
#   clear
#   set seed 20260323
#   set obs 600
#   gen int region = ceil(_n/100)
#   gen double age = rnormal(55, 12)
#   ... (female, bmi, a random intercept per region, y)
#   collect clear
#   collect: mixed y age female bmi || region:
#   regtab, xlsx("`xlsx_regtab'") sheet("Mixed Model") ///
#       title("Table 5. Mixed Effects Model -- BP Change by Region") ///
#       coef("Coef.") stats(n groups aic icc) relabel models("Mixed")
# R: the data Stata drew (demo_data$mixed_bp); REML = FALSE is mixed's
# default maximum likelihood.
mixed_bp <- demo_data$mixed_bp
mixed_model <- lme4::lmer(y ~ age + female + bmi + (1 | region), data = mixed_bp, REML = FALSE)
regtab(mixed_model, xlsx = xlsx_regtab, sheet = "Mixed Model",
       title = "Table 5. Mixed Effects Model -- BP Change by Region",
       coef = "Coef.", stats = c("n", "groups", "aic", "icc"), relabel = TRUE, models = "Mixed")

# **# Sheet 16: CDISC -- Regulatory-format regression output ----
# Demonstrates: regtab cdisc option (4-decimal precision, "Estimate" label)
# Stata:
#   collect clear
#   collect: logistic treated index_age female i.education diabetes hypertension
#   regtab, xlsx("`xlsx_regtab'") sheet("CDISC") ///
#       title("Table 5a. CDISC-Format Regression Output") ///
#       coef("OR") cdisc noint models("CDISC")
regtab(comorbidities, xlsx = xlsx_regtab, sheet = "CDISC",
       title = "Table 5a. CDISC-Format Regression Output",
       coef = "OR", cdisc = TRUE, nointercept = TRUE, models = "CDISC")

# **# Sheet 17: Poisson -- Incidence rate ratios from Poisson regression ----
# Stata:
#   collect clear
#   collect: poisson cv_event treated index_age female diabetes hypertension, ///
#       irr exposure(follow_up)
#   regtab, xlsx("`xlsx_regtab'") sheet("Poisson") ///
#       title("Table 5b. Poisson Regression -- Incidence Rate Ratios") ///
#       coef("IRR") noint stats(n aic) models("Poisson")
# R: exposure(follow_up) is an offset of log(follow_up).
poisson_model <- glm(cv_event ~ treated + index_age + female + diabetes + hypertension +
                       offset(log(follow_up)), family = poisson, data = analysis)
regtab(poisson_model, xlsx = xlsx_regtab, sheet = "Poisson",
       title = "Table 5b. Poisson Regression -- Incidence Rate Ratios",
       coef = "IRR", nointercept = TRUE, stats = c("n", "aic"), models = "Poisson")

# **# Sheet 18: GEE with QICu -- Population-averaged model with QICu statistic ----
# Demonstrates: fixed-scale binomial xtgee, stats(aic) auto-fallback to QICu,
# and a valid same-sample/same-correlation mean-model comparison
# Stata:
#   webuse union, clear
#   xtset idcode year
#   collect clear
#   collect: xtgee union age grade not_smsa south, ///
#       family(binomial) link(logit) corr(exchangeable)
#   collect: xtgee union age grade not_smsa south black, ///
#       family(binomial) link(logit) corr(exchangeable)
#   regtab, xlsx("`xlsx_regtab'") sheet("GEE QICu") ///
#       title("Table 5c. GEE Models -- QICu for Model Comparison") ///
#       noint stats(n aic groups) models("Base mean model" \ "Adjusted mean model")
# R: geepack::geeglm() with id = the panel variable; epsilon = 1e-6 is
# xtgee's convergence tolerance.
union <- demo_data$union
gee_base <- geepack::geeglm(union ~ age + grade + not_smsa + south, id = idcode,
                            family = binomial, corstr = "exchangeable", data = union,
                            control = geepack::geese.control(epsilon = 1e-6))
gee_adjusted <- geepack::geeglm(union ~ age + grade + not_smsa + south + black, id = idcode,
                                family = binomial, corstr = "exchangeable", data = union,
                                control = geepack::geese.control(epsilon = 1e-6))
regtab(gee_base, gee_adjusted, xlsx = xlsx_regtab, sheet = "GEE QICu",
       title = "Table 5c. GEE Models -- QICu for Model Comparison",
       nointercept = TRUE, stats = c("n", "aic", "groups"),
       models = c("Base mean model", "Adjusted mean model"))

# **# Sheet 19: Regtab Advanced -- Conditional formatting and label features ----
# Demonstrates: dimnonsig, factorlabel, starslevels(), and explicit formatting
# Stata:
#   collect clear
#   collect: logistic cv_event treated index_age female i.education ///
#       i.civil_status diabetes hypertension anxiety prior_cvd
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab Advanced") ///
#       title("Table 5d. Logistic Regression with Advanced Formatting") ///
#       coef("OR") noint dimnonsig factorlabel ///
#       starslevels(0.05 0.01 0.001) ///
#       font(Arial) fontsize(10) borderstyle(academic) models("Advanced")
cv_model_civil <- glm(cv_event ~ treated + index_age + female + education + civil_status +
                        diabetes + hypertension + anxiety + prior_cvd,
                      family = binomial, data = analysis)
regtab(cv_model_civil, xlsx = xlsx_regtab, sheet = "Regtab Advanced",
       title = "Table 5d. Logistic Regression with Advanced Formatting",
       coef = "OR", nointercept = TRUE, dimnonsig = TRUE, factorlabel = TRUE,
       starslevels = c(0.05, 0.01, 0.001),
       font = "Arial", fontsize = 10, borderstyle = "academic", models = "Advanced")

# **# Sheet 20: Regtab Select -- Covariate filtering with keep/drop ----
# Demonstrates: keep() to show only selected covariates, stars
# Stata:
#   collect clear
#   collect: logistic cv_event treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab Select") ///
#       title("Table 5e. Selected Covariates (keep/drop demo)") ///
#       coef("OR") noint stars ///
#       keep(treated index_age female diabetes) ///
#       footnote("Selected covariates from full model.") ///
#       models("Selected")
cv_model <- glm(cv_event ~ treated + index_age + female + education + diabetes +
                  hypertension + anxiety + prior_cvd, family = binomial, data = analysis)
regtab(cv_model, xlsx = xlsx_regtab, sheet = "Regtab Select",
       title = "Table 5e. Selected Covariates (keep/drop demo)",
       coef = "OR", nointercept = TRUE, stars = TRUE,
       keep = c("treated", "index_age", "female", "diabetes"),
       footnote = "Selected covariates from full model.",
       models = "Selected")

# **# Sheet 21: Regtab Drop -- Exclude specific covariates with drop() ----
# Demonstrates: drop() to hide covariates while keeping them in the model
# Stata: (the same model)
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab Drop") ///
#       title("Table 5f. Logistic Model (Confounders Suppressed)") ///
#       coef("OR") noint ///
#       drop(index_age female 2.education 3.education) ///
#       footnote("Model adjusted for age, sex, and education (coefficients suppressed).") ///
#       models("Adjusted")
# R: Stata's level keys (2.education) name factor levels in keep()/drop().
regtab(cv_model, xlsx = xlsx_regtab, sheet = "Regtab Drop",
       title = "Table 5f. Logistic Model (Confounders Suppressed)",
       coef = "OR", nointercept = TRUE,
       drop = c("index_age", "female", "2.education", "3.education"),
       footnote = "Model adjusted for age, sex, and education (coefficients suppressed).",
       models = "Adjusted")

# **# Sheet 22: Regtab AddRow -- Append custom summary rows ----
# Demonstrates: addrow() for P-trend, P-interaction, or other custom rows
# Stata:
#   collect clear
#   collect: logistic cv_event treated index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab'") sheet("Regtab AddRow") ///
#       title("Table 5g. Logistic Regression with Custom Summary Rows") ///
#       coef("OR") noint stars ///
#       addrow("P for trend" 0.032 \ "P for interaction" 0.15) ///
#       footnote("Custom rows appended below model estimates.") ///
#       models("Model 1")
cv_model_small <- glm(cv_event ~ treated + index_age + female + diabetes + hypertension,
                      family = binomial, data = analysis)
regtab(cv_model_small, xlsx = xlsx_regtab, sheet = "Regtab AddRow",
       title = "Table 5g. Logistic Regression with Custom Summary Rows",
       coef = "OR", nointercept = TRUE, stars = TRUE,
       addrow = list("P for trend" = 0.032, "P for interaction" = 0.15),
       footnote = "Custom rows appended below model estimates.",
       models = "Model 1")

# **# Sheet 23: MLogit -- Multinomial logit with outcome-specific rows ----
# Stata:
#   collect clear
#   collect: mlogit education index_age female diabetes hypertension, ///
#       baseoutcome(1)
#   regtab, xlsx("`xlsx_regtab_models'") sheet("MLogit") ///
#       title("Table 5h. Multinomial Logit -- Education Level") ///
#       stats(n ll aic bic r2) models("Multinomial")
regtab(mlogit_model, xlsx = xlsx_regtab_models, sheet = "MLogit",
       title = "Table 5h. Multinomial Logit -- Education Level",
       stats = c("n", "ll", "aic", "bic", "r2"), models = "Multinomial")

# **# Sheet 24: OLS -- Linear regression ----
# Stata:
#   collect clear
#   collect: regress crp treated index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab_models'") sheet("OLS") ///
#       title("Table 5i. Linear Regression -- C-Reactive Protein") ///
#       coef("Coef.") stats(n r2 aic bic) models("OLS")
ols_model <- lm(crp ~ treated + index_age + female + diabetes + hypertension, data = analysis)
regtab(ols_model, xlsx = xlsx_regtab_models, sheet = "OLS",
       title = "Table 5i. Linear Regression -- C-Reactive Protein",
       coef = "Coef.", stats = c("n", "r2", "aic", "bic"), models = "OLS")

# **# Sheet 25: Probit -- Binary probit regression ----
# Stata:
#   collect clear
#   collect: probit cv_event treated index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab_models'") sheet("Probit") ///
#       title("Table 5j. Probit Regression -- Cardiovascular Event") ///
#       coef("Coef.") noint stats(n ll aic bic r2) models("Probit")
probit_model <- glm(cv_event ~ treated + index_age + female + diabetes + hypertension,
                    family = binomial(link = "probit"), data = analysis)
regtab(probit_model, xlsx = xlsx_regtab_models, sheet = "Probit",
       title = "Table 5j. Probit Regression -- Cardiovascular Event",
       coef = "Coef.", nointercept = TRUE, stats = c("n", "ll", "aic", "bic", "r2"),
       models = "Probit")

# **# Sheet 26: Ordered Logit -- Ordinal outcome model ----
# Stata:
#   collect clear
#   collect: ologit education index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab_models'") sheet("Ordered Logit") ///
#       title("Table 5k. Ordered Logit -- Education Level") ///
#       keepintercept cutlabels("Primary to Secondary \ Secondary to Tertiary") ///
#       stats(n ll aic bic r2) models("Ordered logit")
# R: MASS::polr(); Hess = TRUE keeps the Hessian for the standard errors.
ologit_model <- MASS::polr(education ~ index_age + female + diabetes + hypertension,
                           data = analysis, Hess = TRUE)
regtab(ologit_model, xlsx = xlsx_regtab_models, sheet = "Ordered Logit",
       title = "Table 5k. Ordered Logit -- Education Level",
       keepintercept = TRUE, cutlabels = c("Primary to Secondary", "Secondary to Tertiary"),
       stats = c("n", "ll", "aic", "bic", "r2"), models = "Ordered logit")

# **# Sheet 27: Negative Binomial -- Count model ----
# Stata:
#   collect clear
#   collect: nbreg prior_hosp treated index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab_models'") sheet("NegBin") ///
#       title("Table 5l. Negative Binomial Regression -- Prior Hospitalizations") ///
#       noint stats(n ll aic bic) models("Negative binomial")
# R: MASS::glm.nb(). prior_hosp is Poisson by construction, so the
# overdispersion estimate runs to its boundary (theta -> infinity) and
# glm.nb() warns "iteration limit reached"; nbreg reports the same
# boundary as alpha near 0. The table is unaffected.
nbreg_model <- suppressWarnings(
  MASS::glm.nb(prior_hosp ~ treated + index_age + female + diabetes + hypertension, data = analysis))
regtab(nbreg_model, xlsx = xlsx_regtab_models, sheet = "NegBin",
       title = "Table 5l. Negative Binomial Regression -- Prior Hospitalizations",
       nointercept = TRUE, stats = c("n", "ll", "aic", "bic"), models = "Negative binomial")

# **# Sheet 28: GLM Poisson -- Generalized linear model ----
# Stata:
#   collect clear
#   collect: glm prior_hosp treated index_age female diabetes hypertension, ///
#       family(poisson) link(log) nolog
#   regtab, xlsx("`xlsx_regtab_models'") sheet("GLM Poisson") ///
#       title("Table 5m. GLM Poisson -- Prior Hospitalizations") ///
#       stats(n aic bic) models("GLM Poisson")
glm_poisson <- glm(prior_hosp ~ treated + index_age + female + diabetes + hypertension,
                   family = poisson, data = analysis)
regtab(glm_poisson, xlsx = xlsx_regtab_models, sheet = "GLM Poisson",
       title = "Table 5m. GLM Poisson -- Prior Hospitalizations",
       stats = c("n", "aic", "bic"), models = "GLM Poisson")

# **# Sheet 29: Panel RE -- Random-effects panel model ----
# Stata (xtreg's GLS random-effects estimator has no regtab() method in R):
#   collect: xtreg symptom_score exposure age, re
#   regtab, xlsx("`xlsx_regtab_models'") sheet("Panel RE") ///
#       title("Table 5n. Panel Random-Effects Regression") ///
#       coef("Coef.") noreeffects stats(n groups) models("Panel RE")

# **# Sheet 30: Quantile -- Median regression ----
# Stata (qreg: quantile regression has no regtab() method in R):
#   collect: qreg crp treated index_age female diabetes hypertension
#   regtab, xlsx("`xlsx_regtab_models'") sheet("Quantile") ///
#       title("Table 5o. Median Regression -- C-Reactive Protein") ///
#       coef("Coef.") stats(n r2) models("Median")

# **# Sheet 31: ZIP ZINB -- Zero-inflated count models ----
# Stata: (the zero-inflated data and models of the console section)
#   regtab, xlsx("`xlsx_regtab_models'") sheet("ZIP ZINB") ///
#       title("Table 5p. Zero-Inflated Count Models") ///
#       stats(n ll aic bic) models("ZIP" \ "ZINB")
regtab(zip_model, zinb_model, xlsx = xlsx_regtab_models, sheet = "ZIP ZINB",
       title = "Table 5p. Zero-Inflated Count Models",
       stats = c("n", "ll", "aic", "bic"), models = c("ZIP", "ZINB"))

# **# Sheet 32: Hurdle -- Cragg hurdle model ----
# Stata (churdle has no R fitter; regtab() takes the estimates as a data
# frame instead, see ?regtab):
#   collect: churdle linear annual_cost dose_intensity, ///
#       select(participation_score) ll(0)
#   regtab, xlsx("`xlsx_regtab_models'") sheet("Hurdle") ///
#       title("Table 5q. Cragg Hurdle Model -- Annual Cost") ///
#       stats(n ll aic bic r2) models("Cragg hurdle")

# **# Verify new regtab workbook content ----
# Stata: import excel ..., allstring, then assert on the cell text.
compact <- sheet_text(xlsx_regtab, "Regtab Compact")
stopifnot(any(grepl("(", compact, fixed = TRUE) & grepl(")", compact, fixed = TRUE)),
          any(trimws(compact) == "p-value"))
stopifnot(!any(trimws(sheet_text(xlsx_regtab, "Regtab NoPvalue")) == "p-value"))
stopifnot(any(grepl("Secondary|Tertiary", sheet_text(xlsx_regtab_models, "MLogit"))))
stopifnot(any(grepl("Primary to Secondary|Secondary to Tertiary",
                    sheet_text(xlsx_regtab_models, "Ordered Logit"))))
stopifnot(any(grepl("Inflation equation", sheet_text(xlsx_regtab_models, "ZIP ZINB"), fixed = TRUE)))

# Build purpose-built Cox model frames for composite demo
# Both use HR -- same coefficient type so headers align correctly
# Stata:
#   collect clear
#   collect: stcox treated index_age female diabetes hypertension anxiety, nolog
#   regtab, xlsx("`xlsx_comptab'") sheet("S Binary") frame(_demo_binary) coef("HR") noint ///
#       title("Cox Model -- Binary Treatment") models("Cox PH")
# R has no frames: comptab() takes the tables regtab() returns.
cox_binary <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age + female +
                                diabetes + hypertension + anxiety,
                              data = analysis, ties = "breslow")
demo_binary <- regtab(cox_binary, xlsx = xlsx_comptab, sheet = "S Binary", coef = "HR",
                      nointercept = TRUE, title = "Cox Model -- Binary Treatment", models = "Cox PH")

# Stata:
#   collect clear
#   collect: stcox i.education index_age female treated diabetes hypertension, nolog
#   regtab, xlsx("`xlsx_comptab'") sheet("S Education") frame(_demo_educ) coef("HR") noint ///
#       title("Cox Model -- Education Categories") models("Cox PH")
cox_education <- survival::coxph(survival::Surv(follow_up, cv_event) ~ education + index_age + female +
                                   treated + diabetes + hypertension,
                                 data = analysis, ties = "breslow")
demo_educ <- regtab(cox_education, xlsx = xlsx_comptab, sheet = "S Education", coef = "HR",
                    nointercept = TRUE, title = "Cox Model -- Education Categories", models = "Cox PH")

# **# Sheet 22: Composite -- Cherry-pick exposure rows from two Cox models ----
# Demonstrates: comptab pulling specific rows into one summary table
# Treatment HR from binary model + education HRs from factor model
# Stata:
#   comptab _demo_binary _demo_educ, ///
#       rows(1 \ 1/4) ///
#       xlsx("`xlsx_comptab'") sheet("Composite") ///
#       title("Table S1. Exposure Effects on Cardiovascular Events") ///
#       separator(2)
# R: one rows() entry per source table, in a list.
comptab(list(demo_binary, demo_educ),
        rows = list(1, 1:4),
        xlsx = xlsx_comptab, sheet = "Composite",
        title = "Table S1. Exposure Effects on Cardiovascular Events",
        separator = 2)

# **# Sheet 23: Composite Compact -- Full composite with sections + footnote ----
# Demonstrates: compact, section(), relabel(), footnote(), and explicit formatting
# Treatment + confounders from model 1, education from model 2
# Stata:
#   comptab _demo_binary _demo_educ, ///
#       rows(1 4 5 6 \ 1/4) compact ///
#       section("Treatment Effect" \ "Education Level") ///
#       xlsx("`xlsx_comptab'") sheet("Composite Compact") ///
#       title("Table 3. Risk Factors for Cardiovascular Events") ///
#       footnote("aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age, sex, and comorbidities.") ///
#       font(Arial) fontsize(9) borderstyle(academic)
comptab(list(demo_binary, demo_educ),
        rows = list(c(1, 4, 5, 6), 1:4), compact = TRUE,
        section = c("Treatment Effect", "Education Level"),
        xlsx = xlsx_comptab, sheet = "Composite Compact",
        title = "Table 3. Risk Factors for Cardiovascular Events",
        footnote = "aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age, sex, and comorbidities.",
        font = "Arial", fontsize = 9, borderstyle = "academic")

# **# Sheet 24: Composite Names -- Pattern-based row selection with rownames() ----
# Demonstrates: comptab rownames() as alternative to rows() for label-based selection
# Stata:
#   comptab _demo_binary _demo_educ, ///
#       rownames(Treatment Diabetes Hypertension \ Secondary Tertiary) ///
#       xlsx("`xlsx_comptab'") sheet("Composite Names") ///
#       title("Table S2. Selected Risk Factors (Name-Based Selection)") ///
#       compact zebra ///
#       footnote("Rows selected by label pattern using rownames() option.")
comptab(list(demo_binary, demo_educ),
        rownames = list(c("Treatment", "Diabetes", "Hypertension"), c("Secondary", "Tertiary")),
        xlsx = xlsx_comptab, sheet = "Composite Names",
        title = "Table S2. Selected Risk Factors (Name-Based Selection)",
        compact = TRUE, zebra = TRUE,
        footnote = "Rows selected by label pattern using rownames() option.")

# **# Sheet 25: ATE -- Treatment effects (IPW) ----
# Stata:
#   collect clear
#   collect: teffects ipw (cv_event) (treated index_age female i.education ///
#       diabetes hypertension anxiety), ate
#   effecttab, xlsx("`xlsx_effecttab'") sheet("ATE") ///
#       effect("ATE") ///
#       title("Table 6. Average Treatment Effect on CV Events (IPW)") ///
#       tlabels(0 "SSRI" 1 "SNRI")
# R: teffects ipw is a weighted outcome model with logistic propensity
# weights; WeightIt::glm_weightit() gives the same estimates with the
# M-estimation variance teffects uses, and marginaleffects the ATE and the
# control group's potential-outcome mean.
te_data <- analysis
te_data$treated <- as.numeric(te_data$treated)
ipw_weights <- WeightIt::weightit(treated ~ index_age + female + education + diabetes +
                                    hypertension + anxiety,
                                  data = te_data, method = "glm", estimand = "ATE")
ipw_model <- WeightIt::glm_weightit(cv_event ~ treated, data = te_data, weightit = ipw_weights)
ipw <- list(marginaleffects::avg_comparisons(ipw_model, variables = "treated"),
            marginaleffects::avg_predictions(ipw_model, variables = list(treated = 0)))
effecttab(list(ipw), data = analysis, xlsx = xlsx_effecttab, sheet = "ATE",
          effect = "ATE", method = "ipw",
          title = "Table 6. Average Treatment Effect on CV Events (IPW)",
          tlabels = c("0" = "SSRI", "1" = "SNRI"))

# **# Sheet 26: ATE Comparison -- Multiple estimators side by side ----
# Demonstrates: effecttab with multiple collected teffects models + models()
# Stata:
#   collect clear
#   collect: teffects ipw (cv_event) (treated index_age female i.education ///
#       diabetes hypertension anxiety), ate
#   collect: teffects aipw (cv_event index_age female i.education ///
#       diabetes hypertension anxiety) (treated index_age female i.education ///
#       diabetes hypertension anxiety), ate
#   effecttab, xlsx("`xlsx_effecttab'") sheet("ATE Comparison") ///
#       effect("ATE") models("IPW \ AIPW") ///
#       title("Table 7. Treatment Effect Estimates -- IPW vs AIPW") ///
#       tlabels(0 "SSRI" 1 "SNRI") zebra ///
#       footnote("IPW = inverse probability weighting. AIPW = augmented IPW (doubly robust).")
# R: augmented IPW in base R. teffects aipw's defaults: a linear outcome
# model per treatment arm, the logistic propensity model above, and the
# joint estimating-equation sandwich for the standard errors. effecttab() takes
# the two estimates as a data frame with Stata's equation and term keys.
aipw_estimate <- function(data) {
  tm <- glm(treated ~ index_age + female + education + diabetes + hypertension + anxiety,
            family = binomial, data = data, na.action = na.fail)
  ps <- fitted(tm)
  outcome <- cv_event ~ index_age + female + education + diabetes + hypertension + anxiety
  om1 <- lm(outcome, data = data[data$treated == 1, ], na.action = na.fail)
  om0 <- lm(outcome, data = data[data$treated == 0, ], na.action = na.fail)
  mu1 <- predict(om1, newdata = data)
  mu0 <- predict(om0, newdata = data)
  a <- as.numeric(data$treated)
  y <- as.numeric(data$cv_event)
  X <- model.matrix(tm)
  n <- nrow(X)
  if (!tm$converged || length(y) != n || !all(a %in% c(0, 1)) ||
      !all(is.finite(c(y, ps, mu0, mu1, X))) || any(ps <= 0 | ps >= 1) ||
      !identical(colnames(X), names(coef(om0))) ||
      !identical(colnames(X), names(coef(om1))) ||
      anyNA(c(coef(tm), coef(om0), coef(om1)))) {
    stop("The AIPW demo requires complete data, both treatment arms, full-rank common designs and interior converged propensity scores.")
  }
  if1 <- mu1 + a * (y - mu1) / ps
  if0 <- mu0 + (1 - a) * (y - mu0) / (1 - ps)
  # Stata teffects aipw manual pp.16-18,23: standard AIPW stacks
  # POM0, POM1, arm-specific linear OM scores and logit TM scores.
  # The linear score has no sigma parameter; its constant scale cancels.
  k <- ncol(X)
  b0 <- seq_len(k) + 2L
  b1 <- b0 + k
  gamma <- b1 + k
  parameter_names <- c("POM0", "POM1", paste0("OM0:", colnames(X)),
                       paste0("OM1:", colnames(X)), paste0("TM:", colnames(X)))
  U <- cbind(if0 - mean(if0), if1 - mean(if1),
             X * ((1 - a) * (y - mu0)), X * (a * (y - mu1)), X * (a - ps))
  colnames(U) <- parameter_names
  # A is mean dU/dtheta, not its inverse. The POM/nuisance blocks
  # are nonzero in finite samples and must not be dropped as in a plug-in IF.
  A <- matrix(0, ncol(U), ncol(U), dimnames = list(parameter_names, parameter_names))
  A[1, 1] <- A[2, 2] <- -1
  A[1, b0] <- colMeans(X * (1 - (1 - a) / (1 - ps)))
  A[2, b1] <- colMeans(X * (1 - a / ps))
  A[1, gamma] <- colMeans(X * ((1 - a) * (y - mu0) * ps / (1 - ps)))
  A[2, gamma] <- colMeans(X * (-a * (y - mu1) * (1 - ps) / ps))
  A[b0, b0] <- -crossprod(X, X * (1 - a)) / n
  A[b1, b1] <- -crossprod(X, X * a) / n
  A[gamma, gamma] <- -crossprod(X, X * (ps * (1 - ps))) / n
  inverse <- solve(A)
  V <- inverse %*% crossprod(U) %*% t(inverse) / n^2
  # Transform POM0/POM1 to the unchanged output order ATE/POM0.
  contrast <- matrix(c(-1, 1, 1, 0), 2L, byrow = TRUE)
  effects_vcov <- contrast %*% V[1:2, 1:2, drop = FALSE] %*% t(contrast)
  dimnames(effects_vcov) <- list(c("r1vs0.treated", "0.treated"), c("r1vs0.treated", "0.treated"))
  out <- data.frame(equation = c("ATE", "POmean"), term = c("r1vs0.treated", "0.treated"),
                    estimate = c(mean(if1 - if0), mean(if0)),
                    std.error = unname(sqrt(diag(effects_vcov))))
  attr(out, "tt_estimator") <- "aipw"
  attr(out, "vcov") <- effects_vcov
  attr(out, "aipw_ee") <- list(theta = setNames(c(mean(if0), mean(if1), coef(om0), coef(om1), coef(tm)), parameter_names),
                               jacobian = A, vcov = V, N = n)
  out
}
aipw <- aipw_estimate(te_data)
effecttab(list(IPW = ipw, AIPW = aipw), data = analysis, xlsx = xlsx_effecttab,
          sheet = "ATE Comparison",
          effect = "ATE", models = "IPW \\ AIPW",
          title = "Table 7. Treatment Effect Estimates -- IPW vs AIPW",
          tlabels = c("0" = "SSRI", "1" = "SNRI"), zebra = TRUE,
          footnote = "IPW = inverse probability weighting. AIPW = augmented IPW (doubly robust).")

# **# Sheet 27: Margins -- Predicted probabilities ----
# Stata:
#   quietly logit cv_event treated##c.index_age female i.education ///
#       diabetes hypertension
#   collect clear
#   collect: margins treated, post
#   effecttab, xlsx("`xlsx_effecttab'") sheet("Margins") ///
#       type(margins) effect("Pr(CV Event)") ///
#       title("Table 8. Predicted Probability of CV Event by Treatment")
# R: i.treated is a factor; avg_predictions() averages over the data as
# margins does.
margins_data <- tt_as_factor(analysis, vars = "treated")
margins_model <- glm(cv_event ~ treated * index_age + female + education + diabetes + hypertension,
                     family = binomial, data = margins_data)
effecttab(marginaleffects::avg_predictions(margins_model, variables = "treated"),
          xlsx = xlsx_effecttab, sheet = "Margins",
          type = "margins", effect = "Pr(CV Event)",
          title = "Table 8. Predicted Probability of CV Event by Treatment")

# **# Sheet 28: Margins AME -- Average marginal effects ----
# Demonstrates: effecttab with margins dydx() for average marginal effects
# Stata:
#   quietly logit cv_event treated index_age female i.education ///
#       diabetes hypertension anxiety prior_cvd
#   collect clear
#   collect: margins, dydx(treated index_age female diabetes hypertension) post
#   effecttab, xlsx("`xlsx_effecttab'") sheet("Margins AME") ///
#       type(margins) effect("AME") ///
#       title("Table 9. Average Marginal Effects on CV Event Risk") ///
#       footnote("AME = average marginal effect. Change in Pr(CV event) per unit change in covariate.")
# R: marginaleffects reports the discrete change 1 - 0 of a 0/1 variable
# (Stata's dydx(i.treated)), where dydx(treated) takes the derivative, so
# the 0/1 rows can differ in the last digit (?effecttab).
effecttab(marginaleffects::avg_slopes(cv_model, variables = c("treated", "index_age", "female",
                                                              "diabetes", "hypertension")),
          xlsx = xlsx_effecttab, sheet = "Margins AME",
          type = "margins", effect = "AME",
          title = "Table 9. Average Marginal Effects on CV Event Risk",
          footnote = "AME = average marginal effect. Change in Pr(CV event) per unit change in covariate.")

# **# Sheet 29: Rates -- Incidence rates with rate ratios + multiple exposure strata ----
# Demonstrates: stratetab rateratio, ratiodigits(), explabels(), multiple strata
# Stata writes four strate-shaped files (2 outcomes x 2 strata):
#   clear
#   input str20 drug_class _D _Y double(_Rate _Lower _Upper)
#   "SSRI"   178 28100 0.00633 0.00545 0.00734
#   "SNRI"   161 22300 0.00722 0.00616 0.00844
#   end
#   label variable _Lower "Lower 95% confidence limit"
#   label variable _Upper "Upper 95% confidence limit"
#   save "`pkg_dir'/_strate_cv_m.dta", replace
#   ... (_strate_sh_m, _strate_cv_f, _strate_sh_f)
# R: the same four tables as data frames, in the same order.
strate_block <- function(D, Y, rate, lower, upper) {
  d <- data.frame(drug_class = c("SSRI", "SNRI"), `_D` = D, `_Y` = Y, `_Rate` = rate,
                  `_Lower` = lower, `_Upper` = upper, check.names = FALSE)
  attr(d[["_Lower"]], "label") <- "Lower 95% confidence limit"
  attr(d[["_Upper"]], "label") <- "Upper 95% confidence limit"
  d
}
strate_cv_m <- strate_block(c(178, 161), c(28100, 22300), c(0.00633, 0.00722),
                            c(0.00545, 0.00616), c(0.00734, 0.00844))
strate_sh_m <- strate_block(c(52, 58), c(28100, 22300), c(0.00185, 0.00260),
                            c(0.00139, 0.00199), c(0.00243, 0.00337))
strate_cv_f <- strate_block(c(134, 128), c(24380, 19520), c(0.00550, 0.00656),
                            c(0.00462, 0.00549), c(0.00652, 0.00781))
strate_sh_f <- strate_block(c(35, 36), c(24380, 19520), c(0.00144, 0.00184),
                            c(0.00100, 0.00129), c(0.00200, 0.00256))

# Stata:
#   stratetab, using("`pkg_dir'/_strate_cv_m" "`pkg_dir'/_strate_sh_m" "`pkg_dir'/_strate_cv_f" "`pkg_dir'/_strate_sh_f") ///
#       xlsx("`xlsx_stratetab'") outcomes(2) sheet("Rates") ///
#       outlabels("CV Events \ Self-Harm") ///
#       explabels("Male \ Female") ///
#       rateratio ratiodigits(2) zebra ///
#       title("Table 12. Incidence Rates per 1,000 Person-Years by Sex") ///
#       footnote("IRR = incidence rate ratio, Female vs Male. CI by log-normal method.")
stratetab(list(strate_cv_m, strate_sh_m, strate_cv_f, strate_sh_f),
          xlsx = xlsx_stratetab, outcomes = 2, sheet = "Rates",
          outlabels = c("CV Events", "Self-Harm"),
          explabels = c("Male", "Female"),
          rateratio = TRUE, ratiodigits = 2, zebra = TRUE,
          title = "Table 12. Incidence Rates per 1,000 Person-Years by Sex",
          footnote = "IRR = incidence rate ratio, Female vs Male. CI by log-normal method.")

stratetab(list(strate_cv_m, strate_sh_m, strate_cv_f, strate_sh_f), outcomes = 2,
          outlabels = c("CV Events", "Self-Harm"), explabels = c("Male", "Female"),
          cformat = "%5.2f", sep = " to ", xlsx = xlsx_stratetab, sheet = "Rates Formatted",
          title = "Table 12b. Incidence Rates, Two Decimals and 'to' Intervals")

# **# Sheet 30: Correlation -- Pearson with stars (lower triangle) ----
# Stata:
#   corrtab index_age crp prior_hosp, ///
#       xlsx("`xlsx_corrtab'") sheet("Correlation") ///
#       title("Table 13. Pearson Correlation Matrix") ///
#       star(0.05 0.01 0.001)

corrtab(demo_data$cohort, c("index_age", "crp", "prior_hosp"), star = c(.05, .01, .001),
        xlsx = xlsx_corrtab, sheet = "Correlation", title = "Table 13. Pearson Correlation Matrix")

# **# Sheet 31: Correlation Spearman -- Spearman with p-values ----
# Stata:
#   corrtab index_age crp prior_hosp, ///
#       xlsx("`xlsx_corrtab'") sheet("Correlation Spear") ///
#       title("Table 14. Spearman Rank Correlation Matrix") ///
#       spearman pvalues

corrtab(demo_data$cohort, c("index_age", "crp", "prior_hosp"), spearman = TRUE, pvalues = TRUE,
        xlsx = xlsx_corrtab, sheet = "Correlation Spear", title = "Table 14. Spearman Rank Correlation Matrix")

# **# Sheet 32: Correlation Full -- Pearson full matrix (all cells) ----
# Stata:
#   corrtab index_age crp prior_hosp, ///
#       xlsx("`xlsx_corrtab'") sheet("Correlation Full") ///
#       title("Table 15. Pearson Correlation Matrix (Full)") ///
#       full star(0.05 0.01 0.001)

corrtab(demo_data$cohort, c("index_age", "crp", "prior_hosp"), full = TRUE, star = c(.05, .01, .001),
        xlsx = xlsx_corrtab, sheet = "Correlation Full", title = "Table 15. Pearson Correlation Matrix (Full)")

# **# Sheet 33: Cross-Tabulation -- 2x2 with Fisher's exact + OR ----
# Stata:
#   crosstab treated female, ///
#       xlsx("`xlsx_crosstab'") sheet("Cross-Tabulation") ///
#       title("Table 16. Treatment by Sex") ///
#       exact or label

crosstab(demo_data$cohort, "treated", "female", exact = TRUE, or = TRUE, label = TRUE,
         xlsx = xlsx_crosstab, sheet = "Cross-Tabulation", title = "Table 16. Treatment by Sex")

# **# Sheet 34: Cross-Tab Measures -- Risk ratio and risk difference ----
# Stata:
#   crosstab treated cv_event, ///
#       xlsx("`xlsx_crosstab'") sheet("Cross-Tab Measures") ///
#       title("Table 16a. Treatment-Outcome Association Measures") ///
#       rr rd label ///
#       footnote("RR = risk ratio; RD = risk difference with 95% CI.")

crosstab(demo_data$cohort, "treated", "cv_event", rr = TRUE, rd = TRUE, label = TRUE,
         xlsx = xlsx_crosstab, sheet = "Cross-Tab Measures",
         title = "Table 16a. Treatment-Outcome Association Measures", footnote = "RR = risk ratio; RD = risk difference with 95% CI.")

# **# Sheet 35: Cross-Tab Styled -- boldp() + zebra ----
# Stata:
#   crosstab outcome exposure, ///
#       xlsx("`xlsx_crosstab'") sheet("Cross-Tab Styled") ///
#       title("Table 16b. Outcome by Ordinal Exposure") ///
#       trend label boldp(0.05) zebra ///
#       footnote("Significant chi-squared and trend rows are bolded when p < 0.05.")

# Exact literal native input, crosstab demo source1423-1434; no RNG redraw.
cross_styled <- data.frame(outcome = rep(c(0, 1), 3), exposure = rep(0:2, each = 2), frequency = c(25, 5, 15, 15, 5, 25))
cross_styled$outcome <- haven::labelled(cross_styled$outcome, c("No event" = 0, "Event" = 1))
cross_styled$exposure <- haven::labelled(cross_styled$exposure, c("Low" = 0, "Medium" = 1, "High" = 2))
crosstab(cross_styled, "outcome", "exposure", weights = "frequency", trend = TRUE, label = TRUE, boldp = .05, zebra = TRUE,
         xlsx = xlsx_crosstab, sheet = "Cross-Tab Styled", title = "Table 16b. Outcome by Ordinal Exposure",
         footnote = "Significant chi-squared and trend rows are bolded when p < 0.05.")

# **# Sheet 36: Cross-Tab Trend -- Cochran-Armitage trend test ----
# Stata:
#   crosstab education cv_event, ///
#       xlsx("`xlsx_crosstab'") sheet("Cross-Tab Trend") ///
#       title("Table 16c. CV Events by Education Level (Trend Test)") ///
#       trend label zebra

crosstab(demo_data$cohort, "education", "cv_event", trend = TRUE, label = TRUE, zebra = TRUE,
         xlsx = xlsx_crosstab, sheet = "Cross-Tab Trend", title = "Table 16c. CV Events by Education Level (Trend Test)")

# **# Sheet 37: Cross-Tab Row Pct -- Row percentages instead of column ----
# Stata:
#   crosstab treated cv_event, ///
#       xlsx("`xlsx_crosstab'") sheet("Cross-Tab Row Pct") ///
#       title("Table 16d. Treatment-Outcome (Row Percentages)") ///
#       rowpct or label ///
#       footnote("Percentages are row percentages within each treatment group.")

crosstab(demo_data$cohort, "treated", "cv_event", rowpct = TRUE, or = TRUE, label = TRUE,
         xlsx = xlsx_crosstab, sheet = "Cross-Tab Row Pct", title = "Table 16d. Treatment-Outcome (Row Percentages)",
         footnote = "Percentages are row percentages within each treatment group.")

# **# Sheet 41: Survival -- Kaplan-Meier table with median ----
# Stata:
#   stset follow_up, failure(cv_event)
#   survtab, times(365 730 1095 1460) by(treated) ///
#       xlsx("`xlsx_survtab'") sheet("Survival") ///
#       title("Table 18. Kaplan-Meier Survival Estimates") ///
#       median timeunit(days) ///
#       footnote("Survival probabilities estimated by Kaplan-Meier method.")

survtab(demo_data$cohort, "follow_up", "cv_event", times = c(365, 730, 1095, 1460), by = "treated", median = TRUE, timeunit = "days",
        xlsx = xlsx_survtab, sheet = "Survival", title = "Table 18. Kaplan-Meier Survival Estimates",
        footnote = "Survival probabilities estimated by Kaplan-Meier method.")

# **# Sheet 42: Survival RMST -- RMST + risk set + between-group difference ----
# Stata:
#   survtab, times(365 730 1095 1460) by(treated) ///
#       rmst(1460) riskset difference ///
#       xlsx("`xlsx_survtab'") sheet("Survival RMST") ///
#       title("Table 18a. Survival with RMST and Group Differences") ///
#       median timeunit(days) ///
#       footnote("RMST = restricted mean survival time truncated at 1460 days.")

survtab(demo_data$cohort, "follow_up", "cv_event", times = c(365, 730, 1095, 1460), by = "treated", rmst = 1460,
        riskset = TRUE, difference = TRUE, median = TRUE, timeunit = "days", xlsx = xlsx_survtab, sheet = "Survival RMST",
        title = "Table 18a. Survival with RMST and Group Differences", footnote = "RMST = restricted mean survival time truncated at 1460 days.")

# **# Sheet 43: Cumulative Incidence -- Reverse survival function ----
# Stata:
#   survtab, times(365 730 1095 1460) by(treated) ///
#       reverse ///
#       xlsx("`xlsx_survtab'") sheet("Cumul Incidence") ///
#       title("Table 18b. Cumulative Incidence of CV Events") ///
#       timeunit(days) font("Times New Roman") fontsize(12) borderstyle(academic)

survtab(demo_data$cohort, "follow_up", "cv_event", times = c(365, 730, 1095, 1460), by = "treated", reverse = TRUE,
        timeunit = "days", font = "Times New Roman", fontsize = 12, borderstyle = "academic", xlsx = xlsx_survtab,
        sheet = "Cumul Incidence", title = "Table 18b. Cumulative Incidence of CV Events")

# **# Sheet 44: Explicit Arial formatting ----
# Demonstrates: Arial 10-point text with academic borders
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ diabetes bin \ hypertension bin \ ///
#            anxiety bin \ prior_cvd bin) ///
#       smd ///
#       font(Arial) fontsize(10) borderstyle(academic) ///
#       title("Table 1. Baseline Characteristics (Arial Style)") ///
#       excel("`xlsx_table1'") sheet("Explicit Arial")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", diabetes = "bin", hypertension = "bin",
                   anxiety = "bin", prior_cvd = "bin"),
          smd = TRUE,
          font = "Arial", fontsize = 10, borderstyle = "academic",
          title = "Table 1. Baseline Characteristics (Arial Style)",
          xlsx = xlsx_table1, sheet = "Explicit Arial")

# **# Sheet 45: Explicit serif formatting ----
# Demonstrates: Times New Roman 12-point text with academic borders
# Stata:
#   table1_tc, by(treated) ///
#       vars(index_age contn %5.1f \ female bin \ ///
#            education cat \ diabetes bin \ hypertension bin \ ///
#            anxiety bin \ prior_cvd bin) ///
#       smd ///
#       font("Times New Roman") fontsize(12) borderstyle(academic) ///
#       title("Table 1. Baseline Characteristics (Serif Style)") ///
#       excel("`xlsx_table1'") sheet("Explicit Serif")
table1_tc(analysis, by = "treated",
          vars = c(index_age = "contn %5.1f", female = "bin",
                   education = "cat", diabetes = "bin", hypertension = "bin",
                   anxiety = "bin", prior_cvd = "bin"),
          smd = TRUE,
          font = "Times New Roman", fontsize = 12, borderstyle = "academic",
          title = "Table 1. Baseline Characteristics (Serif Style)",
          xlsx = xlsx_table1, sheet = "Explicit Serif")

# **# Sheet 46: HR Composite -- hrcomptab final Table 2-style survival composite ----
# Stata writes six strate-shaped rate files, 3 outcomes x 2 exposures:
#   clear
#   input byte exposure double(_D _Y _Rate _Lower _Upper)
#   0 42 5200 0.00808 0.00610 0.01069
#   1 31 4980 0.00622 0.00436 0.00887
#   end
#   label variable _Lower "Lower 95% confidence limit"
#   label variable _Upper "Upper 95% confidence limit"
#   label define _hrc_bin 0 "No HRT" 1 "Any HRT", replace
#   label values exposure _hrc_bin
#   save "`rate11'.dta", replace
#   ... (rate12, rate13: the binary rows of the other two outcomes;
#        rate21-rate23: No HRT, Low, Medium and High dose)
# R: the same six tables as data frames, value labels with haven::labelled().
rate_block <- function(labels, D, Y, rate, lower, upper) {
  d <- data.frame(exposure = haven::labelled(unname(labels), labels), `_D` = D, `_Y` = Y,
                  `_Rate` = rate, `_Lower` = lower, `_Upper` = upper, check.names = FALSE)
  attr(d[["_Lower"]], "label") <- "Lower 95% confidence limit"
  attr(d[["_Upper"]], "label") <- "Upper 95% confidence limit"
  d
}
hrt_labels <- c("No HRT" = 0, "Any HRT" = 1)
dose_labels <- c("No HRT" = 0, "Low dose" = 1, "Medium dose" = 2, "High dose" = 3)
rate11 <- rate_block(hrt_labels, c(42, 31), c(5200, 4980), c(0.00808, 0.00622),
                     c(0.00610, 0.00436), c(0.01069, 0.00887))
rate12 <- rate_block(hrt_labels, c(19, 12), c(5310, 5040), c(0.00358, 0.00238),
                     c(0.00228, 0.00135), c(0.00563, 0.00419))
rate13 <- rate_block(hrt_labels, c(88, 67), c(5150, 4895), c(0.01709, 0.01369),
                     c(0.01384, 0.01062), c(0.02110, 0.01763))
rate21 <- rate_block(dose_labels, c(42, 16, 9, 6), c(5200, 1760, 1510, 1710),
                     c(0.00808, 0.00909, 0.00596, 0.00351), c(0.00610, 0.00557, 0.00310, 0.00158),
                     c(0.01069, 0.01484, 0.01145, 0.00781))
rate22 <- rate_block(dose_labels, c(19, 7, 3, 2), c(5310, 1805, 1535, 1700),
                     c(0.00358, 0.00388, 0.00195, 0.00118), c(0.00228, 0.00185, 0.00063, 0.00029),
                     c(0.00563, 0.00814, 0.00603, 0.00471))
rate23 <- rate_block(dose_labels, c(88, 31, 21, 15), c(5150, 1740, 1495, 1660),
                     c(0.01709, 0.01782, 0.01405, 0.00904), c(0.01384, 0.01253, 0.00917, 0.00545),
                     c(0.02110, 0.02533, 0.02154, 0.01499))

# Stata:
#   stratetab, using(`rate11' `rate12' `rate13' `rate21' `rate22' `rate23') ///
#       outcomes(3) frame(_demo_hr_rates, replace) ///
#       outlabels("Sustained EDSS 4" \ "Sustained EDSS 6" \ "Recurring Relapse") ///
#       outcomeids(edss4 \ edss6 \ relapse) ///
#       explabels("Any HRT" \ "Estrogen Dose")
demo_hr_rates <- stratetab(list(rate11, rate12, rate13, rate21, rate22, rate23),
                           outcomes = 3,
                           outlabels = c("Sustained EDSS 4", "Sustained EDSS 6", "Recurring Relapse"),
                           outcomeids = c("edss4", "edss6", "relapse"),
                           explabels = c("Any HRT", "Estrogen Dose"))

# Binary HRT model frame: one non-reference row
# Stata:
#   clear
#   set seed 20260417
#   set obs 500
#   gen int id = _n
#   gen byte hrt = runiform() < 0.38
#   ... (age, female, education, time and the outcomes edss4, edss6, relapse)
#   collect clear
#   stset time, failure(edss4) id(id)
#   collect: stcox hrt c.age i.female i.education, nolog
#   ... (the same model for edss6 and relapse)
#   regtab, frame(_demo_hr_bin, replace) noint coef("HR") ///
#       models(edss4 \ edss6 \ relapse)
# R: the data Stata drew (demo_data$hrt_bin); i.female and i.education are
# factors.
hrt_bin <- tt_as_factor(demo_data$hrt_bin, vars = c("female", "education"))
hr_model <- function(outcome, exposure, data) {
  f <- stats::as.formula(paste0("survival::Surv(time, ", outcome, ") ~ ", exposure,
                                " + age + female + education"))
  survival::coxph(f, data = data, ties = "breslow")
}
demo_hr_bin <- regtab(lapply(c("edss4", "edss6", "relapse"), hr_model, exposure = "hrt", data = hrt_bin),
                      nointercept = TRUE, coef = "HR", models = c("edss4", "edss6", "relapse"))

# Dose category model frame: three non-reference rows after header + reference
# Stata: (the random-number stream continues)
#   clear
#   set obs 500
#   gen int id = _n
#   gen byte dosecat = floor(runiform() * 4)
#   ... (No HRT / Low / Medium / High dose, and the rest as above)
#   collect: stcox i.dosecat c.age i.female i.education, nolog   (each outcome)
#   regtab, frame(_demo_hr_dose, replace) noint coef("HR") ///
#       models(edss4 \ edss6 \ relapse)
hrt_dose <- tt_as_factor(demo_data$hrt_dose, vars = c("dosecat", "female", "education"))
demo_hr_dose <- regtab(lapply(c("edss4", "edss6", "relapse"), hr_model, exposure = "dosecat", data = hrt_dose),
                       nointercept = TRUE, coef = "HR", models = c("edss4", "edss6", "relapse"))

# Stata:
#   hrcomptab _demo_hr_rates, modelframes(_demo_hr_bin _demo_hr_dose) ///
#       rows(1 \ 3/5) ///
#       outcomemap(edss4 \ edss6 \ relapse) ///
#       xlsx("`xlsx_hrcomptab'") sheet("HR Composite") ///
#       effect("aHR") zebra headershade ///
#       title("Table 19. Hormone Therapy Events, Person-Years, and Adjusted Hazard Ratios") ///
#       footnote("Demo of hrcomptab. The stratetab frame supplies events, person-years, and rates; selected regtab rows supply adjusted hazard ratios and p-values.")
hrcomptab(demo_hr_rates, list(demo_hr_bin, demo_hr_dose),
          rows = list(1, 3:5),
          outcomemap = c("edss4", "edss6", "relapse"),
          xlsx = xlsx_hrcomptab, sheet = "HR Composite",
          effect = "aHR", zebra = TRUE, headershade = TRUE,
          title = "Table 19. Hormone Therapy Events, Person-Years, and Adjusted Hazard Ratios",
          footnote = "Demo of hrcomptab. The stratetab frame supplies events, person-years, and rates; selected regtab rows supply adjusted hazard ratios and p-values.")

# **# Sheets 53-55: puttab -- style an in-memory table (matrix/frame/data) ----
# puttab is the first-mile styled-block producer: it takes a table already in
# memory and writes it as one house-styled Excel sheet. One sheet per source.
# Stata: sysuse auto, clear
auto <- demo_data$auto

# Source 1: a Stata matrix (r(table) from a regression)
# Stata:
#   quietly regress price mpg weight i.foreign
#   matrix _put_T = r(table)'
#   puttab using "`xlsx_puttab'", sheet("Matrix") matrix(_put_T) ///
#       title("Table P1. OLS Coefficients for Car Price") ///
#       digits(3) headershade
# R: Stata's r(table)' is one row per coefficient (the base level
# 0b.foreign included) and the columns b se t pvalue ll ul df crit eform;
# build the same matrix from lm().
auto_ols <- lm(price ~ mpg + weight + factor(foreign), data = auto)
cf <- summary(auto_ols)$coefficients
df_resid <- auto_ols$df.residual
crit <- qt(0.975, df_resid)
rtable_row <- function(term) {
  if (is.na(term)) return(c(0, NA, NA, NA, NA, NA, df_resid, crit, 0))  # the base level
  b <- cf[term, "Estimate"]
  se <- cf[term, "Std. Error"]
  c(b, se, cf[term, "t value"], cf[term, "Pr(>|t|)"], b - crit * se, b + crit * se, df_resid, crit, 0)
}
put_T <- t(sapply(c(mpg = "mpg", weight = "weight", "0b.foreign" = NA,
                    "1.foreign" = "factor(foreign)1", "_cons" = "(Intercept)"), rtable_row))
colnames(put_T) <- c("b", "se", "t", "pvalue", "ll", "ul", "df", "crit", "eform")
puttab(put_T, xlsx = xlsx_puttab, sheet = "Matrix",
       title = "Table P1. OLS Coefficients for Car Price",
       digits = 3, headershade = TRUE)

# Source 2: a named frame (subset of the current data)
# Stata:
#   frame put make mpg price weight in 1/10, into(_put_top)
#   puttab using "`xlsx_puttab'", sheet("Frame") frame(_put_top) ///
#       title("Table P2. First Ten Cars") ///
#       varlabels font(Arial) fontsize(9) borderstyle(academic) zebra
# R has no frames: vars and subset pick the columns and rows of a data frame.
puttab(auto, vars = c("make", "mpg", "price", "weight"), subset = 1:10,
       xlsx = xlsx_puttab, sheet = "Frame",
       title = "Table P2. First Ten Cars",
       varlabels = TRUE, font = "Arial", fontsize = 9, borderstyle = "academic", zebra = TRUE)

# Source 3: the current dataset (a collapse result with value labels)
# Stata:
#   collapse (mean) price mpg (count) n=price, by(foreign)
#   puttab foreign price mpg n using "`xlsx_puttab'", sheet("Collapse") ///
#       title("Table P3. Mean Price and Mileage by Origin") ///
#       varlabels digits(1) borderstyle(academic) ///
#       footnote("n = number of vehicles in each origin group.")
# R: the collapsed data frame, with collapse's variable labels.
auto_means <- data.frame(foreign = haven::labelled(c(0, 1), c(Domestic = 0, Foreign = 1)),
                         price = as.vector(tapply(auto$price, auto$foreign, mean)),
                         mpg = as.vector(tapply(auto$mpg, auto$foreign, mean)),
                         n = as.vector(table(auto$foreign)))
attr(auto_means$foreign, "label") <- "Car origin"
attr(auto_means$price, "label") <- "(mean) price"
attr(auto_means$mpg, "label") <- "(mean) mpg"
attr(auto_means$n, "label") <- "(count) price"
puttab(auto_means, xlsx = xlsx_puttab, sheet = "Collapse",
       title = "Table P3. Mean Price and Mileage by Origin",
       varlabels = TRUE, digits = 1, borderstyle = "academic",
       footnote = "n = number of vehicles in each origin group.")

# **# Sheets 56-59: stacktab -- assemble a composite from puttab-styled blocks ----
# Step 1: emit two estimate/CI blocks as styled sheets with puttab.
# Stata: (the two blocks of the console pipeline above)
#   puttab term ahr ci using "`xlsx_stacktab'", sheet("Block Primary") ///
#       title("Source block: Primary HRT exposure model") varlabels
#   puttab term ahr ci using "`xlsx_stacktab'", sheet("Block Dose") ///
#       title("Source block: Estrogen dose-response model") varlabels
puttab(block_primary, xlsx = xlsx_stacktab, sheet = "Block Primary",
       title = "Source block: Primary HRT exposure model", varlabels = TRUE)
puttab(block_dose, xlsx = xlsx_stacktab, sheet = "Block Dose",
       title = "Source block: Estrogen dose-response model", varlabels = TRUE)

# Step 2 (vstack): merge estimate+CI columns, label each block as a section.
# Stata:
#   stacktab using "`xlsx_stacktab'", sheet("Composite") ///
#       blocks(sheet(Block Primary) rows(2/5) cols(B-D) label(Any HRT use) \ ///
#              sheet(Block Dose) rows(2/4) cols(B-D) label(By estrogen dose)) ///
#       columnmerge(B+C as "aHR (95% CI)") ///
#       title("Table 2. Hormone Therapy and Recurrent Events") ///
#       note("aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age and comorbidities.")
stacktab(hrt_blocks, xlsx = xlsx_stacktab, sheet = "Composite",
         columnmerge = "B+C as \"aHR (95% CI)\"",
         title = "Table 2. Hormone Therapy and Recurrent Events",
         note = "aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age and comorbidities.")

# Step 2 (hstack): place two equal-height blocks side by side.
# Stata:
#   stacktab using "`xlsx_stacktab'", sheet("SideBySide") ///
#       blocks(sheet(Block Primary) rows(2/4) cols(B-D) label(Primary model) \ ///
#              sheet(Block Dose) rows(2/4) cols(B-D) label(Dose-response model)) ///
#       layout(hstack) ///
#       columnmerge(B+C as "aHR (95% CI)" \ E+F as "aHR (95% CI)") ///
#       title("Table 2 supplement. Primary and dose-response estimates side by side")
stacktab(paste("sheet(Block Primary) rows(2/4) cols(B-D) label(Primary model) \\",
               "sheet(Block Dose) rows(2/4) cols(B-D) label(Dose-response model)"),
         xlsx = xlsx_stacktab, sheet = "SideBySide",
         layout = "hstack",
         columnmerge = c("B+C as \"aHR (95% CI)\"", "E+F as \"aHR (95% CI)\""),
         title = "Table 2 supplement. Primary and dose-response estimates side by side")

# **# Verify puttab + stacktab workbook content ----
# Stata: import excel ..., allstring, then assert on the cell text
# (A[1] is the title cell, B[2] the first header cell, ...).
m <- sheet_text(xlsx_puttab, "Matrix")
stopifnot(m[1, 1] == "Table P1. OLS Coefficients for Car Price", any(trimws(m) == "mpg"))
m <- sheet_text(xlsx_stacktab, "Block Primary")
stopifnot(m[1, 1] == "Source block: Primary HRT exposure model",
          m[2, 2] == "Exposure", m[2, 3] == "aHR")
m <- sheet_text(xlsx_stacktab, "Composite")
stopifnot(m[1, 1] == "Table 2. Hormone Therapy and Recurrent Events",
          m[6, 2] == "By estrogen dose", m[6, 3] == "aHR (95% CI)",
          m[7, 2] == "Low dose", m[7, 3] == "0.91 (0.74, 1.12)",
          m[9, 2] == "aHR = adjusted hazard ratio; CI = confidence interval. Models adjusted for age and comorbidities.",
          any(trimws(m) == "aHR (95% CI)"), any(grepl("0.82 (0.69, 0.98)", m, fixed = TRUE)))
m <- sheet_text(xlsx_stacktab, "SideBySide")
stopifnot(m[1, 1] == "Table 2 supplement. Primary and dose-response estimates side by side",
          m[2, 2] == "Primary model", m[2, 3] == "aHR (95% CI)",
          m[2, 4] == "Dose-response model", m[2, 5] == "aHR (95% CI)",
          m[3, 2] == "Any HRT", m[3, 3] == "0.82 (0.69, 0.98)",
          m[3, 4] == "Low dose", m[4, 5] == "0.73 (0.58, 0.92)")

# **# Sheets 47-52: Desctab -- direct descriptive table examples ----
# desctab() is table1_tc() (Stata's table1_tc runs desctab), on sysuse auto.
# Stata:
#   sysuse auto, clear
#   desctab rep78, vars(rep78 cat) ///
#       xlsx("`xlsx_desctab'") sheet("Events") ///
#       title("Repair record distribution")
desctab(auto, vars = c(rep78 = "cat"),
        xlsx = xlsx_desctab, sheet = "Events",
        title = "Repair record distribution")

# Stata:
#   desctab rep78, vars(rep78 cat) ///
#       xlsx("`xlsx_desctab'") sheet("Styled Events") ///
#       title("Repair record distribution") headershade zebra
desctab(auto, vars = c(rep78 = "cat"),
        xlsx = xlsx_desctab, sheet = "Styled Events",
        title = "Repair record distribution", headershade = TRUE, zebra = TRUE)

# Stata:
#   desctab mpg weight, by(foreign) ///
#       vars(mpg contn %6.1f \ weight contn %8.1f) ///
#       xlsx("`xlsx_desctab'") sheet("Mean SD") ///
#       title("Vehicle characteristics by origin")
desctab(auto, by = "foreign",
        vars = c(mpg = "contn %6.1f", weight = "contn %8.1f"),
        xlsx = xlsx_desctab, sheet = "Mean SD",
        title = "Vehicle characteristics by origin")

# Stata:
#   desctab price, by(foreign) vars(price conts %8.0fc) ///
#       xlsx("`xlsx_desctab'") sheet("Median IQR") ///
#       title("Vehicle price by origin")
desctab(auto, by = "foreign", vars = c(price = "conts %8.0fc"),
        xlsx = xlsx_desctab, sheet = "Median IQR",
        title = "Vehicle price by origin")

# Stata:
#   desctab price rep78, by(foreign) ///
#       vars(price contn %8.0fc \ rep78 cat) ///
#       xlsx("`xlsx_desctab'") sheet("Separate Stats") ///
#       title("Price and repair record by origin")
desctab(auto, by = "foreign",
        vars = c(price = "contn %8.0fc", rep78 = "cat"),
        xlsx = xlsx_desctab, sheet = "Separate Stats",
        title = "Price and repair record by origin")

# Stata: (no vars(): each variable's type is detected, as Stata does)
#   desctab price mpg rep78, by(foreign) ///
#       xlsx("`xlsx_desctab'") sheet("Custom") ///
#       title("Mixed descriptive table") smd test
desctab(auto, by = "foreign", vars = c("price", "mpg", "rep78"),
        xlsx = xlsx_desctab, sheet = "Custom",
        title = "Mixed descriptive table", smd = TRUE, test = TRUE)

# **# Verify desctab workbook content ----
stopifnot(sheet_text(xlsx_desctab, "Events")[1, 1] == "Repair record distribution",
          sheet_text(xlsx_desctab, "Mean SD")[1, 1] == "Vehicle characteristics by origin",
          sheet_text(xlsx_desctab, "Median IQR")[1, 1] == "Vehicle price by origin",
          sheet_text(xlsx_desctab, "Custom")[1, 1] == "Mixed descriptive table")
m <- sheet_text(xlsx_desctab, "Separate Stats")
stopifnot(m[1, 1] == "Price and repair record by origin", m[2, 3] == "Domestic", m[2, 4] == "Foreign")

# **# Sheets 60-65: Small-cell disclosure control ----
# The three small tables of the console section (sc_primary, sc_complement,
# sc_binary). Stata asserts r(N_primary_suppressed) and
# r(N_secondary_suppressed); R stores them in the table's $stored.

# **## Primary suppression only ----
# Stata:
#   table1_tc category, by(group) vars(category cat) total(after) ///
#       smallcells(5) ///
#       title("Small-cell suppression: primary counts only") ///
#       xlsx("`xlsx_table1'") sheet("Small Cells Primary")
#   assert r(N_primary_suppressed) == 4
#   assert r(N_secondary_suppressed) == 0
sc <- table1_tc(sc_primary, by = "group", vars = c(category = "cat"), total = "after",
                smallcells = 5,
                title = "Small-cell suppression: primary counts only",
                xlsx = xlsx_table1, sheet = "Small Cells Primary")
stopifnot(sc$stored$N_primary_suppressed == 4, sc$stored$N_secondary_suppressed == 0)

# Stata:
#   desctab category, by(group) vars(category cat) total(after) smallcells(5) ///
#       title("Small-cell suppression: primary counts only") ///
#       xlsx("`xlsx_desctab'") sheet("Small Cells Primary")
sc <- desctab(sc_primary, by = "group", vars = c(category = "cat"), total = "after",
              smallcells = 5,
              title = "Small-cell suppression: primary counts only",
              xlsx = xlsx_desctab, sheet = "Small Cells Primary")
stopifnot(sc$stored$N_primary_suppressed == 4, sc$stored$N_secondary_suppressed == 0)

# Stata:
#   crosstab group category, label smallcells(5) ///
#       title("Small-cell suppression: primary counts only") ///
#       xlsx("`xlsx_crosstab'") sheet("Small Cells Primary")

crosstab(sc_primary, "group", "category", label = TRUE, smallcells = 5,
         title = "Small-cell suppression: primary counts only", xlsx = xlsx_crosstab, sheet = "Small Cells Primary")

# **## Complementary suppression prevents reconstruction ----
# Stata:
#   table1_tc category, by(group) vars(category cat) total(after) ///
#       smallcells(5) ///
#       title("Small-cell suppression: complementary protection") ///
#       xlsx("`xlsx_table1'") sheet("Small Cells Complement")
#   assert r(N_primary_suppressed) == 2
#   assert r(N_secondary_suppressed) == 2
sc <- table1_tc(sc_complement, by = "group", vars = c(category = "cat"), total = "after",
                smallcells = 5,
                title = "Small-cell suppression: complementary protection",
                xlsx = xlsx_table1, sheet = "Small Cells Complement")
stopifnot(sc$stored$N_primary_suppressed == 2, sc$stored$N_secondary_suppressed == 2)

# Stata:
#   desctab category, by(group) vars(category cat) total(after) smallcells(5) ///
#       title("Small-cell suppression: complementary protection") ///
#       xlsx("`xlsx_desctab'") sheet("Small Cells Complement")
sc <- desctab(sc_complement, by = "group", vars = c(category = "cat"), total = "after",
              smallcells = 5,
              title = "Small-cell suppression: complementary protection",
              xlsx = xlsx_desctab, sheet = "Small Cells Complement")
stopifnot(sc$stored$N_primary_suppressed == 2, sc$stored$N_secondary_suppressed == 2)

# Stata:
#   crosstab group category, label smallcells(5) ///
#       title("Small-cell suppression: complementary protection") ///
#       xlsx("`xlsx_crosstab'") sheet("Small Cells Complement")

crosstab(sc_complement, "group", "category", label = TRUE, smallcells = 5,
         title = "Small-cell suppression: complementary protection", xlsx = xlsx_crosstab, sheet = "Small Cells Complement")

# **## Binary variable suppression ----
# Stata:
#   table1_tc rare_ae, by(group) vars(rare_ae bin) total(after) ///
#       smallcells(5) ///
#       title("Small-cell suppression: binary variable") ///
#       xlsx("`xlsx_table1'") sheet("Small Cells Binary")
#   assert r(N_primary_suppressed) == 2
#   assert r(N_secondary_suppressed) == 0
sc <- table1_tc(sc_binary, by = "group", vars = c(rare_ae = "bin"), total = "after",
                smallcells = 5,
                title = "Small-cell suppression: binary variable",
                xlsx = xlsx_table1, sheet = "Small Cells Binary")
stopifnot(sc$stored$N_primary_suppressed == 2, sc$stored$N_secondary_suppressed == 0)

# Stata:
#   desctab rare_ae, by(group) vars(rare_ae bin) total(after) smallcells(5) ///
#       title("Small-cell suppression: binary variable") ///
#       xlsx("`xlsx_desctab'") sheet("Small Cells Binary")
sc <- desctab(sc_binary, by = "group", vars = c(rare_ae = "bin"), total = "after",
              smallcells = 5,
              title = "Small-cell suppression: binary variable",
              xlsx = xlsx_desctab, sheet = "Small Cells Binary")
stopifnot(sc$stored$N_primary_suppressed == 2, sc$stored$N_secondary_suppressed == 0)

# **## Verify primary and complementary markers in every workbook ----
# Stata: count the "<5" and ">=5" cells of each small-cell sheet.
count_cells <- function(xlsx, sheet, text) sum(trimws(sheet_text(xlsx, sheet)) == text)
for (book in c(xlsx_table1, xlsx_desctab)) {
  stopifnot(sheet_text(book, "Small Cells Primary")[1, 1] == "Small-cell suppression: primary counts only",
            count_cells(book, "Small Cells Primary", "<5") == 4,
            count_cells(book, "Small Cells Primary", "\u22655") == 0,
            sheet_text(book, "Small Cells Complement")[1, 1] == "Small-cell suppression: complementary protection",
            count_cells(book, "Small Cells Complement", "<5") == 2,
            count_cells(book, "Small Cells Complement", "\u22655") == 2,
            sheet_text(book, "Small Cells Binary")[1, 1] == "Small-cell suppression: binary variable",
            count_cells(book, "Small Cells Binary", "<5") >= 2)
}

# **# Sheets: regtab 2.3 options -- cformat, flat frame + reftop, transpose, cellnote, fit counts ----
regtab(layout_adjusted, xlsx = xlsx_regtab, sheet = "Regtab cformat", nointercept = TRUE,
       cformat = "%5.3f", sep = " to ", title = "Table 2b. CV Events (Three Decimals, 'to' Intervals)")
flat_fit <- glm(cv_event ~ treated + index_age + female + education, family = binomial,
                data = analysis, contrasts = list(education = contr.treatment(3, base = 3)))
flat_table <- regtab(flat_fit, reftop = TRUE, compact = TRUE, nointercept = TRUE)
flat_keyed <- tt_flat(flat_table)
stopifnot(all(c("_term", "_rowtype") %in% names(flat_keyed)))
puttab(tt_flat(flat_table, keyed = FALSE), xlsx = xlsx_regtab, sheet = "Flat Reftop", varlabels = TRUE,
       title = "Table 2c. regtab flat frame written by puttab (reference level on top)")
regtab(layout_crude, layout_adjusted, transpose = TRUE, keep = "treated", stats = "n", nopvalue = TRUE,
       models = c("Crude", "Adjusted"), xlsx = xlsx_regtab, sheet = "Transpose",
       title = "Table 2d. Treatment Odds Ratio by Model Specification")
regtab(layout_crude, layout_adjusted, nointercept = TRUE, nopvalue = TRUE,
       cellnote = list(list(row = "Diabetes", model = 1L, text = "Not in model")), models = c("Crude", "Adjusted"),
       xlsx = xlsx_regtab, sheet = "Cell Note", title = "Table 2e. Text in a Cell That Must Not Show an Estimate")
masked_regression <- regtab(fit_crude, fit_adjusted, fitcounts = list(counts_crude, counts_adjusted),
  stats = c("events", "people", "exposure"), exposurelabel = "Person-years", mincount = 15,
  models = c("Crude", "Adjusted"), xlsx = xlsx_regtab, sheet = "Fit Counts",
  title = "Table 2f. GI Bleeding by Region, Patients Aged 80+", footnote = "Regions with fewer than 15 events are not estimated (–).")
stopifnot(masked_regression$stored$N_masked > 0)

# **# Sheet: effecttab cformat() ----
effecttab(list(ipw), data = analysis, xlsx = xlsx_effecttab, sheet = "ATE cformat",
          effect = "ATE", method = "ipw", cformat = "%6.4f",
          tlabels = c("0" = "SSRI", "1" = "SNRI"),
          title = "Table 6b. Average Treatment Effect, Four Decimals")

# **# Sheets: comptab cformat(), cisep(), and a flat frame ----
formatted_model <- survival::coxph(survival::Surv(follow_up, cv_event) ~ treated + index_age + female + diabetes + hypertension,
                                 data = analysis, ties = "breslow", model = TRUE, x = TRUE)
formatted_table <- regtab(formatted_model, nointercept = TRUE, coef = "HR")
formatted_composite <- comptab(list(formatted_table), rows = c(1, 4, 5), cformat = "%5.3f", cisep = " to ",
  xlsx = xlsx_comptab, sheet = "Composite Formatted", title = "Table S3. Selected Hazard Ratios (Three Decimals)")
puttab(tt_flat(formatted_composite, keyed = FALSE), xlsx = xlsx_comptab, sheet = "Composite Flat", varlabels = TRUE,
       title = "Table S3 flat frame written by puttab")

# **# Sheets: ratetab -- rates from stset data, then rates + models with comptab ----
ratetab(rate_data, c("treated", "education"), events = "cv_event", exposure = "person_years", outlabels = "CV events",
        explabels = c("Treatment", "Education"), xlsx = xlsx_ratetab, sheet = "Rates Exact",
        title = "Table R1. CV Event Rates per 1,000 Person-Years (Exact Poisson CI)")
ratetab(rate_data, c("treated", "education"), events = "cv_event", exposure = "person_years", ci = "poisson", per = 100,
        outlabels = "CV events", explabels = c("Treatment", "Education"), xlsx = xlsx_ratetab, sheet = "Rates Poisson",
        title = "Table R2. CV Event Rates per 100 Person-Years (Normal-Approximation CI)")
ratetab(rate_data, "treated", events = "cv_event", exposure = "person_years", ci = "cluster", cluster = "region", outlabels = "CV events",
        xlsx = xlsx_ratetab, sheet = "Rates Cluster", title = "Table R3. CV Event Rates with Region-Clustered CI")
ratetab(rate_data, c("treated", "education"), events = "cv_event", exposure = "person_years", cformat = "%5.2f", sep = " to ",
        outlabels = "CV events", explabels = c("Treatment", "Education"), xlsx = xlsx_ratetab, sheet = "Rates Formatted",
        title = "Table R4. CV Event Rates, Two Decimals")
rate_scaffold <- ratetab(rate_data, "education", events = "cv_event", exposure = "person_years", outlabels = "CV events")
rate_crude <- survival::coxph(survival::Surv(follow_up, cv_event) ~ education, data = analysis, ties = "breslow", model = TRUE, x = TRUE)
rate_adjusted <- survival::coxph(survival::Surv(follow_up, cv_event) ~ education + treated + index_age + female + diabetes + hypertension,
                              data = analysis, ties = "breslow", model = TRUE, x = TRUE)
rate_models <- regtab(rate_crude, rate_adjusted, nointercept = TRUE, compact = TRUE, models = c("Crude", "Adjusted"), keep = "education")
comptab(rate_scaffold, modeltables = list(rate_models), rows = "all", allmodels = TRUE, keyed = TRUE, effect = "HR",
        xlsx = xlsx_ratetab, sheet = "Rates Models", title = "Table R5. CV Events, Rates, and Hazard Ratios by Education",
        footnote = "HR = hazard ratio. \\ Adjusted for treatment, age, sex, diabetes, and hypertension.")

# **# Sheets: outtab -- binary outcomes by exposure ----
outtab(analysis, c("cv_event", "selfharm", "fracture", "gi_bleed"), exposure = "treated",
       models = list(~1, ~index_age + female + education + diabetes + hypertension), modellabels = c("Crude", "Adjusted"),
       estimator = "modified_poisson", ratiolabel = "RR", xlsx = xlsx_outtab, sheet = "Outcomes",
       title = "Table 3. Outcomes by Treatment: Events/N and Risk Ratios", footnote = "RR = risk ratio from modified Poisson regression with robust SE.")
outcome_panels <- analysis
outcome_panels$all_patients <- 1L
outcome_panels$age_65plus <- as.integer(outcome_panels$index_age >= 65)
attr(outcome_panels$all_patients, "label") <- "All patients"
attr(outcome_panels$age_65plus, "label") <- "Aged 65 and over"
outtab(outcome_panels, c("cv_event", "gi_bleed"), exposure = "treated", models = list(~1, ~index_age + female),
       modellabels = c("Crude", "Adjusted"), estimator = "logit", ratiolabel = "OR", panels = c("all_patients", "age_65plus"),
       xlsx = xlsx_outtab, sheet = "Outcomes OR Panels", title = "Table 3b. Outcomes by Treatment in Two Analysis Panels")

# **# Sheet: tabcell columns, written to a tabtools session workbook ----
# Stata collapse percentiles use the discontinuity-averaged (type2) rule.
education_levels <- sort(unique(as.numeric(demo_data$cohort$education)))
cell_summary <- data.frame(education = haven::labelled(education_levels,
  labels = attr(demo_data$cohort$education, "labels", exact = TRUE)))
cell_summary$n <- vapply(education_levels, function(code) sum(as.numeric(demo_data$cohort$education) == code), 0L)
cell_summary$events <- vapply(education_levels, function(code) sum(analysis$cv_event[as.numeric(demo_data$cohort$education) == code]), 0)
cell_quartiles <- t(vapply(education_levels, function(code) quantile(analysis$follow_up[as.numeric(demo_data$cohort$education) == code],
  c(.25, .5, .75), type = 2, na.rm = TRUE, names = FALSE), numeric(3)))
cell_summary$ev_cell <- tabcell("enp", e = cell_summary$events, n = cell_summary$n)
cell_summary$fu_cell <- tabcell("iqr", median = cell_quartiles[, 2], q1 = cell_quartiles[, 1], q3 = cell_quartiles[, 3], format = "%6.0fc")
attr(cell_summary$education, "label") <- attr(demo_data$cohort$education, "label", exact = TRUE)
attr(cell_summary$ev_cell, "label") <- "CV events, n (%)"
attr(cell_summary$fu_cell, "label") <- "Follow-up (days), median (IQR)"
tabtools_options(workbook = xlsx_tabcell, headershade = TRUE)
puttab(cell_summary, vars = c("education", "ev_cell", "fu_cell"), sheet = "Tabcell Column", varlabels = TRUE,
       title = "Table T1. CV Events and Follow-up by Education (tabcell cells)")
tabtools_options(workbook = NULL, headershade = NULL)

# **# Sheets: puttab 2.2/2.3 layout options -- hlines/boldrows, panels, spans ----
layout_data <- data.frame(group = c("Domestic", "Foreign", "Repair", "Good (4-5)", "Poor (1-3)"),
                          n = c("52", "22", "N", "29", "40"), price = c("6,072", "6,385", "Price", "6,013", "6,118"))
puttab(layout_data, xlsx = xlsx_puttab, sheet = "Stacked Tables", hlines = c(3, 4), boldrows = 3,
       title = "Table P4. Two Tables Stacked in One Source")
panel_data <- data.frame(row = c("Under 55", "55 and over", "Repleted"),
                         c1 = c("12", "9", "40"), c2 = c("310", "280", "18"),
                         blk = haven::labelled(c(1, 1, 2), c("A. Relapses" = 1, "B. New MRI activity" = 2)),
                         h0 = "", h1 = c("Relapses", "Relapses", "Scans"), h2 = c("Person-years", "Person-years", "Percent"))
puttab(panel_data, vars = c("row", "c1", "c2"), xlsx = xlsx_puttab, sheet = "Panels", noheader = TRUE,
       panel = "blk", panelheader = c("h0", "h1", "h2"), title = "Table P5. Panels with Their Own Headers",
       footnote = "Counts are crude. \\ Ratios are adjusted.")
attr(panel_data$row, "label") <- "Age group"
attr(panel_data$c1, "label") <- "Events"
attr(panel_data$c2, "label") <- "Exposure"
# The successful returned sink is recorded by QA; no second export occurs.
tabtools_options(workbook = xlsx_puttab)
puttab(panel_data, vars = c("row", "c1", "c2"), sheet = "Spans", varlabels = TRUE,
       spanheader = list(list(text = "Counts", first = 2L, last = 3L)), title = "Table P6. A Spanning Header")
tabtools_options(workbook = NULL)

# **# Sheet: stacktab frames() -- stack table1_tc frames as panels ----
frame_all <- table1_tc(analysis, by = "treated", vars = c(index_age = "contn %5.1f", female = "bin", education = "cat"))
frame_old <- table1_tc(analysis[analysis$index_age >= 65, ], by = "treated",
                       vars = c(index_age = "contn %5.1f", female = "bin", education = "cat"))
# Native desctab.ado:1001 labels the stub "Factor "; its frame keeps the
# descriptor/N row as data. Expose those publication rows without recomputing
# the analysis or using the TT-to-puttab header-collapse convenience path.
native_table1_frame <- function(tt, source_data = analysis) {
  stopifnot(identical(tt$command, "table1_tc"), length(tt$header) == 2L)
  body <- as.matrix(tt$body)
  # Stata's if keeps variable labels. R row subsetting loses custom label
  # attributes on plain numeric/factor columns; restore only raw-name variable
  # rows by their source identity, preserving levels and publication overrides.
  if (!is.null(source_data) && is.data.frame(tt$rows) &&
      all(c("type", "var") %in% names(tt$rows))) {
    for (r in which(tt$rows$type %in% c("var", "cat_header"))) {
      variable <- tt$rows$var[r]
      if (is.na(variable) || !variable %in% names(source_data) ||
          !identical(unname(body[r, 1L]), variable)) next
      label <- attr(source_data[[variable]], "label", exact = TRUE)
      if (is.character(label) && length(label) == 1L && !is.na(label) && nzchar(label)) {
        body[r, 1L] <- label
      }
    }
  }
  data <- as.data.frame(rbind(tt$header[[2L]]$text, body),
                        stringsAsFactors = FALSE)
  labels <- tt$header[[1L]]$text
  labels[1L] <- "Factor "
  names(data) <- paste0("c", seq_len(ncol(data)))
  for (j in seq_along(data)) attr(data[[j]], "label") <- labels[j]
  attr(data, "sample_accounting") <- tt$meta$sample_accounting
  data
}
stacktab(frames = list(all = list(data = native_table1_frame(frame_all), label = "All patients"),
                       old = list(data = native_table1_frame(frame_old), label = "Aged 65 and over")),
         xlsx = xlsx_stacktab, sheet = "Frames",
         title = "Table 1. Baseline Characteristics, All Patients and Aged 65+")

# **# Sheets: table1_tc/desctab/crosstab smallcells(#, primary) and cellreplace() ----
primary_demo <- table1_tc(sc_complement, by = "group", vars = c(category = "cat"), total = "after",
                            smallcells = 5, smallcells_mode = "primary",
                            title = "Small-cell suppression: primary mode", xlsx = xlsx_table1, sheet = "Small Cells Primary Mode")
stopifnot(identical(primary_demo$stored$smallcells$n_masked, 2L), identical(primary_demo$stored$smallcells$n_linked, 0L))
desctab(sc_complement, by = "group", vars = c(category = "cat"), total = "after", smallcells = 5, smallcells_mode = "primary",
        title = "Small-cell suppression: primary mode", xlsx = xlsx_desctab, sheet = "Small Cells Primary Mode")
crosstab(sc_complement, "group", "category", label = TRUE, smallcells = 5, smallcells_mode = "primary",
         title = "Small-cell suppression: primary mode", xlsx = xlsx_crosstab, sheet = "Small Cells Primary Mode")
replace_demo <- table1_tc(analysis, by = "treated", vars = c(education = "cat", civil_status = "cat"),
                           cellreplace = list(list(row = "Widowed", column = "SNRI", text = "Not reported")),
                           title = "Table 1. cellreplace() Overwrites One Cell", xlsx = xlsx_table1, sheet = "Cell Replace")
stopifnot(identical(replace_demo$stored$n_cellreplace, 1L))

# **# Verify 2.3 workbook content ----
# Source refresh adds native-equivalent publications without changing the
# authenticated dataset or prior compared calls. Root's reviewed installed demo
# parity run asserts every newly enrolled body, style and annotation boundary.
stopifnot(identical(tabtools_options()$workbook, NULL), is.null(tabtools_options()$headershade))

# **# Convert console output to markdown ----
# Stata: logdoc using "`console_log'", output("`console_md'") format(md) replace quiet
# R: console() wrote console_output.md alongside the log.

# **# Cleanup ----
message("Demo complete. Outputs:")
for (f in c(console_log, console_md, markdown_report, xlsx_table1, xlsx_desctab, xlsx_regtab,
            xlsx_regtab_models, xlsx_comptab, xlsx_effecttab, xlsx_stratetab, xlsx_hrcomptab,
            xlsx_puttab, xlsx_stacktab, xlsx_ratetab, xlsx_outtab, xlsx_tabcell, xlsx_corrtab, xlsx_crosstab, xlsx_survtab)) {
  if (!file.exists(f)) stop("Expected demo artifact not found: ", f)
  message("  ", f)
}

# The session's own tabtools_options(), cleared at the start.
tabtools_options(clear = TRUE)
if (length(user_tabtools_options)) do.call(tabtools_options, user_tabtools_options)

})
