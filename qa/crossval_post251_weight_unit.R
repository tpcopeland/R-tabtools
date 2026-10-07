library(testthat)
library(tabtools)
root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
if (!nzchar(root)) root <- if (file.exists("DESCRIPTION")) getwd() else dirname(getwd())
sys.source(file.path(root, "qa", "helper-post251-native.R"), envir = environment())

p256_weight_data <- function(k) {
  n <- if (k == 1L) 8L else if (k == 4L) 40L else 12L
  g <- if (k == 4L) rep(1:2, each = 20L) else rep(seq_len(n/4L), each = 4L)
  x <- if (k == 4L) 3001 + (seq_len(n)-1L) %% 20L else (seq_len(n)-1L) %% 4L + 1 + 2*(g-1)
  # The retained DTA stores Stata's evaluated 2^1020 at this exact binary
  # double (17-digit roundtrip), 3.12e-14 below R's power expression.
  # Match authentic caller values, rather than silently changing the input.
  data.frame(g = g, x = x, unit = 1,
    huge = if (k == 4L) 1e300 else 1.1235582092889124e307)
}
for (k in 1:4) local({
  current <- k
  test_that(paste0("WX0", current, " complete ordinary/huge-weight native publication and returns"), {
    p256_clear(environment()); proof <- p256_auth("weight-unit"); h <- p256_helpers()
    expect_identical(as.character(p256_vector(proof$oracle$case_ids)), c(as.vector(rbind(paste0("WX0",1:4,"-unit"),paste0("WX0",1:4,"-huge"))),paste0("UL0",1:4)))
    data <- p256_weight_data(current); before <- data
    tables <- list()
    for (weight in c("unit", "huge")) {
      id <- paste0("WX0", current, "-", weight); case <- p256_case(proof, id)
      p256_input(data, proof, id)
      scratch <- withr::local_tempdir(pattern = paste0("post251-", id, "-"))
      paths <- list(csv = file.path(scratch,"case.csv"),md = file.path(scratch,"case.md"),xlsx = file.path(scratch,"case.xlsx"))
      args <- list(data = data, by = "g", vars = "x contn", wt = weight, nopvalue = TRUE,
        csv = paths$csv, markdown = paths$md, xlsx = paths$xlsx, sheet = "Table")
      if (current < 4L) {args$smd <- TRUE; args$smdtype <- c("pair","maxpair","population")[current]}
      r <- do.call(table1_tc,args); tables[[weight]] <- r
      expect_identical(data,before);expect_s3_class(r,"tt_table");expect_identical(r$command,"table1_tc")
      expect_identical(r$stored$sheet,"Table")
      p256_table1_frame(r,proof,id)
      p256_stored(r,case,c("markdown_rows","markdown_cols","Dapa","varlist"))
      note <- switch(as.character(current), "2" = "Max SMD: largest absolute pairwise difference across the 3 groups, in the root-mean of the group variances.",
        "3" = "Pop. SB: largest absolute difference between a group mean and the overall mean, in overall-sample SDs, across the 3 groups (McCaffrey et al. 2013).", NULL)
      expect_identical(r$footnote, if (is.null(note)) "" else note)
      if (current < 4L) {
        want <- p256_matrix(case$matrix)
        expect_identical(dimnames(r$stored$table),dimnames(want));expect_identical(dim(r$stored$table),c(1L,1L))
        expect_equal(r$stored$table,want,tolerance=1e-12)
        p256_stored(r,case,c("smdtype"))
        if (!is.null(note)) {expect_identical(r$stored$smdnote,note);expect_identical(r$meta$console_before,paste("Note:",note))}
      } else {
        # The recipe's later matrix=r(table) creates a ghost missing matrix;
        # the authentic return list contains no analytical r(table).
        expect_true(isTRUE(case$absent_table));expect_null(case$matrix);expect_null(r$stored$table)
        expect_null(r$stored$smdtype);expect_null(r$stored$smdnote)
      }
      p256_console(r,case,"weight",note);p256_sinks(r,paths,proof,id,h,note)
    }
    expect_identical(dim(as.data.frame(tables$unit)),dim(as.data.frame(tables$huge)))
    expect_identical(unname(as.matrix(as.data.frame(tables$unit))),unname(as.matrix(as.data.frame(tables$huge))),info="complete ordinary/huge publication invariant; separate source ledgers retained")
    expect_identical(tables$unit$stored$table,tables$huge$stored$table,info="raw SMD invariant under common rescaling")
  })
})

for (k in 1:4) local({
  current <- k
  test_that(paste0("UL0",current," native rate-unit text, every saved field and every sink"),{
    p256_clear(environment());proof<-p256_auth("weight-unit");h<-p256_helpers()
    id<-paste0("UL0",current);case<-p256_case(proof,id)
    data<-data.frame(g=1,e=1,y=10);before<-data;p256_input(data,proof,id)
    scratch<-withr::local_tempdir(pattern=paste0("post251-",id,"-"))
    paths<-list(csv=file.path(scratch,"case.csv"),md=file.path(scratch,"case.md"),xlsx=file.path(scratch,"case.xlsx"))
    saving<-file.path(scratch,"saved.dta")
    args<-list(data=data,by="g",events="e",exposure="y",per=c(1e-6,.5,1500.5,1e-6)[current],
      csv=paths$csv,markdown=paths$md,xlsx=paths$xlsx,sheet="Table",saving=saving)
    if(current==4L)args$unitlabel<-"micro-unit"
    r<-do.call(ratetab,args);expect_identical(data,before);expect_s3_class(r,"tt_table")
    expect_identical(r$command,"ratetab");expect_identical(r$stored$sheet,"Table")
    units<-c("1e-06","0.5","1,500.5","micro-unit")
    expect_identical(r$header[[2L]]$text[4L],paste0("Per ",units[current]," PY (95% CI)"))
    methods<-paste0("Incidence rates per ",units[current]," person-years with 95% confidence intervals from exact Poisson limits for the event count.")
    expect_identical(case$macros$methods,methods);expect_identical(r$stored$methods,methods)
    p256_stored(r,case,c("N_nopt","N_noci","N_zero","level","per","N","ci_level","N_outcomes","N_exposures","N_rows","markdown_cols","markdown_rows","ci_method","outcome_ids"))
    expect_identical(r$stored$smallcells,list(threshold=0L,mode="primary",n_masked=0L,n_linked=0L))
    expect_identical(case$scalars$smallcells,0)
    want<-p256_matrix(case$matrix)
    expect_identical(dim(r$stored$estimates),c(1L,8L));expect_identical(colnames(r$stored$estimates),colnames(want));expect_null(rownames(r$stored$estimates));expect_identical(rownames(want),"r1")
    expect_equal(unname(r$stored$estimates),unname(want),tolerance=1e-12)
    expect_identical(dim(r$stored$rates),c(1L,1L));expect_identical(dimnames(r$stored$rates),list("_1","e"));expect_equal(as.double(r$stored$rates),unname(want[1L,"rate"]),tolerance=1e-12)
    frame<-haven::read_dta(file.path(proof$directory,"out",paste0(id,"-frame.dta")))
    expect_identical(names(frame),c("title","c1","c2","c3","c4"));expect_identical(dim(frame),c(5L,5L))
    expect_identical(as.vector(frame$title),rep("",5L));expect_true(all(as.matrix(frame[1L,])==""))
    expect_identical(unname(as.matrix(as.data.frame(r))),unname(as.matrix(frame[-1L,-1L])),info="complete rate publication frame")
    for(name in names(frame)) {expect_null(attr(frame[[name]],"label",exact=TRUE));expect_null(attr(frame[[name]],"labels",exact=TRUE))}
    saved<-haven::read_dta(saving);original<-haven::read_dta(file.path(proof$directory,"out",paste0(id,"-saved.dta")))
    expect_identical(names(saved),c("outcome","outcome_var","outcome_label","group","groupvar","level","level_label","events","persontime","rate","lb","ub","masked","nopersontime","g"))
    expect_identical(names(saved),names(original));expect_identical(dim(saved),dim(original))
    for(name in names(original)) {
      if(is.numeric(original[[name]]))expect_equal(as.double(saved[[name]]),as.double(original[[name]]),tolerance=1e-12,info=paste(id,name,"all raw saved values"))
      else expect_identical(as.vector(saved[[name]]),as.vector(original[[name]]),info=paste(id,name,"all saved text"))
      expect_identical(attr(saved[[name]],"label",exact=TRUE),attr(original[[name]],"label",exact=TRUE),info=paste(id,name,"exact variable label"))
      expect_identical(attr(saved[[name]],"labels",exact=TRUE),attr(original[[name]],"labels",exact=TRUE),info=paste(id,name,"exact value labels"))
      if(is.numeric(original[[name]]))expect_identical(haven::is_tagged_na(saved[[name]]),haven::is_tagged_na(original[[name]]))
      else expect_identical(is.na(saved[[name]]),is.na(original[[name]]))
    }
    expect_identical(attr(saved$rate,"label",exact=TRUE),paste0("Rate per ",units[current]," person-time"))
    p256_console(r,case,"rate");p256_sinks(r,paths,proof,id,h)
  })
})
