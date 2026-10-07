library(testthat)
library(tabtools)
source_root <- Sys.getenv("TABTOOLS_QA_SOURCE_ROOT")
if (!nzchar(source_root)) {
  source_root <- if (file.exists(file.path(getwd(),"DESCRIPTION"))) getwd() else dirname(getwd())
}
source_root <- normalizePath(source_root, mustWork = TRUE)
helpers <- new.env(parent = globalenv())
# QA-local existing native readers/comparators; no source-package loading.
for (name in c("helper-golden.R", "helper-golden-footnotes.R", "helper-golden-tidy.R"))
  sys.source(file.path(source_root, "tests", "testthat", name), envir = helpers)
native <- Sys.getenv("TABTOOLS_UPSTREAM256_NATIVE_DIR",
  unset=file.path(source_root,"tests","testthat","golden","post251-layout","out"))
if (!nzchar(native)) stop("TABTOOLS_UPSTREAM256_NATIVE_DIR must name ROOT-authenticated native output.")
native <- normalizePath(native, mustWork = TRUE)
receipt_path <- file.path(dirname(native), "native-receipt-qualified.json")
receipt <- jsonlite::fromJSON(receipt_path)
sha <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
test_that("the exact new-pin native seven-case capture is authenticated and complete", {
  expect_identical(receipt$source_pin, "4eecca4d09d61df0cfe773fc2024b015920b5fe6")
  expect_identical(receipt$recipe_sha256, "0661afbefb1d0ced1e266ce40b47298a7c854d64e6a15198d6c69dff5deaac10")
  expect_identical(receipt$cleanup_verified, TRUE)
  expect_identical(sort(list.files(native)), sort(names(receipt$files)))
  expect_length(receipt$files, 26L)
  for (f in names(receipt$files)) expect_identical(sha(file.path(native,f)), receipt$files[[f]], info=f)
  expect_identical(tidyxl::xlsx_sheet_names(file.path(native,"upstream256_layout.xlsx")),
    c("US001","US002","US003","UW001","UW002","UW003","UW004"))
})

# Strict projection of the authenticated full Stata case log.
# Exact producer/end boundaries exclude wrappers and returned-value diagnostics.
native_command_output <- function(lines,id) {
  joined <- character()
  for(line in lines) {
    if(startsWith(line,"> ")) {
      if(!length(joined)) stop("Orphan native log continuation")
      joined[length(joined)] <- paste0(joined[length(joined)],substring(line,3L))
    } else joined <- c(joined,line)
  }
  commands <- c(
    US001=". puttab lab a b c using `\"`book'\"', sheet(US001) panel(pan) title(\"T\") csv(`\"`output'/US001.csv\"') markdown(`\"`output'/US001.md\"')",
    US002=". puttab lab a b c using `\"`book'\"', sheet(US002) panel(pan) csv(`\"`output'/US002.csv\"') markdown(`\"`output'/US002.md\"')",
    US003=". puttab lab a b c using `\"`book'\"', sheet(US003) panel(pan) spanheader(`\"`span'\"' 2/3) csv(`\"`output'/US003.csv\"') markdown(`\"`output'/US003.md\"')",
    UW001=". table1_tc, by(g) vars(x contn \\ b bin) smd smallcells(3) excel(`\"`book'\"') sheet(UW001) frame(uw001, replace) csv(`\"`output'/UW001.csv\"') markdown(`\"`output'/UW001.md\"')",
    UW002=". table1_tc, by(g) vars(x contn) smd excel(`\"`book'\"') sheet(UW002) frame(uw002, replace) csv(`\"`output'/UW002.csv\"') markdown(`\"`output'/UW002.md\"')",
    UW003=". table1_tc, by(g) vars(x contn) smd excel(`\"`book'\"') sheet(UW003) frame(uw003, replace) csv(`\"`output'/UW003.csv\"') markdown(`\"`output'/UW003.md\"')",
    UW004=". table1_tc, by(g) vars(x contn) smd excel(`\"`book'\"') sheet(UW004) frame(uw004, replace) csv(`\"`output'/UW004.csv\"') markdown(`\"`output'/UW004.md\"')")
  start <- which(joined==commands[[id]])
  finish <- which(joined==". return list")
  if(length(start)!=1L || length(finish)!=1L || finish<=start+1L) stop("Native command-output boundary mismatch")
  logs <- joined[grepl("^       log:  ",joined)]
  if(length(logs)!=2L || !identical(logs[1L],logs[2L])) stop("Native case log path boundary mismatch")
  logpath <- substring(logs[1L],14L)
  if(!identical(basename(logpath),paste0(id,".console.log"))) stop("Native case log identity mismatch")
  output <- joined[seq.int(start+1L,finish-1L)]
  # The producer emits exactly one empty separator before return list.
  if(!identical(tail(output,1L),"")) stop("Native command separator missing")
  list(lines=head(output,-1L),directory=dirname(logpath))
}

run_layout_case <- function(id) {
  scratch <- tempfile(paste0("tabtools-upstream256-",id,"-"))
  if (!dir.create(scratch)) stop("Cannot create owned QA scratch")
  on.exit(unlink(scratch,recursive=TRUE),add=TRUE)
  files <- list(xlsx=file.path(scratch,"case.xlsx"),csv=file.path(scratch,"case.csv"),markdown=file.path(scratch,"case.md"))
  if (startsWith(id,"US")) {
    d <- data.frame(lab=c("Short","Other","Third"),a=c("1","4","7"),b=c("2","5","8"),c=c("3","6","9"),
      pan=c(rep("Any narcolepsy code on or before delivery",2L),"Second"))
    if (id != "US001") d$pan[1:2] <- "A heading that is far longer than fifty characters, it really is long"
    before <- d
    args <- c(list(x=d,panel="pan",sheet=id),files)
    if (id=="US001") args$title <- "T"
    if (id=="US003") args$spanheader <- list(list(text=strrep("Long span ",20L),first=2L,last=3L))
    tt <- suppressMessages(do.call(puttab,args))
    expect_identical(d,before)
  } else {
    d <- data.frame(g=rep(1:2,each=25L),x=1:50,b=c(rep(0,24L),1,rep(0,5L),rep(1,20L)))
    args <- c(list(by="g",vars="x contn",smd=TRUE,sheet=id),files)
    if(id=="UW001") {args$vars<-"x contn \\ b bin";args$smallcells<-3L}
    if(id %in% c("UW003","UW004")) {
      d <- data.frame(g=factor(rep(1:3,each=6L),levels=1:3,labels=if(id=="UW003")c("Primary","Secondary","Tertiary")else c("東","B","C")),x=1:18)
    }
    before<-d;args$data<-d
    tt<-suppressMessages(do.call(table1_tc,args));expect_identical(d,before)
  }
  expect_s3_class(tt,"tt_table")
  expect_identical(tt$command, if(startsWith(id,"US"))"puttab"else"table1_tc")
  expect_identical(tt$stored$sheet,id)
  expect_true(is.data.frame(tt$body) && all(vapply(tt$body,is.character,TRUE)))
  console <- capture.output(print(tt))
  native_log<-readLines(file.path(native,paste0(id,".console.log")),warn=FALSE,encoding="UTF-8")
  native_output <- native_command_output(native_log,id)
  native_console <- native_output$lines
  exports <- c(paste("CSV exported to",file.path(native_output$directory,paste0(id,".csv"))),
    paste("Markdown exported to",file.path(native_output$directory,paste0(id,".md"))))
  if (startsWith(id,"US")) {
    # Native puttab reports a write, while R prints its publication grid.
    # This explicit command-specific console contract does not alter a comparator.
    expect_identical(native_console,c(exports[2L],paste0("puttab: wrote 5 data rows x 4 cols (data source) to sheet ",id,
      " in ",file.path(native_output$directory,"upstream256_layout.xlsx"))))
    heading <- if(id=="US001") "Any narcolepsy code on or before delivery" else "A heading that is far longer than fifty characters, it really is long"
    grid <- rbind(c("lab","a","b","c"),c(heading,"","",""),c("   Short","1","2","3"),
      c("   Other","4","5","6"),c("Second","","",""),c("   Third","7","8","9"))
    if(id=="US003") grid <- rbind(c("",strrep("Long span ",20L),"",""),grid)
    widths <- apply(grid,2L,function(v)max(nchar(v,type="width")))
    inner <- sum(widths)+11L
    edge <- paste0("  +",strrep("-",inner),"+")
    lines <- apply(grid,1L,function(row)paste0("  | ",paste(mapply(function(v,w)paste0(strrep(" ",w-nchar(v,type="width")),v),row,widths),collapse="   ")," |"))
    expected <- c(if(id=="US001") c("","T"),edge,lines,edge,"")
    expect_identical(console,expected,info=id)
    expect_identical(tt$stored$n_panels,2L)
    expect_identical(tt$stored$n_datarows,5L)
  } else {
    expect_identical(tail(native_console,2L),exports,info=paste(id,"complete export messages"))
    native_console <- head(native_console,-2L)
    expect_identical(sum(grepl("^  \\+-+\\+$",native_console)),2L,info=paste(id,"complete publication box"))
    # R publishes every public footnote as a separate paragraph after the box.
    # Native UW003/004 prints its SMD note above the box only. The full native
    # grid/prefix remains exact; only these declared R paragraph additions differ.
    paragraph <- switch(id,
      UW003="SMD compares Primary vs Secondary only (the first two of 3 groups).",
      UW004="SMD compares 東 vs B only (the first two of 3 groups).",
      character())
    expected <- if(id=="UW001") c(native_console,"") else
      if(id %in% c("UW003","UW004")) c(native_console,paragraph,"") else native_console
    expect_identical(console,expected,info=paste(id,"complete R grid and publication paragraphs"))
  }
  expect_identical(length(helpers$golden_compare_sink(files$csv,file.path(native,paste0(id,".csv")))),0L,info=paste(id,"CSV"))
  expect_identical(length(helpers$golden_compare_sink(files$markdown,file.path(native,paste0(id,".md")))),0L,info=paste(id,"Markdown"))
  expect_identical(length(helpers$golden_compare_styles(files$xlsx,id,file.path(native,"upstream256_layout.xlsx"),id,
    got_width_offset=helpers$golden_r_width_offset)),0L,info=paste(id,"full XLSX cells/styles/merges/widths/heights"))
  old <- tt
  tt_write_csv(tt,file.path(scratch,"later.csv"));tt_write_markdown(tt,file.path(scratch,"later.md"))
  expect_identical(helpers$golden_bytes(file.path(scratch,"later.csv")),helpers$golden_bytes(files$csv))
  expect_identical(helpers$golden_bytes(file.path(scratch,"later.md")),helpers$golden_bytes(files$markdown))
  expect_identical(tt,old)
  if (id=="UW001") expect_identical(tt$footnote,
    "Counts below 3 are shown as <3; complementary cells are shown as ≥3 to prevent exact reconstruction. Percentages are withheld for any variable carrying a suppressed count.")
  if (id %in% c("UW003","UW004")) {
    note <- if(id=="UW003") "SMD compares Primary vs Secondary only (the first two of 3 groups)." else "SMD compares 東 vs B only (the first two of 3 groups)."
    expect_identical(tt$footnote,note)
    expect_identical(tt$meta$console_before,paste("Note:",note))
    expect_identical(native_console[startsWith(native_console,"Note:")],paste("Note:",note))
  }
  st <- helpers$golden_sheet_layout(files$xlsx,id)
  layout <- getFromNamespace(if(startsWith(id,"US")) ".xlsx_layout_puttab" else ".xlsx_layout_table1","tabtools")(tt)
  state <- getFromNamespace(".xlsx_apply_rules","tabtools")(layout$rules,nrow(layout$grid),ncol(layout$grid),tt$style)
  if(startsWith(id,"US")) {
    expect_identical(st$merges,if(id=="US003")c("A1:E1","C2:D2")else"A1:E1")
    expect_equal(unname(state$widths["2"]),
      if(id=="US001")41 else 50,tolerance=0)
  } else {
    pos<-which(tt$cols$role=="smd")+1L
    expect_equal(unname(state$widths[as.character(pos)]),
      c(UW001=10,UW002=8,UW003=25,UW004=14)[[id]],tolerance=0)
    expect_true(file.exists(file.path(native,paste0(id,".frame.dta"))))
  }
}
for(id in c("US001","US002","US003","UW001","UW002","UW003","UW004")) local({
  case <- id
  test_that(paste(case,"exact new-pin publication and all sinks"),run_layout_case(case))
})
