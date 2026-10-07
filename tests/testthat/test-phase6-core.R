test_that("new summary frames preserve header and sample identity through plain export", {
  d <- data.frame(x = 1:4, y = c(1,2,4,3), exit = 1:4, event = c(1,1,0,0), group = c(1,2,1,2))
  tables <- list(crosstab(d,"event","group"), corrtab(d, c("x","y"), pvalues = TRUE),
                 survtab(d,"exit","event",c(1,2),rmst = 2))
  for (tt in tables) {
    before <- serialize(tt,NULL)
    flat <- tt_flat(tt,keyed = FALSE)
    expect_identical(attr(flat,"header",exact=TRUE),tt$header)
    expect_identical(attr(flat,"frame",exact=TRUE),tt$meta$frame)
    expect_identical(attr(flat,"sample_accounting",exact=TRUE),tt$meta$sample_accounting)
    expect_identical(attr(flat,"command",exact=TRUE),tt$command)
    expect_false(any(startsWith(names(flat),"_state_")))
    expect_error(tt_flat(tt,keyed=TRUE),class="tabtools_error_flat")
    expect_error(tt_merge(tt,tt),class="tabtools_error_composition")
    expect_error(tt_stack(tt,tt),class="tabtools_error_composition")
    publication <- puttab(unserialize(serialize(flat,NULL)))
    expect_identical(unname(as.matrix(publication$body)),unname(as.matrix(tt$body)))
    expect_identical(serialize(tt,NULL),before)
  }
})

test_that("one-row summary headers remain aligned in workbook and presentation dispatch", {
  d <- data.frame(x=1:4,y=c(1,2,4,3),exit=1:4,event=c(1,1,0,0),group=c(1,2,1,2))
  tables <- list(crosstab(d,"event","group"), corrtab(d,c("x","y"),pvalues=TRUE,footnote="Boundary note."),
    survtab(d,"exit","event",c(1,2),rmst=2,footnote="Boundary note."))
  for (tt in tables) {
    spec <- .tt_render_spec(tt)
    rendered_body <- spec$body$text
    rendered_body[] <- gsub("\u00a0", " ", rendered_body, fixed = TRUE)
    expect_identical(unname(rendered_body),unname(as.matrix(tt$body)))
    book <- withr::local_tempfile(fileext=".xlsx")
    tt_write_xlsx(tt,book,sheet="Contract")
    cells <- openxlsx2::wb_to_df(openxlsx2::wb_load(book),sheet="Contract",col_names=FALSE,
      rows=2L:(nrow(tt$body)+2L),cols=2L:(ncol(tt$body)+1L))
    cells[is.na(cells)] <- ""
    expect_identical(unname(as.matrix(cells[1L,,drop=FALSE])),
      matrix(tt$header[[1L]]$text,nrow=1L))
    expect_identical(unname(as.matrix(cells[-1L,,drop=FALSE])),unname(as.matrix(tt$body)))
  }
})
