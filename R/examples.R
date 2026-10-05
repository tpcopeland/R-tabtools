# Examples for regtab() and as_forest_data(), whose roxygen blocks live in
# R/regtab.R. They are kept in this file while milestone 5w edits
# R/regtab.R on its own branch; fold them into that file's blocks after the
# merge.

#' @name regtab
#' @rdname regtab
#' @references
#' Fine JP, Gray RJ (1999). A proportional hazards model for the
#' subdistribution of a competing risk. *Journal of the American Statistical
#' Association* 94(446), 496-509. \doi{10.1080/01621459.1999.10474144}
#' (Subdistribution hazard ratios from `survival::finegray()` fits.)
#'
#' StataCorp (2021). *Stata 17 User's Guide*, section 20.22, "Obtaining
#' robust variance estimates", and *Stata 17 Base Reference Manual*,
#' `[R] vce_option`. College Station, TX: Stata Press. (The `vce = "stata"`
#' conventions: the model-based variance with Stata's small-sample and
#' degrees-of-freedom choices per command.)
#' @examples
#' fit1 <- glm(breaks ~ wool, family = poisson, data = warpbreaks)
#' fit2 <- glm(breaks ~ wool + tension, family = poisson, data = warpbreaks)
#' regtab(fit1, fit2, models = c("Crude", "Adjusted"), stats = c("n", "aic"))
#'
#' # A linear model with an interaction, written to an Excel sheet: the
#' # table is then returned invisibly, so assign it and print it
#' xlsx <- tempfile(fileext = ".xlsx")
#' tab <- regtab(lm(breaks ~ wool * tension, data = warpbreaks), stars = TRUE,
#'               xlsx = xlsx, sheet = "Linear")
#' tab
#' # A methods sentence for the paper
#' tab$stored$methods
NULL

#' @name as_forest_data
#' @rdname as_forest_data
#' @examples
#' fit <- glm(breaks ~ wool + tension, family = poisson, data = warpbreaks)
#' as_forest_data(regtab(fit))
NULL
