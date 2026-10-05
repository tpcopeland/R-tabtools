#' Regression results table
#'
#' R implementation of the Stata `regtab` command. Where Stata reads the active
#' `collect`, this function takes one or more fitted models and renders them
#' side by side: one block of estimate, confidence-interval, and p-value columns
#' per model, variable-label header rows for factors, indented levels, and an
#' italic reference row for each base category.
#'
#' Pass fitted models (`regtab(fit1, fit2)`, or a named list to label
#' them); the result is a `tt_table` that prints, and `xlsx`, `csv` and
#' `markdown` write it. The effect scale (OR, HR, IRR, Coef.) is detected per
#' model unless `coef` is given, and ratio-scale estimates are
#' exponentiated. Argument names mirror the Stata options.
#'
#' Common tasks, and the sections below that cover them:
#' * Which models work: "Supported models"; ordinal, multinomial, count,
#'   survival and competing-risks models: "Ordinal, multinomial, count, and
#'   survival models"; `lme4`, `glmmTMB`, `nlme` and `geepack` fits: "Mixed
#'   models and GEE"; any other model, as a data frame of its estimates:
#'   "Data-frame input".
#' * Choose rows (`keep`, `drop`): "Rows and keys"; interaction labels:
#'   "Interactions".
#' * Add N, events, AIC and other statistics (`stats`): "Model statistics".
#' * Robust, clustered or user-supplied standard errors, survey and IPTW
#'   weights, marginal structural models (`vce`, `cluster`): "Standard
#'   errors and intervals" and "Weights, robust variance, IPTW and marginal
#'   structural models", and `vignette("weighted-analyses")`.
#' * Multiply imputed data: "Multiple imputation".
#' * Univariable (crude) models in one column: [regtab_uv()]; whole tables
#'   side by side or one under another: [tt_merge()], [tt_stack()].
#'
#' `vignette("regression-tables")` works through these with examples.
#'
#' @section Supported models:
#' `lm`, `glm` (including `MASS::glm.nb`), and `survival::coxph`;
#' `MASS::polr`, `ordinal::clm`, `nnet::multinom`, `pscl::zeroinfl`,
#' `pscl::hurdle`, `survival::survreg` (and `AER::tobit`), Fine-Gray
#' `coxph` on `survival::finegray()` data, `cmprsk::crr`, and data frames;
#' `lme4::lmer`, `lme4::glmer`, `glmmTMB::glmmTMB`, `nlme::lme`, and
#' `geepack::geeglm` (see the sections on ordinal, multinomial, count and survival
#' models, on mixed models and GEE, and on data-frame input);
#' `survey::svyglm` and `WeightIt::glm_weightit` (see the
#' section on weights). The gate is
#' an allow-list on the fit's first class: any other class, including a
#' subclass of a supported one (`mgcv::gam`,
#' `survey::svycoxph`, a `coxph` with a `frailty()` term), stops with an
#' error naming it; pass such a model as a data frame. An `lmerTest` fit
#' (class `lmerModLmerTest`) is converted with `as(fit, "lmerMod")`, with a
#' note: regtab reports Stata `mixed`'s z statistics, not lmerTest's
#' Satterthwaite t tests. Multiply imputed fits (`mice`'s `mira`, or
#' [tt_mi()]) are pooled (see the section on multiple imputation); a pooled
#' `mice` result (`mipo`) is refused with a hint to pass the `mira`. To
#' show the imputations side by side instead, pass `fit$analyses` as a
#' list. A list passed to `regtab()` must hold only fitted models (or data
#' frames): any other element is an error naming it.
#'
#' Limitations:
#' * Collinear terms: R's `lm()`/`glm()` alias the later of linearly
#'   dependent columns, while Stata may omit another (`regress y x1 x2 x3`
#'   with `x3 = 2 * x2` omits `x2` in Stata and `x3` in R; `mi estimate`
#'   likewise). The fitted model is the same; the Omitted row and the
#'   coefficient shown on the other row differ. Drop the redundant term
#'   before fitting for Stata's layout.
#' * Multi-equation interaction rows follow Stata's colnames for two-way
#'   terms (`1.female#index_age`, `treatment#age_z`, with Reference rows)
#'   or, with `interactions = "fvgen"`, fvgen's labels (`part1 x part2`);
#'   higher-order terms render natively, untested against Stata.
#' * `pscl::hurdle` has no Stata counterpart (it is shown in `churdle`'s
#'   layout): its standard errors are the observed information from the
#'   analytic score, checked against the logit GLM (zero part) and pscl's
#'   own `vcov()`, not against Stata.
#' * `glmer` families with an estimated scale (Gamma, or gaussian with a
#'   non-identity link) keep lme4's `vcov()` for the fixed effects and get
#'   no random-effects intervals (with a warning on every call, recorded
#'   in `stored$vce_fallback`): lme4 holds their scale outside the deviance
#'   function regtab differentiates. A `glmmTMB` fit whose full variance
#'   cannot be formed likewise warns, and its random-effects rows have no
#'   interval.
#' * Mixed-effects Cox models (`coxme::coxme`) have no Stata equivalent
#'   and are not supported: Stata's `mestreg` is a parametric survival
#'   model, and `stcox, shared()` is a gamma frailty model.
#'
#' @section Rows and keys:
#' Rows follow the formula as written, the union of all models' rows in
#' order of first appearance, with the intercept last. Each row has a
#' Stata-style key used by `keep`/`drop`: `age` (a numeric term), `education`
#' (a factor's header row), `2.education` (a level), `_cons` (intercept),
#' and `1.foreign#3.rep78` (an interaction cell). A bare name selects the
#' variable's rows and every interaction containing it; `i.`/`c.` prefixes
#' are ignored; R coefficient names (`educationSecondary`) also work. A
#' `##` term (`keep = "i.foreign##i.rep78"`) matches nothing and errors,
#' where Stata keeps only the parent row. Keys are Stata-style in fvgen mode
#' too (`1.foreign#2.pclass`, `keep = "foreign"`), not fvgen's generated
#' variable names. A factor made in the formula keeps the formula's text as
#' its name: with `factor(cyl)` the keys are `factor(cyl)` and
#' `6.factor(cyl)`, so `keep = "cyl"` matches nothing (and errors); convert
#' the column in the data instead, or write `keep = "factor(cyl)"`.
#'
#' A level's code is the Stata value code when the factor keeps a
#' `"labels"` attribute, as [tt_as_factor()] does for `haven::read_dta()`
#' data; else the level itself when it is an integer; `0`/`1` for a logical
#' (shown as an unlabelled 0/1 variable); a character predictor's sorted
#' position (Stata's `encode`); otherwise the level's position (1, 2, ...).
#' `haven::as_factor()` drops the codes, so its 0/1 `foreign` is keyed
#' `1.foreign`/`2.foreign`: regtab prints a note when native interaction
#' keys or `keep`/`drop` level terms rely on such positional codes. Rows
#' of several models are joined as Stata joins them, by level: Stata by
#' value code, regtab by the code when there is one and else by the level
#' itself. So a factor relevelled in one model (each model's Reference on
#' its own level's row), a subset model that lacks a level (its row empty
#' there) and a character predictor missing a value in one model line up;
#' where the models number a variable's levels differently, its keys
#' follow one numbering over all models (model 1's levels in order, a
#' level only a later model has placed after its predecessor; a character
#' predictor's sorted values, as Stata's `encode`), and the rows follow
#' it. A variable whose levels still cannot be matched (a factor mixing
#' numeric and text levels) is refused, never shown with one model's
#' estimate under another level's label.
#' Character predictors take R's level order at fit time, which depends on
#' the locale's collation; Stata's `encode` sorts bytewise (the C locale).
#'
#' Factors must use treatment-type contrasts (`contr.treatment` with any
#' base, or `contr.SAS`, Stata's `ib(last).`), since a Reference row claims a
#' contrast against that level: ordered factors (polynomial contrasts),
#' `contr.sum`, `contr.helmert` and custom contrasts are refused. A term
#' with several columns (a matrix, `poly()`, splines) gets one row per
#' column, keyed and labelled by its coefficient name.
#'
#' Variable labels come from the `"label"` attribute of the model frame
#' columns; for a fit with `subset =` they are read from the data at
#' `regtab()` time, with a warning if the data no longer match the fit.
#'
#' @section Interactions:
#' By default (`interactions = "fvgen"`) the table matches Stata's `fvgen` +
#' `regtab` workflow, with factor main effects of interacted variables as
#' flat rows labelled by level (optionally `vsref = "(vs. @)"`) and one row
#' per interaction cell labelled `part1 x part2` (parts joined by `xsymbol`
#' with a space on each side); `I(x^2)` reads `<label>` with a superscript
#' two. As in fvgen, the factor comes first whatever the formula's order
#' (`mpg * foreign` reads `Foreign x Mileage`), generated labels are cut to
#' 80 characters, and only two-way terms are supported (`f * I(x^2)` is
#' three-way). `interactions = "native"` reproduces native `regtab`: a
#' `foreign#rep78` parent row and raw `1.foreign#3.rep78` rows, with base
#' cells `refcat`, empty cells `emptylabel`, and aliased cells `omitlabel`.
#'
#' @section Model statistics:
#' `stats` adds rows below the estimates, in Stata tabtools 2.1.12's fixed
#' order:
#' * `n`: Observations, or Subjects when a model has them (survival
#'   models; `%12.0fc`);
#' * `events`: Events, the number of failures (Stata's e(N_fail)) of a
#'   `coxph` (`stcox`: weighted by the case weights, as after `stset
#'   [pweight]`), Fine-Gray (`stcrreg`: the failures of interest) or
#'   `survreg` fit (`streg`: weighted by its case weights); blank for
#'   other models, and for `cmprsk::crr`, which keeps no failure count.
#'   A weighted fit's failures are counted on its observations (for a
#'   `tt()` model, not the records survival expands it into), from the fit
#'   or from its data re-sorted with their row names reset; when neither
#'   pairs its statuses with its weights (`y = FALSE` with data that
#'   cannot be checked) the cell is blank, with a note;
#' * `groups`: Groups (mixed models);
#' * `mi_m`: Imputations, the number of imputations of a multiply imputed
#'   model (Stata's e(M_mi));
#' * `aic`, `qic`, `bic`, `ll`: AIC (QICu for a GEE model), QICu, BIC,
#'   Log-likelihood (`%12.2f`);
#' * `icc`: ICC (mixed models);
#' * `r2`: R-squared, the pseudo R-squared where there is no R-squared,
#'   else the adjusted R-squared (`%5.3f`);
#' * `r2_a`: Adjusted R-squared (`lm`, Stata `regress`; `%5.3f`);
#' * `rmse`: Root MSE (`lm`), the square root of the (weighted) residual sum
#'   of squares over N - k, the weights rescaled to mean 1 as `regress`
#'   rescales `[aweight]`s (`%9.3f`);
#' * `F`: F statistic (`%9.2f`), shown only for linear regression (`lm`,
#'   and a Gaussian identity-link `survey::svyglm`, Stata's `svy:
#'   regress`), never for `glm` or other models: the classical F of
#'   `regress`, the Wald F with the robust or cluster-robust variance under
#'   `vce = "robust"`/`"cluster"` (blank, as in Stata, when there are fewer
#'   clusters than coefficients), the design-based adjusted Wald F of `svy:
#'   regress`; a constant-only model shows 0, Stata's e(F);
#' * `fmi`: Largest FMI, the largest fraction of missing information over
#'   the coefficients of a multiply imputed model (`%6.4f`);
#' * `vce` (R only; Stata regtab has no such token): Standard errors, each
#'   model's variance as text, in the words of the `vce_note` footnote
#'   ("robust", "robust, clustered by id", "user-supplied", "survey design
#'   (linearized)", ...), and "model-based" for the command's model-based
#'   default; under `vce = "model"` it names the fit's own variance (a
#'   robust `coxph`/`survreg` keeps survival's sandwich, a `geeglm`
#'   geepack's sandwich or jackknife). Blank for a data frame, whose
#'   standard errors regtab cannot know.
#'
#' The full-precision values are stored as `n_1`, `events_1`, `aic_1`,
#' ..., `F_1`, `fmi_1` (one per model), as Stata's `r()` (`vce` stores
#' nothing). `stat_fun` adds R-only rows below them.
#'
#' @section Standard errors and intervals:
#' Confidence intervals are Wald intervals by default, on the same
#' reference distribution as the reported p-value, and match what the
#' Stata command reports (`vce = "stata"`):
#' * `lm` (Stata `regress`): `vcov()`, Student t with the residual degrees
#'   of freedom.
#' * `glm`, every family and link (Stata `logit`, `probit`, `cloglog`,
#'   `poisson`, `glm`): the normal distribution, also for families with an
#'   estimated dispersion (gaussian, Gamma, quasi), because Stata's `glm`
#'   reports z statistics. Standard errors come from the observed
#'   information at the final estimates, Stata's default `vce(oim)`,
#'   multiplied by the Pearson dispersion where it is estimated (Stata's
#'   `scale(x2)`). For canonical links (logit, log-Poisson, identity) this is
#'   the Fisher information at the final estimates; R's `vcov()` evaluates it
#'   at the previous IRLS iterate, which moves standard errors in the 7th
#'   digit, and for other links (probit, cloglog, Gamma-log) it uses the
#'   expected information, which can move the CI and p cells. The header
#'   follows Stata's family/link rule: odds ratios for binomial logit,
#'   incidence rate ratios for Poisson log, `Coef.` otherwise, so a
#'   log-binomial fit (`binomial(link = "log")`, Stata's `glm, family(binomial)
#'   link(log)` without `eform`) shows log risk ratios under `Coef.`; its
#'   methods sentence names "log-binomial regression". A `glm` with the
#'   `MASS::negative.binomial(theta)` family (a known theta, Stata's `glm,
#'   family(nbinomial k)` with k = 1/theta) has scale 1, as in Stata, not
#'   the Pearson dispersion `summary()` uses; its weights are analytic
#'   weights (`glm [aweight]`, which Stata allows for this family).
#'   `ci_method = "profile"` is refused for it: R's profile of this family
#'   uses the Pearson dispersion.
#' * `MASS::glm.nb` (Stata `nbreg`): the coefficient block of the inverse
#'   joint observed information over the coefficients and ln(alpha). With
#'   a link other than log (`link = identity`, `sqrt`) it is Stata's `glm,
#'   family(nbinomial ml) link()`: no ln(alpha) row, and the variance of the
#'   coefficients at a fixed alpha. Stata takes that alpha from the log-link
#'   `nbreg`, while `glm.nb()` estimates theta jointly with the other link's
#'   coefficients, so estimates and standard errors differ from Stata's:
#'   about 1e-5 (relative) unweighted on the probe data, and more with
#'   weights (3% on a standard error on `auto` with `[iw=turn]`, identity
#'   link).
#' * `survival::coxph` (Stata `stcox`): `vcov()`, normal. A robust
#'   (sandwich) fit, from `cluster()`, `robust = TRUE`, or an `id` with
#'   overlapping records, is multiplied by Stata's small-sample factor
#'   M/(M - 1), M the number of clusters (every observation without
#'   `cluster()`), as `stcox, vce(cluster)`/`vce(robust)` report it;
#'   likewise a robust `survival::survreg` fit (`streg, vce(robust)`).
#'   `vce = "model"` keeps survival's sandwich without the factor.
#'   survival makes a fit robust by itself only for non-integer weights,
#'   `cluster()` or `robust = TRUE`: integer (for instance rounded) weights
#'   and `robust = FALSE` give the model-based variance, with a warning.
#'   **Ties:** `coxph()` defaults to Efron's approximation and `stcox` to
#'   Breslow's, so on tied failure times the two differ (a one-time note
#'   says so); fit with `ties = "breslow"` for Stata's numbers. `stcox`
#'   refuses weights with `efron`, so a weighted Efron fit on tied failure
#'   times has no Stata analogue: its log-likelihood, AIC and BIC are blank,
#'   with a note. Without ties Efron's likelihood is Breslow's, so neither
#'   note appears and the statistics are shown.
#' * `survival::clogit` (Stata `clogit`): odds ratios, `vcov()`, normal;
#'   the `strata()` term gives no row. Only `method = "exact"` (the default,
#'   Stata's exact conditional likelihood, which survival fits unweighted);
#'   `vce = "stata"` or `"model"`. Observations counts the rows; `events` and
#'   `groups` add no row (Stata's clogit stores neither). As in Stata
#'   tabtools 2.1.14, which classifies `clogit` like `logit` (odds ratios
#'   whether or not it was fitted with `or`), the header is OR, `dimnonsig`
#'   judges against 1, the methods sentence says "conditional logistic
#'   regression", and a `logistic` model beside it loses its intercept row
#'   under the automatic `nointercept`.
#'
#' `vce = "model"` uses each fit's own `vcov()` and the distribution R's
#' `summary()` uses (t for `glm` families with an estimated dispersion).
#' A `glm` whose `converged` flag is `FALSE` is refused with
#' `tabtools_error_model_convergence`; refit before reporting its inference.
#' Explicit failed optimizer diagnostics from mixed, count, multinomial,
#' ordinal and GEE fits are refused with the same class. Mixed-model Hessian
#' failures use `tabtools_error_model_hessian`; unusable active `clm`
#' covariance uses `tabtools_error_model_covariance`. A singular random-effect
#' boundary or a gradient/conditioning advisory alone does not establish
#' failure when the estimated fixed-effect covariance remains usable.
#' Cox and `survreg` objects do not retain a complete convergence flag.
#' Right-censored Breslow/Efron Cox fits are refused when the stored iteration
#' count exceeds a literal or default iteration limit; mutable control objects
#' are not re-evaluated. Explicit limits must be literal control arguments
#' in the fitted call or in a qualified `survival::coxph.control()` call.
#' `survreg` fits whose likelihood is materially below
#' their nested intercept-only fit are refused. Other ambiguous survival
#' diagnostics remain the caller's responsibility to check at fit time.
#' An implicit-wave unstructured GEE working correlation that is singular
#' or indefinite is refused; explicit-wave or custom correlation designs
#' cannot be reconstructed from these stored diagnostics alone.
#' Robust or cluster-robust `lm` variances require more positive-weight
#' observations than the model rank. An ordinary robust `glm` variance
#' requires at least two positive-weight observations; these failures use
#' `tabtools_error_vce_sample_size`.
#'
#' `ci_method = "profile"` replaces the Wald bounds with profile-likelihood
#' intervals (`confint()`), for `lm`, `glm` and `MASS::glm.nb` fits only,
#' and only with `vce = "stata"` or `"model"`: they come from the model's
#' likelihood, so beside a robust, clustered or design-based variance they
#' would not match the p-values, and they are refused (as are the other
#' classes, whose rows are always Wald; [tt_ci_methods()] lists what a fit
#' supports). Weighted fits are profiled under the reading their Wald
#' interval uses: with `vce = "stata"`, a weighted binomial (0/1 response)
#' or Poisson `glm` is refitted with its weights rescaled to mean 1 (Stata's
#' `glm [aweight]` likelihood; an error if the refit does not reproduce the
#' fit), and `vce = "model"` profiles the fit as it is (`confint()`).
#' Non-integer, non-constant (inverse-probability) weights are profiled
#' too, with the warning that `vce = "robust"` is Stata's `[pweight]`
#' reading (under `vce = "model"` as well, which profiles them as frequency
#' weights). The methods sentence then names the intervals "profile-likelihood" and
#' the p-values, which stay Wald, "from Wald tests". A profile that fails is an error, never a silent Wald
#' interval, and `stored$ci_method` records the method used. Stata's
#' intervals are Wald intervals.
#' Subclasses of `lm`/`glm` other than `survey::svyglm` and
#' `WeightIt::glm_weightit` (`mgcv::gam`, ...) carry their own variance and
#' degrees of freedom, which have not been verified against a Stata
#' command, and are refused. `vce = "robust"` and `"cluster"` are described
#' in the next section; [tt_vcov()] returns the matrix behind any of them.
#'
#' @section Weights, robust variance, IPTW and marginal structural models:
#' Stata reports robust standard errors whenever a model has probability
#' weights (`logit y x [pw=w]`) or `vce(robust)`/`vce(cluster id)`; R's
#' `glm(weights = w)` does not know which kind of weight `w` is. So `vce`
#' says it, per model:
#' * `vce = "robust"`: `glm` (logit, probit, poisson, glm) HC0 times
#'   N/(N - 1), z; `lm` (regress) HC1, t(N - k); `coxph` the dfbeta
#'   sandwich (clusters from the fit's `cluster()`/`id`, else one per
#'   row) times G/(G - 1), as `stcox` reports after `stset ... [pw=w],
#'   id()`. This is Stata's `[pweight]` (or `vce(robust)`) variance.
#' * `vce = "cluster"` with `cluster = ~id` (a formula, a column name, or a
#'   vector): `glm` and `coxph` times G/(G - 1), z; `lm` times
#'   G/(G - 1) (N - 1)/(N - k), t(G - 1): Stata's `vce(cluster id)`, the
#'   variance of a pooled-logistic marginal structural model on
#'   person-period data (`logit y treat ... [pw=sw], vce(cluster id)`).
#'   Giving `cluster` without `vce` means `vce = "cluster"`.
#' * Either argument may be a list with one entry per model, so crude,
#'   adjusted, IPTW and MSM columns can differ:
#'   `regtab(crude, adj, iptw, msm, vce = list("stata", "stata", "robust",
#'   "cluster"), cluster = list(NULL, NULL, NULL, ~id))`. A single
#'   `cluster` applies to the models whose `vce` is `"cluster"`.
#' * `MASS::glm.nb` (nbreg) supports both: the sandwich over the
#'   coefficients and ln(alpha) jointly, times N/(N - 1) (or G/(G - 1)), as
#'   `nbreg [pweight]` reports it, the `lnalpha` row included.
#' * Classes that support neither (`polr`, `clm`, `multinom`, `zeroinfl`,
#'   `hurdle`, mixed models, ...) refuse `"robust"`/`"cluster"` with an
#'   error naming the class ([tt_vce_types()]); robust `survreg` fits get
#'   Stata's G/(G - 1) under the default `vce = "stata"`.
#' * `cluster` given while no model has `vce = "cluster"` is an error,
#'   never ignored.
#'
#' Weighted fits never get model-based standard errors silently, since
#' Stata's `[pweight]` always reports robust ones:
#' * Under the default `vce = "stata"`, `glm`/`lm` weights are analytic
#'   weights (Stata `[aweight]`, rescaled to mean 1), with model-based
#'   standard errors; `glm.nb` weights are those of `nbreg [iweight]`,
#'   since `nbreg` takes no `[aweight]`. When they are non-integer (any of
#'   them) and non-constant, as inverse-probability weights are, regtab
#'   warns that `vce = "robust"` is Stata's `[pweight]` reading, but never
#'   switches by itself. Integer
#'   inverse-probability weights cannot be told from frequency weights and
#'   are not flagged.
#' * A weighted `geeglm` is `xtgee [pweight]`, which forces `vce(robust)`:
#'   its default variance is the robust one.
#' * A weighted `coxph` with survival's model-based variance (integer
#'   weights, `robust = FALSE`) warns, as do weighted `polr`, `clm`,
#'   `multinom`, `zeroinfl`, `hurdle`, `survreg` and mixed-model fits with
#'   non-integer weights: their weights act as frequency weights, and
#'   regtab has no robust variance for them (refit `survreg` with
#'   `robust = TRUE`).
#'
#' Under probability weights (a robust or cluster `vce` on a weighted
#' `lm`/`glm`/`glm.nb`, a robust weighted `coxph`, `geeglm` with
#' `gee_as = "glm"`) Stata reports a pseudo-log-likelihood, and regtab
#' shows it with the AIC and BIC computed from it, as Stata's regtab does:
#' the weighted log-likelihood of `logit`, `probit`, `cloglog`, `poisson`,
#' `nbreg` and `glm`'s Gamma and inverse Gaussian families (scale 1) and
#' Gaussian family (scale sum(w e^2)/N); `regress`'s `[aweight]` log-likelihood (weights
#' normalised to mean 1); and `stcox`'s Breslow partial likelihood with the
#' weights normalised to mean 1. The pseudo R-squared of a logit or probit
#' fit is against the weighted constant-only model (`logit`/`probit`
#' e(r2_p); none with an offset, and none for other links, as `cloglog`);
#' `poisson [pweight]` and Stata's `glm` command store none. A binomial
#' with trials, whose Stata `[pweight]` log-likelihood has not been
#' checked, leaves the cells blank, with a note.
#' `svyglm` and `glm_weightit` report no likelihood (nor does Stata's
#' `svy:` or `teffects`). Observations with a zero weight are not counted,
#' as Stata drops them.
#'
#' **AIC and BIC under few clusters.** After `vce(cluster)`, Stata's
#' e(rank) is the rank of the cluster-robust variance matrix, at most
#' G - 1. With fewer clusters than coefficients, Stata tabtools 2.1.12 then
#' counts the coefficients with a nonzero standard error, as
#' regtab counts the estimated parameters whatever the variance (2.1.11
#' used e(rank), `logit y x1 x2 x3, vce(cluster c)` with 3 clusters giving
#' AIC -2 ll + 4).
#'
#' **Data changed after fitting.** A robust or cluster-robust variance is
#' computed from the rows the model was fitted on (see [tt_vcov()], "Rows
#' the variance is computed from"): if a `coxph`, `survreg` or
#' `model = FALSE` fit's data frame is re-sorted, filtered or edited after
#' fitting, a variance that needs those rows is an error (refit, or fit
#' with `model = TRUE`), never a silently different number. The table as a
#' whole follows the same rule: a fit that keeps no copy of its data
#' (`coxph`, `survreg`, `lm`/`glm.nb`/`polr` with `model = FALSE`, and
#' `nnet::multinom`, whose default is `model = FALSE`) is refused when its
#' data no longer reproduce it (removed, filtered or edited, or a fit read
#' from a file in a session without its data), since its labels, reference
#' rows, N and log-likelihood would come from other data than its
#' estimates; data re-sorted with their row names kept, or holding the same
#' observations in another order, are used. Storage options are either
#' irrelevant or refused by name: `lm(qr = FALSE)`, `ordinal::clm(model =
#' FALSE)` and `pscl::zeroinfl`/`hurdle(model = FALSE)` are refused;
#' `glm(y = FALSE)`, `glm.nb(y = FALSE)`, `coxph(y = FALSE)`, `x = TRUE`,
#' `Hess =` and the others give the default fit's table. A fit read from a
#' file in a fresh session loads its class's package (`survival`, `nnet`,
#' ...) itself. A `coxph` fit with `tt()` terms, which survival does not
#' let keep its data, is checked on its observations, events and event
#' times, and gets the variances survival stores (`vce = "stata"` or
#' `"model"`, and the sandwich of a fit with `cluster =` or
#' `robust = TRUE`); a variance that needs its rows is refused.
#'
#' * `survey::svyglm` (Stata `svy:`): the design-based `vcov()`, t with the
#'   design degrees of freedom `survey::degf()` (`svyset _n [pw=w]`: N - 1;
#'   `svyset id [pw=w]`: clusters - 1), odds ratios for a
#'   (quasi)binomial logit fit and IRRs for a (quasi)poisson log fit;
#'   Observations is the unweighted number of rows, and a design from
#'   `subset()` (Stata's `svy, subpop()`) shows the subpopulation as
#'   Subjects, as Stata's e(N_sub) (a design subset with `design[rows, ]`
#'   is not recognised as a subpopulation and shows Observations). A
#'   Gaussian identity fit shows the
#'   weighted R-squared (`svy: regress` e(r2)). Use `glm()` with
#'   `vce = "robust"` for `logit [pw]`'s z statistics. As in Stata
#'   tabtools 2.1.11 and later, a `svy:` fit is classified like its plain command:
#'   `svy: logit` shows odds ratios with the intercept dropped, `svy:
#'   poisson` incidence rate ratios (tabtools 2.1.9 left `svy:` fits
#'   unclassified).
#' * `WeightIt::glm_weightit`: its own M-estimation variance, which
#'   accounts for the estimation of the weights (for an identity-link
#'   `y ~ treat` fit, the ATE and standard error of Stata's
#'   `teffects ipw`); z statistics. `vce = "stata"` and `"model"` both
#'   use it.
#' * `geepack::geeglm` with `gee_as = "glm"` and an independence working
#'   correlation: Stata's `glm y x [pw=w], family() link() vce(cluster id)`
#'   (odds ratios for a binomial logit fit, IRRs for a Poisson log fit, the
#'   GEE sandwich times G/(G - 1)) instead of the default `xtgee` reading.
#' * `vce_note = TRUE` (R only; off by default) appends a sentence naming
#'   each model's non-default variance to the footnote, including the fits
#'   that are robust without `vce = "robust"`: a weighted or clustered
#'   `coxph`, a robust `survreg`, `crr`, a weighted `geeglm`.
#' * A weighted `coxph` reports Stata's weighted subject count (`e(N_sub)`
#'   after `stset [pw=w], id()` is the sum of the subjects' weights, each
#'   subject once) in the Subjects row, with a note saying it is a sum of
#'   weights. The subjects are the fit's `id`, else its `cluster()` (or
#'   `cluster =`) variable. When the weights vary within a subject (which
#'   `stset` refuses) the cell is blank, with a note. `coxph()` stops iterating when the relative change in
#'   the log partial likelihood is below `eps` (`1e-9`), which on a flat
#'   likelihood can leave a hazard ratio ~4e-9 from `stcox`'s; refit with
#'   `control = coxph.control(eps = 1e-10)` when a last digit differs.
#'
#' After a weighted fit, `marginaleffects` reproduces Stata's `margins` with
#' `vcov = tt_vcov(fit, "robust")` and `wts = w` (margins averages with the
#' estimation weights).
#'
#' **User-supplied variance (R only).** For an `lm`, `glm` or
#' `survival::coxph`/`clogit` fit, a `vce` entry may be a function
#' `function(fit)` returning the covariance matrix of the coefficients, or
#' the matrix itself: `regtab(fit, vce = function(f) sandwich::vcovHC(f,
#' "HC3"))`, or `vce = list("stata", V)` for two models. The matrix is
#' checked (square, symmetric, finite, named by the coefficients or in
#' their order; aliased coefficients may be left out) and used for every
#' standard error, interval, p-value and the `F` token; the estimates and
#' the other statistics are the fit's. The reference distribution is
#' `vce_df`: by default t with the residual degrees of freedom for `lm` and
#' the normal otherwise. A footnote says the standard errors are
#' user-supplied (every sink), whatever `vce_note`, and `stats = "vce"`
#' adds a row saying so for each model. None of Stata's variance
#' names is ever mapped to such a variance, and it cannot be combined with
#' `cluster`, `ci_method = "profile"`, a [tt_mi()] model or a data frame.
#'
#' R's `glm()` stops iterating when the relative change in deviance is
#' below `epsilon` (default `1e-8`), which leaves non-canonical fits
#' (probit, cloglog, Gamma-log) up to ~1e-4 (relative) from the maximum
#' Stata reaches; refit with `control = glm.control(epsilon = 1e-12)` when
#' a cell differs from Stata in the last digit. Likewise, R does not drop
#' observations a covariate predicts perfectly, as Stata's `logit` does:
#' such coefficients (standard error above 100 on the link scale of a
#' binomial or Poisson model) raise a warning and print an enormous ratio
#' with a blank confidence interval. `glm(weights =)` are treated as
#' analytic (or binomial trials) weights: the `Observations` row counts rows
#' with a positive weight, not their sum as Stata's `[fweight]` would, and
#' the standard errors, log-likelihood, AIC and BIC are those of Stata's
#' `glm [aweight]` (weights rescaled to mean 1; `vcov()` and `logLik()` of
#' a binomial or Poisson fit use them as they are, Stata's `[iweight]`).
#' A binomial `cbind(successes, failures)` response, or a proportion
#' response with a value strictly between 0 and 1, has trials, not
#' analytic weights (Stata's `glm, family(binomial m)`); a proportion
#' response with `weights =` whose values are all 0 or 1 cannot be told
#' from 0/1 data with analytic weights, and is read as the latter. The
#' log-likelihood of a Gamma or inverse Gaussian `glm` is Stata's, at scale
#' 1 (the estimated dispersion enters the standard errors only), where
#' `logLik()` plugs in deviance/n; quasi families have none.
#' Weighted `lm` fits report the log-likelihood, AIC and BIC of Stata's
#' `regress [aweight]` (weights rescaled to mean 1). The weights of
#' `MASS::polr`, `ordinal::clm`, `nnet::multinom`, `pscl::zeroinfl`,
#' `pscl::hurdle` and `survival::survreg` fits act as case (frequency)
#' weights: their standard errors are those of the Stata command with
#' `[fweight]`, and `Observations` (and BIC) use the sum of the weights, as
#' Stata's e(N) does (`survreg()` refuses zero weights: drop those rows with
#' `subset = w > 0`).
#'
#' @section Mixed models and GEE:
#' `lme4::lmer()` fits correspond to Stata `mixed` (fit with `REML = FALSE`
#' for Stata's default ML), `lme4::glmer()` fits to `melogit`,
#' `mepoisson`, `meprobit`, `mecloglog`, `menbreg` or `meglm` by family and
#' link, and `glmmTMB::glmmTMB()` fits likewise. Fixed effects are shown as
#' for fixed-effect models (odds ratios for a logit link, and so on).
#' Random-effects parameters follow every fixed-effect row, outermost
#' grouping level first, then the residual variance, with a rule above the
#' first: `var(_cons)`, `var(age)`, `cov(age,_cons)`, `var(e)` (a
#' bracketed grouping variable, `var(_cons[school])`, with several levels
#' and for every `glmer` model), or with `relabel = TRUE` `Variance:
#' <group label> (Intercept)`, `Covariance: ...`, `Residual Variance`, as
#' Stata words them (grouping levels that share a label are named by their
#' variables). For logit and cloglog `glmer` models the random intercept is
#' shown as the median odds (hazard) ratio, `exp(sqrt(2 var) qnorm(0.75))`.
#' `noreeffects = TRUE` drops these rows. A negative binomial model
#' (`glmer.nb()`, or `glmmTMB(family = nbinom2)`) shows menbreg's
#' `lnalpha` (ln of 1/theta, no p-value) before the intercept.
#' Variance CIs are Wald intervals for the log standard deviation,
#' back-transformed; covariance CIs and p-values are Wald on the covariance
#' scale (delta method). The variance of these parameters, and for `glmer`
#' of the fixed effects too, is the inverse observed information from the
#' model's own deviance function (numerical second derivatives), as Stata
#' reports it; `lmer` fixed effects use `vcov()`, Stata `mixed`'s GLS
#' variance. `vce = "model"` changes only the fixed effects (lme4 reports no
#' variance for the random-effects parameters). A variance estimated at or
#' near zero has an ill-conditioned information matrix: its interval, and
#' through the joint inverse a neighbouring component's, can differ from
#' Stata's in the last digits (Stata's own are numerically fragile there);
#' an exact zero gets a blank interval, as in Stata.
#'
#' `glmer` with `nAGQ` above 1 corresponds to Stata's default 7-point
#' adaptive quadrature (`nAGQ = 7`); its log-likelihood, AIC and BIC include
#' the saturated-model constant lme4's `logLik()` leaves out for `nAGQ > 1`,
#' as Stata's e(ll) does. Models with several random effects are Laplace
#' fits in `glmer` (Stata `intmethod(laplace)`). For probit and cloglog
#' links and for `glmer.nb()`, lme4's Laplace approximation is not Stata's,
#' so estimates and standard errors can differ in the last digit (regtab
#' says so once per session); `glmmTMB::glmmTMB()` reproduces Stata's
#' Laplace fits exactly and is the fit to use for exact parity. `nAGQ = 0`
#' fits are refused. `glmmTMB` fits with a `ziformula` or `dispformula`
#' submodel are refused (Stata's me* commands have no such equations), as
#' is the `nbinom1` family (menbreg's constant dispersion, not checked
#' against Stata).
#'
#' `mecloglog` fixed effects are shown as hazard ratios, `exp()` of the
#' coefficients, as Stata tabtools 2.1.10 and later show them (2.1.9 printed
#' the raw log-hazard coefficients under its HR header).
#'
#' Stats: `groups` counts the innermost level's groups; `icc` is the sum of
#' the random-intercept variances over itself plus the residual variance
#' (the latent `pi^2/3`, 1, or `pi^2/6` for logit, probit, cloglog), and is
#' not computed for count families. A factor keeps its header row in models
#' with several grouping levels, as in Stata tabtools 2.1.11 and later (2.1.9
#' lost it).
#'
#' `nlme::lme()` fits also correspond to Stata `mixed` (`method = "ML"` for
#' Stata's default; nlme's own default, REML, is `mixed, reml`), with the
#' same rows, labels, intervals and statistics as an `lmer` fit of the same
#' model. Fixed-effect standard errors are the fit's `varFix`, the GLS
#' variance Stata reports, with z statistics; `summary.lme()` shows other
#' ones for an ML fit (it rescales them by `sqrt(N / (N - p))` and uses t
#' distributions), and regtab never reads it. An unstructured random-effects
#' covariance (`pdSymm`, `pdLogChol`, the default) is Stata's
#' `cov(unstructured)`, `pdDiag` its `cov(independent)` (a variance row per
#' component and no covariance row), and nested levels (`~ 1 | zone/location`)
#' `|| zone: || location:`. lme never estimates a variance of exactly 0 or a
#' correlation of +/-1, so regtab maximises the fit's likelihood again with
#' the variances bounded at 0, as lme4 does: a term whose maximum is on
#' that boundary is shown at it (exact zeros) with blank intervals, as for
#' lmer, and a fit that stopped short of its maximum (typically on the flat
#' ridge towards such a boundary) is refused with a hint to refit. Unused factor
#' levels are dropped, and a fit without `data =` is read from its
#' formula's environment, as lme does. Refused, with an error naming the feature:
#' `correlation =` and `weights =` structures (Stata's `mixed, residuals()`
#' parameters have no rows in regtab's layout), a fixed residual standard
#' deviation (`lmeControl(sigma = )`), `pdIdent`, `pdCompSymm` and
#' `pdBlocked` covariances of several random effects, and a fit whose data
#' no longer reproduce it (fitted with `keep.data = FALSE` and the data
#' changed or gone since). `nlme::gls()` and `nlme::nlme()` fits are not
#' supported.
#'
#' `geepack::geeglm()` fits correspond to Stata `xtgee`, which Stata
#' tabtools 2.1.12 shows by glm's family/link rule: odds ratios for a
#' binomial logit and incidence rate ratios for a Poisson log link
#' (exponentiated, the intercept dropped automatically), coefficients with
#' the intercept otherwise (2.1.11 showed every xtgee on the
#' linear-predictor scale); xtgee's default model-based variance (the scale fixed at 1 for binomial
#' and Poisson families). A fit with `scale.fix = TRUE` is `xtgee,
#' scale(#)`: `scale.value` (which `geeglm()` takes as one value per
#' observation) must be one positive constant, e.g. `rep(2, nrow(d))` with
#' `data = d`, and a varying one is refused. geepack stores only the
#' `scale.value` expression, not its value, so the expression may refer only
#' to the fit's data (its columns, or the data argument's name): a scale
#' held in another variable (`scale.value = sc`, `rep(2, n)`) is refused,
#' since its value at fit time cannot be established (it may have changed
#' since). With a working correlation other than
#' independence, geepack's `scale.fix = TRUE` fit estimates the
#' correlation with the dispersion held at its start value, while xtgee's
#' correlation estimate does not depend on the scale, so the estimates
#' differ from `xtgee, scale(#)`'s (a note says so; about 1e-4 relative on
#' the package's fixture). geepack takes each run of equal `id`
#' values as a cluster, so a fit on data not sorted by `id` (more clusters
#' than ids) is refused: Stata's `xtgee` groups by the panel variable.
#' `vce = "model"` is the fit's own `vcov()`, which follows geepack's
#' `std.err`: the sandwich for the default `"san.se"` (Stata's
#' `vce(robust)` without its `G/(G - 1)` factor), geepack's jackknife
#' variance for `"jack"`, `"j1s"` or `"fij"`. `vce = "robust"`,
#' `"cluster"`, the default variance of a weighted fit and `gee_as =
#' "glm"` always use the stored GEE sandwich (`fit$geese$vbeta`) times
#' `G/(G - 1)`, whatever `std.err` the fit chose. There is
#' no AIC or BIC; `stats = "qic"` (or `"aic"`, then labelled `QICu`) shows
#' QICu = deviance + 2k when the scale is fixed at 1, else a note. Its
#' additive constant follows Stata's deviance; compare QICu only between
#' models on the same data. geepack's default convergence tolerance
#' (`geese.control(epsilon = 1e-4)`) leaves estimates about 1e-6 from
#' xtgee's; `epsilon = 1e-6` reproduces xtgee's default fit.
#'
#' @section Multiple imputation:
#' A multiply imputed model, a `mice` `mira` (`with(imp, glm(...))`) or
#' [tt_mi()] of a list of fits, is shown as one model, as Stata shows
#' `collect: mi estimate: <command>`: rows and labels from the first
#' imputation, numbers pooled by Rubin's rules over each imputation's
#' `tt_vcov(fit, vce, cluster)`, with the degrees of freedom of Stata's
#' `mi estimate` (Barnard and Rubin's small-sample degrees of freedom for
#' `lm`, whose completed-data degrees of freedom are finite, and Rubin's
#' large-sample degrees of freedom for `glm` and Cox models, except a `glm`
#' with an estimated dispersion under `vce = "model"`, whose t reference
#' gives it the small-sample ones; see [tt_mi()]). `stats = c("mi_m", "fmi")` show the number of imputations
#' and the largest fraction of missing information; the log-likelihood,
#' AIC, BIC, R-squared, Root MSE and F are blank, as `mi estimate` reports
#' none, and Observations (Subjects, Events) show when every imputation
#' has the same. The methods sentence adds "with multiple imputation".
#' `lm`, `glm`, `coxph` (Fine-Gray included) and `cmprsk::crr` fits are
#' pooled; other classes are refused. A pooled `mice` result (`mipo`) is
#' refused: pass the `mira`. With `vce = "cluster"`, `cluster` is read
#' from each imputation (a variable: from its data, or for a `mira` from
#' the completed dataset `with()` evaluated it in) or used for every
#' imputation (a vector). Rows and labels come from the first imputation:
#' `mice`'s completed data carry no variable labels, so a `mira` table
#' shows variable names unless the data were labelled (see [tt_mi()]).
#' Use `tt_mi(fits, observation_ids = ids, sample_check = "strict")` when
#' row names no longer retain observation identity. The optional
#' `meta$mi_sample_identity` list has one element per model (`NULL` for
#' non-MI models), recording the method (`"explicit_ids"`, `"row_names"`
#' or `"counts_only"`), per-imputation observation counts and strict policy.
#' It contains no observation IDs; row-name checks still depend on stable
#' original identifiers and counts alone do not establish identity.
#'
#' The effect scale follows the model class, as for a single fit. Stata's
#' `mi estimate` reports coefficients unless `mi estimate` itself is given
#' an eform option (`mi estimate, or: logit ...`), and Stata tabtools
#' 2.1.12 reads that rule from the command line, so that `mi estimate:
#' logit` and `mi estimate, or: logit` both show odds ratios; R has no
#' such prefix, and the tables agree. The one case R cannot express is an
#' eform option given to `mi estimate` for a family Stata shows as
#' coefficients (`mi estimate, eform: glm ..., family(gaussian)`: `exp(b)`
#' in Stata; coefficients in R).
#'
#' @param ... Fitted models, or a single list of fitted models. A `mice`
#'   `mira` or a [tt_mi()] object is one multiply imputed model.
#' @param models Model header labels: a character vector, or one Stata-style
#'   string `"A \\ B"`. Default: the names of a named list of models, else
#'   `"Model"` for one model and `"Model 1"`, `"Model 2"`, ... otherwise.
#'   Extra labels are ignored with a warning.
#' @param coef Estimate-column header for every model; overrides scale
#'   detection (not the exponentiation, which follows the model).
#' @param sep Confidence-interval delimiter. Default `", "`.
#' @param title,footnote Table title (cell A1) and footnote.
#' @param nointercept,keepintercept Drop or keep the intercept row.
#'   `nointercept` has three states: `NULL` (default), the intercept is
#'   dropped when every model is on a ratio scale (for a data frame: an
#'   `effect_scale` with null 1, a ratio `stata_cmd`, or broom.helpers'
#'   `exponentiate` attribute) unless `keepintercept = TRUE`; `TRUE` drops
#'   it for every model; `FALSE` keeps it, ratio scale or not. An explicit
#'   `nointercept` wins over `keepintercept`: with both `TRUE` the
#'   intercept is dropped, silently, as Stata drops it when given both
#'   options. Cutpoints and the
#'   ancillary parameters Stata drops with it (`ln_p`, `p`, `1/p`,
#'   `lnalpha`, `alpha`, and every `Ancillary:`/`Scale:` row of the
#'   multi-equation layout) go too; `lnsigma`/`sigma` and `lngamma`/`gamma`
#'   stay, as in Stata. The rows are identified from the model's structure,
#'   never from their labels: Stata matches the displayed label, so it also
#'   drops a covariate named or labelled `p`, `alpha`, `constant`, `cut1`,
#'   ... (and prints it unexponentiated under `keepintercept`); regtab keeps
#'   and exponentiates it.
#' @param noreeffects Omit random-effects rows of mixed models.
#' @param stats Model statistics rows, as a vector or one space-separated
#'   string of tokens (case-insensitive): `"n"` (`"n_sub"`, `"subjects"`),
#'   `"events"`, `"groups"`, `"mi_m"`, `"aic"`, `"qic"`, `"bic"`, `"ll"`,
#'   `"icc"`, `"r2"`, `"r2_a"`, `"rmse"`, `"F"`, `"fmi"`, and the R-only
#'   `"vce"`. The rows always come in that order, whatever the order of the
#'   tokens, and a row no model reports is left out; see the section on
#'   model statistics.
#' @param stat_fun R only: extra statistics rows below the Stata ones, a
#'   named list whose names are the row labels and whose entries are
#'   functions of the fit (for a multiply imputed model, its [tt_mi()]
#'   object), each returning one number (shown with the Stata format
#'   `"%9.3f"`), one string (shown as it is), or `NA`/`NULL` (a blank
#'   cell); an entry may be `list(fun = , fmt = )` with another Stata
#'   format, e.g. `list("C statistic" = list(fun = function(fit)
#'   survival::concordance(fit)$concordance, fmt = "%5.3f"))`. A row no model
#'   fills is left out.
#' @param relabel Label random-effects rows `Variance: <group> (Intercept)`
#'   and so on instead of Stata's raw `var(_cons)` names.
#' @param digits Decimal places for estimates and confidence limits (0-6;
#'   default 2 or the `tabtools_options(digits =)` value).
#' @param level Confidence level, a proportion (`0.90`) or a percentage (`90`),
#'   from 10% to 99.99% as Stata's `level()`.
#' @param ci_method `"wald"` (default; matches Stata) or `"profile"`
#'   (profile-likelihood intervals for `lm`, `glm` and `glm.nb` fits with
#'   `vce = "stata"` or `"model"` only; see the section on standard
#'   errors).
#' @param keep,drop Rows to keep or drop (not both); see the Rows and keys section.
#' @param labelmatch Match `keep`/`drop` as case-insensitive substrings of the
#'   displayed labels instead.
#' @param dimnonsig Grey out rows whose confidence intervals include the null
#'   in every model (reference rows always).
#' @param factorlabel Accepted for Stata compatibility; R factor levels always
#'   display their labels.
#' @param refcat,omitlabel,emptylabel Labels for base levels, coefficients
#'   dropped for collinearity, and level combinations without observations.
#' @param cutlabels Labels for ordered-model cutpoints, applied in order to
#'   the rows shown as `cut1`, `cut2`, ...: a character vector or one
#'   Stata-style string `"A \\ B"`.
#' @param compact Combine estimate and CI into one column per model.
#' @param nopvalue Suppress p-value columns.
#' @param stars,starslevels Significance stars and their thresholds (three
#'   values, default `c(0.05, 0.01, 0.001)`).
#' @param addrow Extra rows below the table: a named list,
#'   `list("P trend" = c(0.032, 0.041))`, or a Stata string
#'   `"\"P trend\" 0.032 0.041 \\ \"P interaction\" 0.15"`; at most one
#'   value per model, shown in the model's first column, in model order:
#'   fewer values leave the later models' cells blank, and values beyond
#'   the number of models are ignored (as in Stata). Numbers print in full
#'   decimal form (`100000`, `0.00001`); strings are kept verbatim.
#' @param pdp,highpdp Decimal places for p < 0.10 and p >= 0.10.
#' @param cdisc CDISC defaults (4 digits when `digits` is 2, "Estimate"
#'   header, `stats = "n"`).
#' @param labelwidth Maximum label-column width in the workbook (default 45).
#' @param interactions `"fvgen"` (default) or `"native"`; see the
#'   Interactions section.
#' @param xsymbol,vsref fvgen label options: the symbol joining interaction
#'   parts (default a multiplication sign, written with a space on each
#'   side, as fvgen's `xsymbol()`), and a template such as `"(vs. @)"`
#'   appended to flat main-effect levels, `@` standing for the base level.
#' @param vce `"stata"` (default): standard errors and reference
#'   distribution as the Stata command reports them; `"model"`: the fit's
#'   own `vcov()` and `summary()` distribution; `"robust"`: Stata's
#'   `vce(robust)`/`[pweight]` variance; `"cluster"`: `vce(cluster)`, with
#'   `cluster`. One value, or one per model (a list or a character vector
#'   as long as the models). See the sections on standard errors and on
#'   weights, and [tt_vcov()]. R only: a list entry (or, for one model,
#'   `vce` itself) may instead be a user-supplied variance, a function
#'   `function(fit)` returning the coefficients' covariance matrix
#'   (`function(f) sandwich::vcovHC(f, type = "HC3")`) or that matrix
#'   itself, for `lm`, `glm` and `survival::coxph`/`clogit` fits; see
#'   "User-supplied variance".
#' @param vce_df Reference distribution of a user-supplied variance: `NULL`
#'   (default; t with the residual degrees of freedom for `lm`, the normal
#'   otherwise), `"z"`, `"t"` (the residual degrees of freedom), or a
#'   positive number; one value, or one per model. Only for models with a
#'   user-supplied `vce`.
#' @param cluster Cluster variable for `vce = "cluster"`: a one-sided
#'   formula naming one variable (`~id`), a column name, or a vector with
#'   one value per observation (in the order of the fitted rows); or a list
#'   with one entry per model (`NULL` for none). Given without `vce`, it
#'   implies `vce = "cluster"` for the models it applies to; given when no
#'   model has `vce = "cluster"`, it is an error. A variable is read from
#'   the model frame, `glm`'s stored data, or the data as they are now only
#'   when they reproduce the fit (see [tt_vcov()]). An `lm()` fit keeps no
#'   copy of columns outside its formula, so for it `~id` is the `id` of
#'   the data as they are when `regtab()` runs: recoding `id` between the
#'   fit and the table changes the clustered standard errors, and no check
#'   can see it (the model's own columns are checked). Table a model from
#'   the data it was fitted on, or pin the clusters: keep them when fitting
#'   (`ids <- d$id`) and pass `cluster = ids`, or fit with `glm()`, which
#'   stores its data. As in Stata's `vce(cluster id)`,
#'   `~id` is not refused.
#' @param gee_as `"xtgee"` (default) or `"glm"`: how `geepack::geeglm` fits
#'   are shown (see the section on weights).
#' @param finegray `NULL` (default): a `survival::coxph()` fit is a
#'   Fine-Gray (`stcrreg`, SHR) model when it is weighted, its response is
#'   `Surv(start, stop, status)`, and its data are `survival::finegray()`
#'   output (they carry that function's `"event"` attribute); column names
#'   play no part. `TRUE`/`FALSE` declares it, for every `coxph` model or
#'   one value per model, e.g. for a fit whose data are gone or were built
#'   another way. See the section on ordinal, multinomial, count, and
#'   survival models.
#' @param vce_note Append a sentence naming each model's non-default
#'   variance (robust, clustered, survey design, M-estimation) to the
#'   footnote.
#' @param xlsx,sheet,open Excel target.
#' @param borderstyle,font,fontsize,boldp,highlight,zebra,headershade,headercolor,zebracolor
#'   Styling options shared with [table1_tc()].
#' @param csv,markdown,mdappend Additional export targets; `csv` must be a
#'   `.csv` file, and every target is checked before any is written, as in
#'   [table1_tc()].
#' @return A `tt_table` object. It is returned invisibly when `xlsx`, `csv`
#'   or `markdown` writes a file, so assign it (`tab <- regtab(...)`) and
#'   print it to see it. Its `stored` element holds Stata's `r()` results:
#'   `coef_label`, `ci_level`,
#'   `methods` (a sentence built from the models: the estimates by effect
#'   scale, the model by its Stata command, family and link,
#'   "survey-weighted" for `svyglm`, and "univariable" for one predictor
#'   variable, where `strata()`, `cluster()`, `frailty()` and `offset()`
#'   terms are not predictors and a `cmprsk::crr` fit with several
#'   coefficients gets no adjective; Stata builds it from the estimate
#'   header instead, so it differs where Stata's names the wrong model),
#'   `table` (display-scale estimates per row and model), `N_rows`,
#'   `N_cols`, `N_models`, `stars`, per-model statistics (`n_1`, `aic_1`, ...),
#'   and, when written, `xlsx`, `sheet`, `markdown`, `markdown_rows`,
#'   `markdown_cols`; and two R-only entries: `ci_method` (`"wald"` or
#'   `"profile"`), and, when a model's variance fell back to another source
#'   (mixed models only, with a warning), `vce_fallback`, one description
#'   per model (`""` for none). See [as_forest_data()] for the estimates as
#'   data.
#'
#'   `$meta$sample_accounting` carries the source ledger described in
#'   [tt_table()], with separate fitted populations for each model or
#'   imputation and explicit unavailable original-input counts.
#' @export
regtab <- function(..., models = NULL, coef = NULL, sep = ", ",
                   title = NULL, footnote = NULL,
                   nointercept = NULL, keepintercept = FALSE,
                   noreeffects = FALSE, stats = NULL, stat_fun = NULL, relabel = FALSE,
                   digits = NULL, level = 0.95, ci_method = c("wald", "profile"),
                   keep = NULL, drop = NULL, labelmatch = FALSE,
                   dimnonsig = FALSE, factorlabel = TRUE,
                   refcat = "Reference", omitlabel = "Omitted",
                   emptylabel = "Empty", cutlabels = NULL, compact = FALSE,
                   nopvalue = FALSE, stars = FALSE,
                   starslevels = NULL, addrow = NULL,
                   pdp = 3, highpdp = 2, cdisc = FALSE, labelwidth = 45,
                   interactions = c("fvgen", "native"), xsymbol = "\u00d7",
                   vsref = NULL, vce = c("stata", "model", "robust", "cluster"), vce_df = NULL,
                   cluster = NULL, gee_as = c("xtgee", "glm"), finegray = NULL, vce_note = FALSE,
                   xlsx = NULL, sheet = "Regression", open = FALSE,
                   borderstyle = NULL, font = NULL, fontsize = NULL,
                   boldp = NULL, highlight = NULL, zebra = FALSE,
                   headershade = FALSE, headercolor = NULL, zebracolor = NULL,
                   csv = NULL, markdown = NULL, mdappend = FALSE) {
  # sheet = NULL is no sheet: the default (review P2-2).
  sheet_given <- !base::missing(sheet) && !is.null(sheet)
  if (is.null(sheet)) sheet <- "Regression"
  ci_method <- match.arg(ci_method)
  interactions <- match.arg(interactions)
  gee_as <- match.arg(gee_as)
  vce_missing <- missing(vce)
  level_missing <- missing(level)
  fits <- list(...)
  # A named value in `...` that cannot be a model is a mistyped option, e.g.
  # table1_tc's excel() synonym, which Stata's regtab does not have
  # (Phase 6 review P2-9); say so rather than "unsupported model".
  dots_nm <- names(fits) %||% rep("", length(fits))
  typo <- which(nzchar(dots_nm) & vapply(fits, function(f) is.null(f) || is.atomic(f), TRUE))
  if (length(typo)) {
    a <- dots_nm[typo[1]]
    cli::cli_abort(c("{.fn regtab} has no argument {.arg {a}}.",
                     "i" = if (identical(a, "excel")) "Use {.arg xlsx}: Stata's {.code excel()} synonym belongs to {.code table1_tc}."),
                   call = NULL)
  }
  # A mice `mira` is a list whose elements are not fits (its call, its
  # nmis, then the fits in $analyses): it is one multiply imputed model
  # (task 5.14), never unpacked as a list of models (user item (d)).
  fits <- lapply(fits, function(f) if (inherits(f, "mira")) tt_mi(f) else f)
  # Only a plain list is unpacked: a classed object whose coef() fails
  # (brmsfit, ...) is a model the class gate should name, not a list.
  unpacked <- FALSE
  if (length(fits) == 1L && is.list(fits[[1]]) && (!is.object(fits[[1]]) || identical(class(fits[[1]]), "list"))) {
    fits <- fits[[1]]
    unpacked <- TRUE
  }
  fits <- lapply(fits, function(f) if (inherits(f, "mira")) tt_mi(f) else f)
  # A named list supplies default model labels (review P3-4).
  if (is.null(models) && !is.null(names(fits)) && all(nzchar(names(fits)))) models <- names(fits)
  if (!length(fits)) {
    cli::cli_abort("Supply at least one fitted model.", call = NULL)
  }
  for (a in c("keepintercept", "noreeffects", "relabel", "labelmatch", "dimnonsig", "factorlabel",
              "compact", "nopvalue", "stars", "cdisc", "open", "zebra", "headershade", "mdappend", "vce_note")) {
    x <- get(a)
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      cli::cli_abort("{.arg {a}} must be TRUE or FALSE.", call = NULL)
    }
  }
  if (!is.null(nointercept) && (!is.logical(nointercept) || length(nointercept) != 1L || is.na(nointercept))) {
    cli::cli_abort("{.arg nointercept} must be TRUE, FALSE, or NULL.", call = NULL)
  }
  # (Assigned only when cast: `fits[[i]] <- NULL` would delete an element.)
  for (i in seq_along(fits)) if (inherits(fits[[i]], "lmerModLmerTest")) fits[[i]] <- .rt_cast_lmertest(fits[[i]], i)
  for (i in seq_along(fits)) if (inherits(fits[[i]], "clogit")) fits[[i]] <- .rt_clogit_env(fits[[i]])
  # A regtab_uv() stack of clogit fits (stack review nit).
  for (i in seq_along(fits)) {
    if (inherits(fits[[i]], "tt_uv")) {
      fits[[i]]$fits <- lapply(fits[[i]]$fits, function(f) if (inherits(f, "clogit")) .rt_clogit_env(f) else f)
    }
  }
  for (i in seq_along(fits)) .rt_check_model(fits[[i]], i, unpacked)
  sample_accounting <- lapply(seq_along(fits), function(i) {
    .tt_sample_model_population(fits[[i]], "fit", model = i)
  })
  fits <- .rt_fg_tag(fits, finegray)
  # Attach each fit's fit-time model frame where it kept none (coxph,
  # survreg, model = FALSE), verified against the fit, so that nothing below
  # reads data changed after fitting (review P0-1; R/regtab_vce.R); every
  # imputation of a multiply imputed model.
  fits <- lapply(fits, function(f) .rt_mi_apply(f, .rt_anchor))
  # Every fit of a regtab_uv() stack (task 7.13) likewise.
  fits <- lapply(fits, function(f) {
    if (inherits(f, "tt_uv")) f$fits <- lapply(f$fits, .rt_anchor)
    f
  })
  # A fresh pool cache per call for each multiply imputed model.
  fits <- lapply(fits, function(f) {
    if (inherits(f, "tt_mi")) f$pool_cache <- new.env(parent = emptyenv())
    f
  })
  # A fit whose data no longer reproduce it is refused as a whole, not only
  # where a variance needs its rows: its labels, factor levels, reference
  # rows, N and log-likelihood would otherwise come from other data than
  # its estimates (Milestone H decision H-D7; external review F24). Data
  # that hold the same observations in another order are not refused here
  # (what the rows read from them does not depend on the order).
  for (i in seq_along(fits)) {
    subs <- if (inherits(fits[[i]], "tt_mi")) fits[[i]]$analyses else if (inherits(fits[[i]], "tt_uv")) fits[[i]]$fits else list(fits[[i]])
    for (f in subs) {
      if (!isS4(f) && is.list(f) && !is.data.frame(f) && isTRUE(f$tt_stale) && !.rt_reordered_ok(f)) .rt_abort_stale(f, i)
    }
  }
  # Per-model variance (milestone 5w): expand vce/cluster, tag geeglm fits
  # with their reading, check each model's vce against its class, resolve
  # cluster specifications to vectors once, and warn when a weighted
  # model's variance is not robust (review P0-2).
  ex <- .rt_expand_vce(if (vce_missing) NULL else vce, cluster, length(fits), vce_missing)
  vdf <- .rt_expand_vce_df(vce_df, ex$vce)
  for (i in seq_along(fits)) {
    if (!is.character(ex$vce[[i]])) {
      fits[[i]] <- .rt_user_vce(fits[[i]], ex$vce[[i]], vdf[[i]], i)
      ex$vce[[i]] <- "user"
    }
  }
  cluster_spec <- ex$cluster
  for (i in seq_along(fits)) {
    if (inherits(fits[[i]], "geeglm")) fits[[i]] <- .rt_gee_tag(fits[[i]], gee_as, i)
    fit_i <- fits[[i]]
    ex$cluster[i] <- list(tryCatch({
      if (inherits(fit_i, "tt_mi")) {
        .rt_mi_prepare_cluster(fit_i, ex$vce[[i]], ex$cluster[[i]])
      } else {
        .rt_check_vce(fit_i, ex$vce[[i]], ex$cluster[[i]])
        .rt_prepare_cluster(fit_i, ex$cluster[[i]])
      }
    }, error = function(e) {
      cli::cli_abort("Model {i} ({.cls {class(.rt_mi_first(fit_i))[1]}}): invalid {.arg vce}/{.arg cluster}.", parent = e, call = NULL)
    }))
    if (identical(ci_method, "profile")) .rt_check_profile(fit_i, ex$vce[[i]], i)
    # Every fit of a regtab_uv() stack is checked (stack review 7).
    for (f in if (inherits(fit_i, "tt_uv")) fit_i$fits else list(.rt_mi_first(fit_i))) {
      .rt_warn_weights(f, ex$vce[[i]], i, ci_method)
      .rt_note_cox_ties(f, i)
      .rt_note_mn_offset(f, i)
      .rt_note_mn_base(f, i)
    }
  }

  # regtab.ado:82-95: the three constrained-row labels must differ.
  for (a in c("refcat", "omitlabel", "emptylabel")) .check_string(get(a), a)
  if (refcat == omitlabel || refcat == emptylabel || omitlabel == emptylabel) {
    cli::cli_abort("{.arg refcat}, {.arg omitlabel}, and {.arg emptylabel} must differ from each other.",
                   call = NULL)
  }
  sheet <- .check_sheet(sheet)
  # regtab.ado:100-104: digits resolves from the persistent default, then 2.
  if (is.null(digits)) digits <- getOption("tabtools.digits") %||% 2
  digits <- .check_int_range(digits, "digits", 0, 6)
  pdp <- .check_dp(pdp, "pdp")
  highpdp <- .check_dp(highpdp, "highpdp")
  # Stata's level() accepts 10 to 99.99 (pre-release review P3-8: 1.5 was
  # taken as a 1.5% interval).
  if (!is.numeric(level) || length(level) != 1L || is.na(level) ||
      !((level >= 0.1 && level <= 0.9999) || (level >= 10 && level <= 99.99))) {
    cli::cli_abort("{.arg level} must be a proportion from 0.10 to 0.9999 or a percentage from 10 to 99.99, as Stata's {.code level()}.",
                   call = NULL)
  }
  if (level > 1) level <- level / 100
  # Supplied intervals of a data frame fix the level (external review F32);
  # with `level` left at its default, the data frames' own level is used.
  if (level_missing) level <- .rt_df_default_level(fits, level)
  for (i in seq_along(fits)) if (is.data.frame(fits[[i]])) .rt_df_check_level(fits[[i]], level, i)
  has_xlsx <- !is.null(xlsx)
  .tt_check_sheet_xlsx(sheet_given, has_xlsx)
  has_md <- !is.null(markdown)
  if (mdappend && !has_md) cli::cli_abort("{.arg mdappend} requires {.arg markdown}.", call = NULL)
  if (has_md && (!is.character(markdown) || length(markdown) != 1L ||
                 !grepl("\\.(md|markdown|qmd|rmd)$", tolower(markdown)))) {
    cli::cli_abort("{.arg markdown} must specify a .md, .markdown, .qmd, or .rmd file.", call = NULL)
  }
  if (has_xlsx && (!is.character(xlsx) || length(xlsx) != 1L || !grepl("\\.xlsx$", tolower(xlsx)))) {
    cli::cli_abort("{.arg xlsx} must specify a .xlsx file.", call = NULL)
  }
  if (open && !has_xlsx) cli::cli_abort("{.arg open} requires {.arg xlsx}.", call = NULL)
  .tt_preflight_targets(xlsx = xlsx, csv = csv, markdown = markdown, mdappend = mdappend)
  for (a in c("title", "footnote")) .tt_check_text_arg(get(a), a)
  if (labelmatch && is.null(keep) && is.null(drop)) {
    cli::cli_abort("{.arg labelmatch} requires {.arg keep} or {.arg drop}.", call = NULL)
  }
  if (!is.null(keep) && !is.null(drop)) {
    cli::cli_abort("{.arg keep} and {.arg drop} cannot be used together.", call = NULL)
  }
  for (a in c("keep", "drop")) {
    x <- get(a)
    if (!is.null(x) && (!is.character(x) || !length(x) || anyNA(x))) {
      cli::cli_abort("{.arg {a}} must be a character vector of terms.", call = NULL)
    }
  }
  if (!is.null(keep)) keep <- unlist(strsplit(trimws(keep), "[[:space:]]+"))
  if (!is.null(drop)) drop <- unlist(strsplit(trimws(drop), "[[:space:]]+"))
  starslevels_given <- !is.null(starslevels)
  if (is.null(starslevels)) starslevels <- c(0.05, 0.01, 0.001)
  if (!is.numeric(starslevels) || length(starslevels) != 3L || anyNA(starslevels)) {
    cli::cli_abort("{.arg starslevels} requires exactly 3 values (e.g. {.code c(0.05, 0.01, 0.001)}).",
                   call = NULL)
  }
  .check_string(sep, "sep")
  if (!is.numeric(labelwidth) || length(labelwidth) != 1L || is.na(labelwidth)) {
    cli::cli_abort("{.arg labelwidth} must be a number.", call = NULL)
  }
  if (labelwidth <= 0) labelwidth <- 45
  if (!is.null(coef)) .check_string(coef, "coef")
  if (!is.null(vsref)) .check_string(vsref, "vsref")
  if (!is.character(xsymbol) || length(xsymbol) != 1L || is.na(xsymbol)) {
    cli::cli_abort("{.arg xsymbol} must be a single string.", call = NULL)
  }
  style <- tt_resolve_style(font = font, fontsize = fontsize, borderstyle = borderstyle,
                            headershade = headershade, zebra = zebra, headercolor = headercolor,
                            zebracolor = zebracolor, boldp = boldp, highlight = highlight,
                            dimnonsig = dimnonsig)

  M <- length(fits)
  labels <- .rt_model_labels(models, M)
  # CDISC (regtab.ado:305-309): digits 2 becomes 4, the header "Estimate",
  # and stats default to n.
  if (cdisc) {
    if (digits == 2L) digits <- 4L
    if (is.null(stats)) stats <- "n"
  }
  infos <- lapply(fits, tt_model_info)
  for (m in seq_len(M)) infos[[m]]$model_label <- labels[m]
  # dimnonsig needs each model's null (a data frame with an unknown effect
  # scale has none; review P0-2 of group t2a).
  if (dimnonsig) {
    nonull <- which(vapply(infos, function(i) is.na(i$null_value %||% NA_real_), TRUE))
    if (length(nonull)) {
      cli::cli_abort(c(
        "{.arg dimnonsig} needs each model's null value, and model{?s} {nonull} (a data frame) {?has/have} an effect scale regtab does not know.",
        "i" = "Set {.code attr(x, \"null_value\")} to 1 (a ratio) or 0 (a difference), or use a known {.code effect_scale} such as {.val OR}, {.val RR}, {.val Coef.}."
      ), call = NULL)
    }
  }
  o <- list(coef = coef, cdisc = cdisc, nointercept = nointercept, keepintercept = keepintercept,
            level = level, ci_method = ci_method, vce = ex$vce, cluster = ex$cluster, vce_note = vce_note,
            cluster_spec = cluster_spec,
            interactions = interactions, xsymbol = xsymbol,
            vsref = vsref, keep = keep, drop = drop, labelmatch = labelmatch, refcat = refcat,
            omitlabel = omitlabel, emptylabel = emptylabel, digits = digits, sep = sep,
            pdp = pdp, highpdp = highpdp, dimnonsig = dimnonsig, stars = stars,
            starslevels = starslevels, starslevels_given = starslevels_given,
            starstext = if (starslevels_given) stata_fmt(starslevels, "%18.0g") else c("0.05", "0.01", "0.001"),
            stats = .rt_parse_stats(stats), stat_fun = .rt_parse_stat_fun(stat_fun),
            addrow = .rt_parse_addrow(addrow),
            compact = compact, nopvalue = nopvalue,
            # Phase 5 row options: cutpoint labels (5a), random effects (5b).
            cutlabels = .rt_parse_cutlabels(cutlabels), noreeffects = noreeffects, relabel = relabel,
            models = labels, title = title %||% "",
            footnote = footnote %||% "", style = style, labelwidth = labelwidth, sheet = sheet,
            sample_accounting = sample_accounting)
  tt <- tt_regtab_build(fits, infos, o)
  if (any(vapply(fits, inherits, TRUE, "tt_mi"))) {
    tt$meta$mi_sample_identity <- lapply(fits, function(f) {
      if (inherits(f, "tt_mi")) .rt_mi_sample_identity(f) else NULL
    })
  }

  written <- FALSE
  if (has_xlsx) {
    tt_write_xlsx(tt, xlsx, sheet = sheet, open = open)
    tt$stored$xlsx <- xlsx
    tt$stored$sheet <- sheet
    written <- TRUE
  }
  if (!is.null(csv)) {
    tt_write_csv(tt, csv)
    written <- TRUE
  }
  if (has_md) {
    res <- tt_write_markdown(tt, markdown, append = mdappend)
    tt$stored$markdown <- markdown
    tt$stored$markdown_rows <- attr(res, "n_rows")
    tt$stored$markdown_cols <- attr(res, "n_cols")
    written <- TRUE
  }
  if (written) invisible(tt) else tt
}

# The model gate is an allow-list on the fit's first class (Phase 5 review
# P0-10; the Phase 4 review P0-3 fix did the same for lm/glm): each class
# below has been checked against the Stata command it stands for, and any
# other class, including a subclass of one of these (survey::svyglm and
# mgcv::gam are glm subclasses, survey::svycoxph and a frailty coxph are
# coxph subclasses; lmerTest's lmer fit, an lmerMod subclass, is cast to
# lmerMod first by .rt_cast_lmertest()), is refused
# with an error naming it rather than rendered through a default path that
# would show its rows or standard errors wrongly. A data frame (any class,
# tibbles included) is the escape hatch. Classes with Phase 5 methods also
# declare tt_regtab_adapter() (R/regtab_generics.R).
.rt_supported_classes <- c(
  lm = "regress", glm = "logit, probit, poisson, glm", negbin = "nbreg",
  coxph = "stcox, or stcrreg on finegray() data", clogit = "clogit", polr = "ologit, oprobit",
  clm = "ologit, oprobit", multinom = "mlogit", zeroinfl = "zip, zinb", hurdle = "churdle",
  survreg = "streg, intreg", tobit = "tobit", crr = "stcrreg", lmerMod = "mixed", lme = "mixed",
  glmerMod = "melogit, mepoisson, menbreg, meprobit, mecloglog, meglm",
  glmmTMB = "mixed, melogit, mepoisson, menbreg, ...", geeglm = "xtgee",
  svyglm = "svy: logit, svy: poisson, svy: regress, ...", glm_weightit = "teffects ipw (M-estimation)"
)

# The package whose S3 methods (coef(), vcov(), model.frame(), ...) each
# class needs: a fit read from an RDS file in a fresh session does not load
# it (Milestone H task H4).
.rt_class_package <- c(
  negbin = "MASS", polr = "MASS", coxph = "survival", clogit = "survival", survreg = "survival", tobit = "survival",
  clm = "ordinal", multinom = "nnet", zeroinfl = "pscl", hurdle = "pscl", crr = "cmprsk",
  lmerMod = "lme4", glmerMod = "lme4", lme = "nlme", glmmTMB = "glmmTMB", geeglm = "geepack", svyglm = "survey",
  glm_weightit = "WeightIt"
)

# Classes refused with a specific hint.
.rt_refused_hint <- function(cls) {
  switch(cls,
    coxph.penal = "Penalized Cox terms ({.fn frailty}, {.fn ridge}, {.fn pspline}; Stata {.code stcox, shared()}) are not supported: their variance and the frailty variance have not been checked against Stata.",
    coxme = "Mixed-effects Cox models have no Stata equivalent (Stata's {.code mestreg} is parametric; {.code stcox, shared()} is gamma frailty).",
    gam = "Generalized additive models have no Stata equivalent.",
    gls = "Generalized least squares fits ({.fn nlme::gls}) have no direct Stata equivalent; for a random-effects model use {.fn nlme::lme} or {.fn lme4::lmer}.",
    nlme = "Nonlinear mixed-effects models (Stata {.code menl}) are not supported.",
    rq = "Quantile regression (Stata {.code qreg}) is not supported.",
    rlm = "Robust M-estimation ({.fn MASS::rlm}) has no Stata equivalent ({.code rreg} is a different estimator).",
    aov = "Fit the same formula with {.fn lm}.",
    mlm = "A matrix response is several outcomes: fit one {.fn lm} per outcome and pass the fits together.",
    nls = "Nonlinear least squares ({.code nl} in Stata) is not supported.",
    lavaan = "Structural equation models (Stata {.code sem}) are not supported.",
    fixest = "Fixed-effects estimators (Stata {.code areg}, {.code xtreg, fe}, {.code reghdfe}) are not supported: their small-sample corrections have not been checked against Stata.",
    plm = "Panel estimators (Stata {.code xtreg}) are not supported.",
    ivreg = "Instrumental-variables regression (Stata {.code ivregress}) is not supported.",
    lm_robust = , iv_robust = "For Stata's {.code vce(robust)}/{.code vce(cluster)} fit the model with {.fn lm} and use {.code vce = \"robust\"} or {.code \"cluster\"}.",
    betareg = "Beta regression (Stata {.code betareg}) is not supported yet.",
    logistf = "Firth logistic regression ({.code firthlogit}) is not supported: its intervals are profile penalized-likelihood ones.",
    brmsfit = , stanreg = "Bayesian fits (Stata {.code bayes:}) are not supported: regtab reports frequentist Wald statistics.",
    svycoxph = , svyolr = "Survey-design fits other than {.fn survey::svyglm} are not supported yet.",
    glmmPQL = "Penalized quasi-likelihood is not maximum likelihood, which Stata's me* commands use: fit the model with {.fn lme4::glmer} or {.fn glmmTMB::glmmTMB} instead.",
    clmm = "Mixed ordinal models (Stata {.code meologit}) are not supported.",
    NULL
  )
}

.rt_check_model <- function(fit, i, unpacked = FALSE) {
  # Something that is not a model object at all (NULL, a number, a string,
  # a function, a plain list): name it as what it is (user item (d)).
  if (is.null(fit) || is.atomic(fit) || is.function(fit) || is.environment(fit) ||
      (is.list(fit) && !is.object(fit))) {
    what <- if (unpacked) "Element {i} of the list passed to {.fn regtab}" else "Model {i}"
    cli::cli_abort(c(
      paste0(what, " (class {.cls {class(fit)[1]}}) is not a fitted model."),
      "i" = if (unpacked) "{.fn regtab} takes fitted models, or one list of fitted models (not nested lists).",
      "i" = if (!unpacked && is.list(fit)) "{.fn regtab} unpacks a list only when it is the only argument."
    ), call = NULL)
  }
  # A pooled mice result is a data frame subclass, but not a coefficient
  # table: refuse it before the escape hatch reads it (user item (d)).
  if (inherits(fit, "mipo")) .rt_refuse_mi(fit, i)
  if (inherits(fit, "modelsummary_list")) {
    cli::cli_abort(c("Model {i} is a {.cls modelsummary_list}: convert it with {.fn tt_from_modelsummary}.",
                     "i" = "Its coefficients are on the model's scale; {.code tt_from_modelsummary(x, exponentiate = )} says whether the table shows their {.fn exp}."),
                   call = NULL)
  }
  if (inherits(fit, "tt_mi")) return(.rt_mi_check(fit, i))
  # regtab_uv() checked each of its fits when it made them (task 7.13).
  if (inherits(fit, "tt_uv")) return(invisible(TRUE))
  if (is.data.frame(fit)) return(.rt_tidy_check(fit, i))
  cls <- class(fit)[1]
  if (!cls %in% names(.rt_supported_classes)) {
    hint <- .rt_refused_hint(cls)
    cli::cli_abort(c(
      "{.fn regtab} does not support {.cls {cls}} models (model {i}).",
      "i" = hint,
      "i" = "Supported classes: {.cls {names(.rt_supported_classes)}}.",
      "i" = "For another model, pass a data frame shaped like {.fn broom.helpers::tidy_plus_plus} output (see {.help tabtools::regtab}, Data-frame input)."
    ), call = NULL)
  }
  pkg <- .rt_class_package[cls]
  if (!is.na(pkg) && !requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort("Model {i} is a {.cls {cls}} fit, which needs the {.pkg {pkg}} package.", call = NULL)
  }
  # Class-specific refusals of settings with no Stata equivalent.
  .rt_check_fit_diagnostics(fit, i)
  tt_regtab_check(fit, i)
  if (isTRUE(tt_regtab_adapter(fit))) return(invisible(TRUE))
  if (is.null(tryCatch(tt_coef(fit), error = function(e) NULL)) ||
      is.null(tryCatch(.rt_quiet_zero_weight(stats::vcov(fit)), error = function(e) NULL))) {
    cli::cli_abort("Model {i} ({.cls {cls}}) has no {.fn coef}/{.fn vcov} methods.", call = NULL)
  }
  invisible(TRUE)
}

# A pooled mice result (user item (d), task 5.16): its numbers come from
# mice::pool(), whose degrees of freedom are not Stata's mi estimate's, so
# regtab pools the `mira` itself (task 5.14).
.rt_refuse_mi <- function(x, i = NULL) {
  where <- if (is.null(i)) "" else paste0(", model ", i)
  cli::cli_abort(c(
    paste0("{.fn regtab} does not take a pooled {.pkg mice} result ({.cls mipo}", where, ")."),
    "i" = "Pass the {.cls mira} instead ({.code regtab(with(imp, lm(...)))}, not {.code regtab(pool(...))}): regtab pools it by Rubin's rules with Stata's {.code mi estimate} degrees of freedom, which {.fn mice::pool} does not use.",
    "i" = "Or pass the pooled estimates as a data frame with {.field term}, {.field estimate} and {.field std.error} (and {.field df}) or {.field conf.low}/{.field conf.high} (see {.help tabtools::regtab}, Data-frame input)."
  ), call = NULL)
}

# lmerTest's lmer() fit is an lmerMod subclass whose only additions are
# the Satterthwaite/Kenward-Roger machinery; regtab reports Stata mixed's z
# statistics, so the fit is cast back to lmerMod (task 5.16), with a note.
# survival::clogit() stores a coxph formula with an unqualified
# Surv(rep(1, n), case) and strata(), whatever the user wrote, so reading
# its data needs survival attached (task 5.15; a fit read in a fresh
# session, or fitted inside withr::with_package()). The fit's formula gets
# an environment that finds both in survival's namespace, in front of its
# own: nothing is attached, and the user's variables still resolve.
.rt_clogit_env <- function(fit) {
  env <- environment(fit$terms)
  if (is.null(env)) return(fit)
  shim <- new.env(parent = env)
  shim$Surv <- survival::Surv
  shim$strata <- survival::strata
  environment(fit$terms) <- shim
  if (inherits(fit$formula, "formula")) environment(fit$formula) <- shim
  # clogit puts the evaluated formula itself into the coxph call it stores,
  # which model.frame() evaluates again.
  if (!is.null(fit$call) && inherits(fit$call$formula, "formula")) environment(fit$call$formula) <- shim
  fit
}

.rt_cast_lmertest <- function(fit, i) {
  if (!inherits(fit, "lmerModLmerTest")) return(fit)
  out <- tryCatch(methods::as(fit, "lmerMod"), error = function(e) e)
  if (inherits(out, "error") || !identical(class(out)[1], "lmerMod")) {
    cli::cli_abort(c(
      "Model {i} ({.cls lmerModLmerTest}) could not be converted to {.cls lmerMod}.",
      "i" = "Convert it yourself with {.code as(fit, \"lmerMod\")}, or refit with {.fn lme4::lmer}."
    ), call = NULL)
  }
  cli::cli_inform(c(
    "i" = "Model {i}: the {.pkg lmerTest} fit is shown as {.code as(fit, \"lmerMod\")}.",
    " " = "regtab reports Stata {.code mixed}'s z statistics, not lmerTest's Satterthwaite t tests."
  ))
  out
}

#' Refuse settings of a supported class that have no Stata equivalent
#'
#' An S3 generic so each family keeps its checks with its adapter
#' (glmmTMB zero-inflation, glmer nAGQ = 0, survreg distributions, ...).
#' @keywords internal
#' @noRd
tt_regtab_check <- function(fit, i) UseMethod("tt_regtab_check")

#' @export
tt_regtab_check.default <- function(fit, i) invisible(TRUE)

# lm(qr = FALSE) keeps no QR decomposition, which vcov() and summary()
# need (Milestone H task H4: a refusal naming the storage option, not
# "has no coef()/vcov() methods").
#' @export
tt_regtab_check.lm <- function(fit, i) {
  if (is.null(fit$qr)) {
    cli::cli_abort(c(
      "{.fn regtab} needs the QR decomposition of this {.cls {class(fit)[1]}} fit (model {i}), which was fitted with {.code qr = FALSE}.",
      "i" = "Refit with {.code qr = TRUE} (the default)."
    ), call = NULL)
  }
  invisible(TRUE)
}

#' @export
tt_regtab_check.glm <- function(fit, i) {
  tt_regtab_check.lm(fit, i)
  .rt_check_glm_convergence(fit, i)
}

.rt_check_glm_convergence <- function(fit, i = 1L) {
  if (identical(fit$converged, FALSE)) {
    cli::cli_abort(c(
      "Model {i} ({.cls {class(fit)[1]}}) did not converge; its estimates and inference cannot be reported reliably.",
      "i" = "Refit the model and check {.code fit$converged} before calling {.fn regtab} or {.fn tt_vcov}; inspect separation and the iteration limit."
    ), class = "tabtools_error_model_convergence", call = NULL)
  }
  invisible(TRUE)
}

# Model header labels (regtab.ado:1878-1914): user labels in order (a single
# string splits on backslashes), else "Model" / "Model 1".."Model k".
.rt_model_labels <- function(models, M) {
  default <- if (M == 1L) "Model" else paste("Model", seq_len(M))
  if (is.null(models)) return(default)
  if (!is.character(models) || anyNA(models)) {
    cli::cli_abort("{.arg models} must be a character vector of labels.", call = NULL)
  }
  if (length(models) == 1L && grepl("\\", models, fixed = TRUE)) {
    models <- trimws(strsplit(models, "\\", fixed = TRUE)[[1]])
    models <- models[nzchar(models)]
  }
  if (length(models) > M) {
    # Stata errors (r(111)) when models() has more labels than models.
    cli::cli_warn("{.arg models} has {length(models)} labels for {M} model{?s}; the extra labels are ignored.")
  }
  out <- default
  k <- min(length(models), M)
  out[seq_len(k)] <- models[seq_len(k)]
  out
}

#' Regression estimates as a data frame for forest plots
#'
#' The analogue of Stata `regtab, eplotframe()`:
#' one row per displayed coefficient row and model that carries an estimate,
#' interval, or p-value, plus base-level rows (`rowtype = "reference"`, with
#' missing numbers). Omitted and empty cells, header rows, and statistics rows
#' are not included, as in Stata. Estimates are on the display scale
#' (exponentiated for ratio models) at full precision.
#'
#' A table from [effecttab()] gives Stata's `effecttab, eplotframe()`: one
#' row per effect and model (addrow rows excluded), a blank model label read
#' as `"Model k"`. Stata's effecttab stores the estimate rounded to the
#' displayed digits there; R keeps full precision.
#'
#' A table from [comptab()] or [hrcomptab()] gives Stata's `comptab,
#' eplotframe()`: the selected estimates with `section` and `reference`
#' rows, the `section` column naming each row's section, and
#' `source_frame` the model table it came from (built when every model
#' table is a `tt_table`). There `model` and `model_label` are the
#' composite's model column, as displayed (a model table whose models were
#' aligned in another order keeps each model in its composite column), and
#' `label` a row's `relabel` text when it has one; `source_model` keeps the
#' model's index in its own model table. Stata's eplotframe copies the
#' source table's model index and labels instead.
#'
#' @param x A `tt_table` from [regtab()], [effecttab()], [comptab()] or
#'   [hrcomptab()].
#' @return A data frame with columns `label` (as displayed, indent included),
#'   `estimate`, `ll`, `ul`, `pvalue`, `model`, `model_label`, `rowtype`
#'   (`"effect"` or `"reference"`), `section` (empty), and `source_row` (the
#'   body row). Attributes `source`, `ci_level`, `n_models`, `statistic_ids`
#'   (always `"estimate ci pvalue"`, as eplotframe writes it), and per-model
#'   `model_id`, `outcome_id`, `effect_scale`, `model_label` mirror the
#'   Stata frame characteristics. A table with no such row (every row
#'   omitted, or `keep`/`drop` leaving none) gives a zero-row data frame
#'   with the same columns, types and attributes.
#' @export
as_forest_data <- function(x) {
  # comptab()/hrcomptab() tables carry their eplotframe() rows, built from
  # the model tables when the composite was made (R/comptab.R).
  if (inherits(x, "tt_table") && x$command %in% c("comptab", "hrcomptab")) {
    if (is.null(x$meta$forest)) {
      cli::cli_abort(x$meta$forest_error %||% "No forest data were kept for this composite.", call = NULL)
    }
    return(x$meta$forest)
  }
  if (!inherits(x, "tt_table") || is.null(x$meta$regtab_rows)) {
    cli::cli_abort("{.arg x} must be a table returned by {.fn regtab}, {.fn effecttab}, {.fn comptab} or {.fn hrcomptab}.", call = NULL)
  }
  r <- x$meta$regtab_rows
  fm <- x$meta$frame
  keep <- r$status %in% c("est", "base")
  r <- r[keep, , drop = FALSE]
  r <- r[order(r$row, r$model), , drop = FALSE]
  # Every column is built at the length of `r`, so a table with no
  # estimate or reference row (every row omitted, or keep/drop leaving
  # none) gives a zero-row frame with the same columns, types and
  # attributes (external review F10).
  est <- r$status == "est"
  num <- function(v) as.numeric(ifelse(est, v, NA_real_))
  out <- data.frame(
    label = as.character(r$label),
    estimate = num(r$estimate),
    ll = num(r$conf.low),
    ul = num(r$conf.high),
    pvalue = num(r$p.value),
    model = as.integer(r$model),
    # effecttab's eplotframe() names a blank model "Model k"
    # (effecttab.ado:1167-1169); regtab's labels are never blank.
    model_label = as.character((x$meta$forest_model_label %||% fm$model_label)[r$model]),
    rowtype = as.character(ifelse(r$status == "base", "reference", "effect")),
    section = rep("", nrow(r)),
    source_row = as.integer(r$row),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  for (a in names(fm)) attr(out, a) <- fm[[a]]
  # eplotframe() always holds estimate, bounds and p, so its characteristic
  # is fixed (`regtab.ado:2288`), whatever compact/nopvalue did to the
  # table (review P1-3).
  attr(out, "statistic_ids") <- "estimate ci pvalue"
  out
}

# regtab(finegray =): tag each coxph fit with the declaration (external
# review F25), and name the evidence when a fit's column names look like
# finegray() output but its data carry no finegray() mark.
.rt_fg_tag <- function(fits, finegray) {
  M <- length(fits)
  # A multiply imputed model is tagged imputation by imputation.
  is_cox <- vapply(fits, function(f) identical(class(.rt_mi_first(f))[1], "coxph"), TRUE)
  if (!is.null(finegray)) {
    if (!is.logical(finegray) || anyNA(finegray) || !length(finegray) %in% c(1L, M)) {
      cli::cli_abort("{.arg finegray} must be {.code NULL}, {.code TRUE}/{.code FALSE}, or one logical per model.", call = NULL)
    }
    flag <- rep_len(finegray, M)
    bad <- if (length(finegray) == 1L && any(is_cox)) integer() else which(flag & !is_cox)
    if (length(bad)) {
      what <- if (length(bad) == 1L) paste("model", bad, "is not one") else paste("models", paste(bad, collapse = ", "), "are not")
      cli::cli_abort("{.arg finegray} applies to {.fn survival::coxph} fits only ({what}).", call = NULL)
    }
    for (i in which(is_cox)) {
      fits[[i]] <- .rt_mi_apply(fits[[i]], function(f) {
        if (flag[i] && !.rt_fg_structure(f)) {
          cli::cli_abort(c(
            "Model {i} cannot be a Fine-Gray fit ({.code finegray = TRUE}).",
            "i" = "A Fine-Gray {.fn survival::coxph} fit is weighted (the {.fn finegray} weights) with a {.code Surv(start, stop, status)} response."
          ), call = NULL)
        }
        f$tt_finegray <- flag[i]
        f
      })
    }
    return(fits)
  }
  for (i in which(is_cox)) {
    f <- .rt_mi_first(fits[[i]])
    if (.mi_is_finegray(f)) next
    lhs <- tryCatch(paste(deparse(stats::formula(f)[[2L]]), collapse = ""), error = function(e) "")
    w <- if (!is.null(f$call$weights)) paste(deparse(f$call$weights), collapse = "") else ""
    if (grepl("\\bfgstatus\\b", lhs) || identical(w, "fgwt")) {
      cli::cli_inform(c(
        "i" = "Model {i} uses {.fn survival::finegray}'s column names, but {if (.rt_fg_structure(f)) 'its data carry no finegray() mark (they are gone, or were rebuilt)' else 'it is not weighted'}: it is shown as a Cox model ({.code stcox}, HR).",
        " " = "If it is a Fine-Gray fit, pass {.code finegray = TRUE}."
      ))
    }
  }
  fits
}

# Cox ties (review P1-3): R's coxph() defaults to Efron's approximation,
# Stata's stcox to Breslow's, so on data with tied failure times the two
# differ; and stcox refuses weights with efron ("weights not allowed with
# efron option"), so a weighted Efron fit has no Stata analogue. Said once
# per session.
.rt_note_cox_ties <- function(fit, i) {
  if (!identical(class(fit)[1], "coxph") || !identical(fit$method, "efron") || .mi_is_finegray(fit)) {
    return(invisible(FALSE))
  }
  # Without tied failure times Efron's, Breslow's and the exact likelihood
  # coincide: no note, weighted or not (a weighted fit's note used to fire
  # regardless). Unknown ties (no response stored) keep the weighted note.
  tied <- .rt_cox_tied(fit)
  if (!is.null(fit$weights) && !isFALSE(tied)) {
    cli::cli_inform(c(
      "i" = "Model {i} is a weighted {.fn survival::coxph} fit with Efron ties, R's default and the better approximation for tied failure times; its estimates are valid.",
      " " = "It has no Stata analogue ({.code stcox} refuses weights with {.code efron}): refit with {.code ties = \"breslow\"} to match Stata's {.code stcox} after {.code stset [pweight]}."
    ), .frequency = "once", .frequency_id = "tabtools_cox_efron_weighted")
    return(invisible(TRUE))
  }
  if (!isTRUE(tied)) return(invisible(FALSE))
  cli::cli_inform(c(
    "i" = "Model {i} is a {.fn survival::coxph} fit with Efron ties, R's default and the better approximation for tied failure times; its estimates are valid.",
    " " = "Stata's {.code stcox} defaults to Breslow ties, so its estimates differ slightly: refit with {.code ties = \"breslow\"} to match them."
  ), .frequency = "once", .frequency_id = "tabtools_cox_efron_ties")
  invisible(TRUE)
}

# A multinomial fit with an offset (Milestone H decision H-D10: supported,
# with the pseudo R-squared against the with-offset null) has no Stata
# equivalent, since mlogit refuses offset() (muse P2-20). Said once per
# session.
.rt_note_mn_offset <- function(fit, i) {
  if (!identical(class(fit)[1], "multinom")) return(invisible(FALSE))
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  if (is.null(mf) || is.null(stats::model.offset(mf))) return(invisible(FALSE))
  cli::cli_inform(c(
    "i" = "Model {i} ({.fn nnet::multinom}) has an offset: Stata's {.code mlogit} refuses {.code offset()}, so this table has no Stata equivalent (R only)."
  ), .frequency = "once", .frequency_id = "tabtools_multinom_offset")
  invisible(TRUE)
}

# nnet::multinom() compares every outcome with the first level; Stata's
# mlogit, by default, with the most frequent outcome, so the default Stata
# table has other rows and relative-risk ratios. Said once per session,
# only when the two bases differ (Muse audit P1-14; user decision
# 2026-09-28).
.rt_note_mn_base <- function(fit, i) {
  if (!identical(class(fit)[1], "multinom")) return(invisible(FALSE))
  lev <- fit$lev
  mf <- tryCatch(stats::model.frame(fit), error = function(e) NULL)
  y <- if (is.null(mf)) NULL else stats::model.response(mf)
  if (is.null(lev) || length(lev) < 3L || is.null(y) || !is.factor(y)) return(invisible(FALSE))
  n <- table(factor(y, levels = lev))
  modal <- names(n)[which.max(n)]
  if (identical(modal, lev[1])) return(invisible(FALSE))
  cli::cli_inform(c(
    "i" = "Model {i} ({.fn nnet::multinom}) uses the first level, {.val {lev[1]}}, as its base outcome; Stata's {.code mlogit} uses the most frequent, {.val {modal}}, unless {.code baseoutcome()} is given.",
    " " = "The table matches {.code mlogit, baseoutcome({match(lev[1], lev)})} on outcomes coded 1, 2, ...; {.code relevel()} the outcome for another base."
  ), .frequency = "once", .frequency_id = "tabtools_multinom_base")
  invisible(TRUE)
}

# Whether a coxph fit has tied failure times (TRUE/FALSE), from its stored
# response or its fit-time model frame; NA when neither is stored.
.rt_cox_tied <- function(fit) {
  y <- fit$y %||% (if (is.data.frame(fit$model)) stats::model.response(fit$model))
  if (is.null(y)) return(NA)
  ev <- y[, ncol(y)] == 1
  anyDuplicated(y[ev, ncol(y) - 1L]) > 0L
}
