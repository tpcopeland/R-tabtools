# demo_tabtools_short.R: the main tabtools tables, one short example each.
#
# Run "Setup" and "Example data" first; after that every numbered section
# runs on its own. To make a table from your own data, copy a section and
# change the data frame and the variable names.
#
# Each table prints in the console and is also saved as a sheet of
# demo_tabtools_short.xlsx in the working directory. Section 6 saves Word
# and CSV copies. demo_tabtools_short.md, next to this file, shows the
# code with its output.
#
# Needs survival (installed with R) and, for section 6, flextable.

# Setup ----
library(tabtools)
library(survival)   # coxph() and Surv(), for the survival models

# Example data ----
# A made-up cohort of 1,000 people followed for up to 5 years. Older people
# and people with diabetes are more likely to be treated.
set.seed(2026)
n <- 1000
cohort <- data.frame(
  age       = round(rnorm(n, mean = 60, sd = 10)),
  female    = rbinom(n, 1, 0.5),
  diabetes  = rbinom(n, 1, 0.2),
  education = sample(c("Primary", "Secondary", "University"), n, replace = TRUE)
)
cohort$treated <- rbinom(n, 1, plogis(-3 + 0.04 * cohort$age + 0.8 * cohort$diabetes))
death_time <- rexp(n, 0.04 * exp(0.03 * (cohort$age - 60) + 0.5 * cohort$diabetes -
                                   0.4 * cohort$treated))
cohort$died  <- as.integer(death_time <= 5)
cohort$years <- pmin(death_time, 5)

# Categorical variables should be factors. The level order is the row
# order, and in a regression table the first level is the reference.
cohort$treated   <- factor(cohort$treated, levels = 0:1, labels = c("Control", "Treated"))
cohort$education <- factor(cohort$education, levels = c("Primary", "Secondary", "University"))

# Labels are the text the tables show instead of the column names.
labels <- c(age = "Age (years)", female = "Female", diabetes = "Diabetes",
            education = "Education", treated = "Treatment")
for (v in names(labels)) attr(cohort[[v]], "label") <- labels[[v]]

# 1. Table 1: baseline characteristics ----
# vars names each row and how to summarise it:
#   "contn"  mean and SD          "conts"  median and IQR
#   "bin"    count and % of 1s    "cat"    count and % for each level
table1 <- table1_tc(cohort, by = "treated",
                    vars = c(age = "contn", female = "bin", diabetes = "bin",
                             education = "cat"),
                    total = "after", smd = TRUE,
                    xlsx = "demo_tabtools_short.xlsx", sheet = "Table 1")
table1

# 2. Regression tables ----
# regtab() takes one or more fitted models and picks the effect measure
# from the model: odds ratios for logistic, hazard ratios for Cox, and so on.
logistic <- glm(died ~ treated + age + female + diabetes + education,
                family = binomial, data = cohort)
logistic_table <- regtab(logistic, nointercept = TRUE,
                         xlsx = "demo_tabtools_short.xlsx", sheet = "Logistic")
logistic_table

# Several models side by side: crude and adjusted hazard ratios.
crude    <- coxph(Surv(years, died) ~ treated, data = cohort)
adjusted <- coxph(Surv(years, died) ~ treated + age + female + diabetes + education,
                  data = cohort)
cox_table <- regtab(crude, adjusted, models = c("Crude", "Adjusted"),
                    xlsx = "demo_tabtools_short.xlsx", sheet = "Cox")
cox_table

# 3. Incidence rates ----
# tt_rates() counts events and person-time by group; stratetab() lays them
# out. ratescale = 1000 gives rates per 1,000 person-years.
rates <- stratetab(tt_rates(cohort, time = "years", event = "died", by = "treated"),
                   outlabels = "Death", outcomeids = "died", explabels = "Treatment",
                   ratescale = 1000, unitlabel = "1,000",
                   xlsx = "demo_tabtools_short.xlsx", sheet = "Rates")
rates

# 4. Table 2: rates beside hazard ratios ----
# hrcomptab() puts the adjusted hazard ratio for "Treated" on the Treated
# row of the rate table; Control becomes the reference. rownames are
# matched as parts of the row labels, not whole labels ("treat" would also
# match the "Treatment" heading).
table2 <- hrcomptab(rates, regtab(adjusted), rownames = "treated",
                    title = "Table 2. Deaths by treatment",
                    footnote = "aHR: hazard ratio adjusted for age, sex, diabetes and education.",
                    xlsx = "demo_tabtools_short.xlsx", sheet = "Table 2")
table2

# 5. Weighted Table 1 (inverse probability of treatment weights) ----
# Fit a propensity score model, turn it into weights, and pass the weight
# column as wt. Standardized mean differences near 0 mean good balance.
ps <- fitted(glm(treated ~ age + female + diabetes + education,
                 family = binomial, data = cohort))
cohort$iptw <- ifelse(cohort$treated == "Treated", 1 / ps, 1 / (1 - ps))
weighted <- table1_tc(cohort, by = "treated",
                      vars = c(age = "contn", female = "bin", diabetes = "bin",
                               education = "cat"),
                      wt = "iptw", smd = TRUE,
                      xlsx = "demo_tabtools_short.xlsx", sheet = "Table 1 weighted")
weighted

# 6. Word and CSV ----
# Any table converts to a flextable for Word; save_as_docx() takes several.
flextable::save_as_docx(`Table 1` = flextable::as_flextable(table1),
                        `Table 2` = flextable::as_flextable(table2),
                        path = "demo_tabtools_short.docx")
tt_write_csv(table2, "demo_tabtools_short_table2.csv")
