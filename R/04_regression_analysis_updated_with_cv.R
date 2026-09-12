# ==============================================================================
# 04_regression_analysis_updated.R
# Updated Ontario housing affordability models
# ==============================================================================

library(data.table)
library(dplyr)
library(sandwich)
library(lmtest)
library(car)
library(betareg)

# ------------------------------------------------------------------------------
# 1. Load and prepare data
# ------------------------------------------------------------------------------

dat <- fread(
  "H:/github_repos/Assignment_MMAH/data/processed/final_analytic_dataset.csv",
  encoding = "Latin-1"
) |>
  as_tibble() |>
  transmute(
    municipality_name = GEO,
    n_total = `Total - Shelter-cost-to-income ratio`,
    n_30_50 = `30% to less than 50%`,
    n_50plus = `50% or more`,

    renter_pct = `Renter`,
    median_age = `Median age of the population`,
    median_income_1p =
      `Median total income of one-person households in 2020 ($)`,
    pop_density =
      `Population density per square kilometre`,
    avg_hh_size =
      `Average household size`,
    unsuitable_repairs_pct =
      `'Not suitable' and 'major repairs needed'`,
    pop_growth_pct =
      `Population percentage change, 2016 to 2021`,
    population_2021 =
      `Population, 2021`,

    cmhc_municipality = municipality,
    starts_2020 = starts_2020
  ) %>% 
  mutate(
    n_30plus = n_30_50 + n_50plus,

    affordability_30plus =
      n_30plus / n_total,

    affordability_50plus =
      n_50plus / n_total,

    income_10k =
      median_income_1p / 10000,

    density_100 =
      pop_density / 100,

    log_density =
      log1p(pop_density),

    # IMPORTANT:
    # municipalities without a CMHC match are missing, not zero starts.
    cmhc_covered =
      !is.na(cmhc_municipality),

    starts_2020_per_1000 =
      if_else(
        cmhc_covered &
          !is.na(population_2021) &
          population_2021 > 0,
        1000 * starts_2020 / population_2021,
        NA_real_
      )
  )

# ------------------------------------------------------------------------------
# 2. Model formulas
# ------------------------------------------------------------------------------

formula_30 <- affordability_30plus ~
  renter_pct +
  median_age +
  income_10k +
  density_100 +
  avg_hh_size +
  unsuitable_repairs_pct +
  pop_growth_pct

formula_50 <- affordability_50plus ~
  renter_pct +
  median_age +
  income_10k +
  density_100 +
  avg_hh_size +
  unsuitable_repairs_pct +
  pop_growth_pct

formula_30_log_density <- affordability_30plus ~
  renter_pct +
  median_age +
  income_10k +
  log_density +
  avg_hh_size +
  unsuitable_repairs_pct +
  pop_growth_pct

formula_50_log_density <- affordability_50plus ~
  renter_pct +
  median_age +
  income_10k +
  log_density +
  avg_hh_size +
  unsuitable_repairs_pct +
  pop_growth_pct

# ------------------------------------------------------------------------------
# 3. Complete Statistics Canada sample
# ------------------------------------------------------------------------------

base_vars <- c(
  "affordability_30plus",
  "affordability_50plus",
  "renter_pct",
  "median_age",
  "income_10k",
  "density_100",
  "log_density",
  "avg_hh_size",
  "unsuitable_repairs_pct",
  "pop_growth_pct"
)

model_data <- dat |>
  filter(
    complete.cases(
      across(all_of(base_vars))
    )
  )

cat("Primary model N =", nrow(model_data), "\n")

# ------------------------------------------------------------------------------
# 4. OLS baseline with HC3 robust standard errors
# ------------------------------------------------------------------------------

ols_30 <- lm(
  formula_30,
  data = model_data
)

ols_50 <- lm(
  formula_50,
  data = model_data
)

ols_30_hc3 <- coeftest(
  ols_30,
  vcov. = vcovHC(
    ols_30,
    type = "HC3"
  )
)

ols_50_hc3 <- coeftest(
  ols_50,
  vcov. = vcovHC(
    ols_50,
    type = "HC3"
  )
)

print(ols_30_hc3)
print(ols_50_hc3)

cat(
  "\n30%+ R2:",
  summary(ols_30)$r.squared,
  "\n50%+ R2:",
  summary(ols_50)$r.squared,
  "\n"
)

# ------------------------------------------------------------------------------
# 5. Fractional logistic models
# ------------------------------------------------------------------------------

frac_30 <- glm(
  formula_30,
  data = model_data,
  family = quasibinomial(
    link = "logit"
  )
)

frac_50 <- glm(
  formula_50,
  data = model_data,
  family = quasibinomial(
    link = "logit"
  )
)

frac_30_hc3 <- coeftest(
  frac_30,
  vcov. = vcovHC(
    frac_30,
    type = "HC3"
  )
)

frac_50_hc3 <- coeftest(
  frac_50,
  vcov. = vcovHC(
    frac_50,
    type = "HC3"
  )
)

print(frac_30_hc3)
print(frac_50_hc3)

# Average marginal effects in percentage points
average_marginal_effects <- function(model) {

  p <- predict(
    model,
    type = "response"
  )

  beta <- coef(model)

  beta *
    mean(
      p * (1 - p)
    ) *
    100
}

ame_30 <- average_marginal_effects(
  frac_30
)

ame_50 <- average_marginal_effects(
  frac_50
)

frac_df <- data.frame(
  term = names(coef(frac_30)),
  estimate_30 = frac_30_hc3[,2],
  ame_30 = ame_30,
  p_30 = frac_30_hc3[, 4],
  estimate_50 = frac_50_hc3[,2],
  ame_50 = ame_50,
  p_50 = frac_50_hc3[, 4],
  row.names = NULL
)

data.table::fwrite(
  frac_df, 
  "H:/github_repos/Assignment_MMAH/data/processed/avg_marg_effects.csv"
)



# ------------------------------------------------------------------------------
# 6. Population-density sensitivity
# ------------------------------------------------------------------------------

ols_30_log <- lm(
  formula_30_log_density,
  data = model_data
)

ols_50_log <- lm(
  formula_50_log_density,
  data = model_data
)

cat(
  "\nRaw-density 30%+ R2:",
  summary(ols_30)$r.squared,
  "\nLog-density 30%+ R2:",
  summary(ols_30_log)$r.squared,
  "\nRaw-density 50%+ R2:",
  summary(ols_50)$r.squared,
  "\nLog-density 50%+ R2:",
  summary(ols_50_log)$r.squared,
  "\n"
)

# ------------------------------------------------------------------------------
# 7. Household-weighted quasibinomial sensitivity
# ------------------------------------------------------------------------------

weighted_30 <- glm(
  formula_30,
  data = model_data,
  weights = n_total,
  family = quasibinomial(
    link = "logit"
  )
)

weighted_50 <- glm(
  formula_50,
  data = model_data,
  weights = n_total,
  family = quasibinomial(
    link = "logit"
  )
)

summary(weighted_30)
summary(weighted_50)


# ------------------------------------------------------------------------------
# 9. CMHC housing-start sensitivity model
# ------------------------------------------------------------------------------

cmhc_model_data <- model_data |>
  filter(
    cmhc_covered,
    !is.na(starts_2020_per_1000)
  )

cat(
  "\nCMHC-enriched sample N =",
  nrow(cmhc_model_data),
  "\n"
)

formula_30_cmhc <- update(
  formula_30,
  . ~ . + starts_2020_per_1000
)

formula_50_cmhc <- update(
  formula_50,
  . ~ . + starts_2020_per_1000
)

cmhc_ols_30 <- lm(
  formula_30_cmhc,
  data = cmhc_model_data
)

cmhc_ols_50 <- lm(
  formula_50_cmhc,
  data = cmhc_model_data
)

print(
  coeftest(
    cmhc_ols_30,
    vcov. = vcovHC(
      cmhc_ols_30,
      type = "HC3"
    )
  )
)

print(
  coeftest(
    cmhc_ols_50,
    vcov. = vcovHC(
      cmhc_ols_50,
      type = "HC3"
    )
  )
)

# Compare with the same model on the CMHC subset without starts.
cmhc_ols_30_no_starts <- lm(
  formula_30,
  data = cmhc_model_data
)

cmhc_ols_50_no_starts <- lm(
  formula_50,
  data = cmhc_model_data
)

cat(
  "\n30%+ CMHC subset R2 without starts:",
  summary(cmhc_ols_30_no_starts)$r.squared,
  "\n30%+ CMHC subset R2 with starts:",
  summary(cmhc_ols_30)$r.squared,
  "\n50%+ CMHC subset R2 without starts:",
  summary(cmhc_ols_50_no_starts)$r.squared,
  "\n50%+ CMHC subset R2 with starts:",
  summary(cmhc_ols_50)$r.squared,
  "\n"
)

# ------------------------------------------------------------------------------
# Interpretation reminder
# ------------------------------------------------------------------------------

cat(
  "\nThese are cross-sectional municipal associations.",
  "Coefficients should not be interpreted as causal effects.\n"
)


# ------------------------------------------------------------------------------
# 11. 10-fold cross-validation: log(1 + population density)
# ------------------------------------------------------------------------------

# Use the same folds for every model so RMSE values are directly comparable.
set.seed(123)

model_data <- model_data |>
  mutate(
    cv_fold = sample(
      rep(
        1:10,
        length.out = n()
      )
    )
  )

cv_compare_models <- function(data, outcome, outcome_label) {

  rhs <- paste(
    "renter_pct",
    "median_age",
    "income_10k",
    "log_density",
    "avg_hh_size",
    "unsuitable_repairs_pct",
    "pop_growth_pct",
    sep = " + "
  )

  model_formula <- as.formula(
    paste(
      outcome,
      "~",
      rhs
    )
  )

  fold_predictions <- lapply(
    1:10,
    function(fold) {

      train <- data |>
        filter(
          cv_fold != fold
        )

      test <- data |>
        filter(
          cv_fold == fold
        )

      # OLS
      ols_fit <- lm(
        model_formula,
        data = train
      )

      pred_ols <- predict(
        ols_fit,
        newdata = test
      )

      # Fractional logistic regression
      frac_fit <- glm(
        model_formula,
        data = train,
        family = quasibinomial(
          link = "logit"
        )
      )

      pred_frac <- predict(
        frac_fit,
        newdata = test,
        type = "response"
      )

      # Adjust observed zeros/ones within the training fold only.
      n_train <- nrow(train)

      train <- train |>
        mutate(
          y_beta =
            (.data[[outcome]] * (n_train - 1) + 0.5) /
            n_train
        )


      tibble(
        actual = test[[outcome]],
        OLS = pred_ols,
        `Fractional logit` = pred_frac
      
      )
    }
  ) |>
    bind_rows()

  fold_predictions |>
    summarise(
      OLS =
        sqrt(
          mean(
            (actual - OLS)^2,
            na.rm = TRUE
          )
        ) * 100,

      `Fractional logit` =
        sqrt(
          mean(
            (actual - `Fractional logit`)^2,
            na.rm = TRUE
          )
        ) * 100,

      
    ) |>
    mutate(
      Outcome = outcome_label,
      .before = 1
    )
}


cv_rmse_log_density <- bind_rows(

  cv_compare_models(
    model_data,
    "affordability_30plus",
    "30%+ affordability"
  ),

  cv_compare_models(
    model_data,
    "affordability_50plus",
    "50%+ severe affordability"
  )
)

data.table::fwrite(
  cv_rmse_log_density,
  "H:/github_repos/Assignment_MMAH/data/processed/rmse_log_results.csv"
)

print(
  cv_rmse_log_density
)

# Presentation-ready Markdown table
dir.create(
  "output/model",
  recursive = TRUE,
  showWarnings = FALSE
)

write.csv(
  cv_rmse_log_density,
  "output/model/cv_rmse_log_density.csv",
  row.names = FALSE
)

writeLines(
  knitr::kable(
    cv_rmse_log_density,
    format = "pipe",
    digits = 2,
    caption = paste(
      "10-fold cross-validation RMSE (percentage points);",
      "all models use log(1 + population density)."
    )
  ),
  "output/model/cv_rmse_log_density.md"
)
