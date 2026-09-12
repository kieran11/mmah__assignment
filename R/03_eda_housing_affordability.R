# ==============================================================================
# 04_eda_housing_affordability_updated_v2.R
# Standalone R script converted from the R Markdown EDA
# ==============================================================================

# Input file
data_file <- "data/processed/analytic_dataset.csv"

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)


# ------------------------------------------------------------------------------
# Purpose
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 1. Load and prepare the data
# ------------------------------------------------------------------------------

dat <- data.table::fread(
  data_file,
  encoding = "Latin-1"
)

eda_data <- dat |>
  as_tibble() |>
  rename(
    n_total =
      `Total - Shelter-cost-to-income ratio`,
    n_30_to_50 =
      `30% to less than 50%`,
    n_50plus =
      `50% or more`,
    population_2021 =
      `Population, 2021`,
    population_growth_pct =
      `Population percentage change, 2016 to 2021`,
    population_density =
      `Population density per square kilometre`,
    median_age =
      `Median age of the population`,
    average_household_size =
      `Average household size`,
    median_income_1p =
      `      Median total income of one-person households in 2020 ($)`,
    renter_pct =
      `  Renter`,
    unsuitable_major_repairs_pct =
      `    'Not suitable' and 'major repairs needed'`,
    housing_target_status =
      `Housing Target Status`
  ) |>
  mutate(
    # Recalculate affordability outcomes from the underlying counts
    affordability_30plus_pct =
      100 * (n_30_to_50 + n_50plus) / n_total,

    affordability_50plus_pct =
      100 * n_50plus / n_total,

    log_population_density =
      log1p(population_density),

    # A municipality name in the CMHC join indicates actual CMHC coverage.
    # Rows without a CMHC match may currently contain zero after NA replacement;
    # restore these to NA so "not covered" is not interpreted as "zero starts".
    cmhc_covered =
      !is.na(municipality),

    starts_2020_cmhc =
      if_else(
        cmhc_covered,
        as.numeric(starts_2020),
        NA_real_
      ),

    starts_2021_cmhc =
      if_else(
        cmhc_covered,
        as.numeric(starts_2021),
        NA_real_
      ),

    starts_2020_per_1000_clean =
      if_else(
        cmhc_covered &
          !is.na(population_2021) &
          population_2021 > 0,
        1000 * starts_2020_cmhc / population_2021,
        NA_real_
      )
  )


# ------------------------------------------------------------------------------
# 2. Data quality assessment
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Missingness
# ------------------------------------------------------------------------------

analysis_variables <- c(
  "affordability_30plus_pct",
  "affordability_50plus_pct",
  "population_2021",
  "population_growth_pct",
  "renter_pct",
  "median_age",
  "median_income_1p",
  "population_density",
  "average_household_size",
  "unsuitable_major_repairs_pct",
  "starts_2020_per_1000_clean",
  "TNR_SF",
  "TNR_LF"
)

missingness <- eda_data |>
  summarise(
    across(
      all_of(analysis_variables),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing"
  ) |>
  mutate(
    `Missing (%)` =
      100 * Missing / nrow(eda_data)
  ) |>
  arrange(desc(`Missing (%)`))

knitr::kable(
  missingness,
  digits = 1,
  caption = "Missingness in analysis variables"
)


# ------------------------------------------------------------------------------
# Complete-case samples
# ------------------------------------------------------------------------------

base_model_vars <- c(
  "affordability_30plus_pct",
  "affordability_50plus_pct",
  "renter_pct",
  "median_age",
  "median_income_1p",
  "population_density",
  "average_household_size",
  "unsuitable_major_repairs_pct",
  "population_growth_pct"
)

supply_model_vars <- c(
  base_model_vars,
  "starts_2020_per_1000_clean"
)

sample_summary <- tibble(
  Sample = c(
    "Full dataset",
    "Complete StatsCan predictor sample",
    "Complete sample including CMHC starts"
  ),
  CSDs = c(
    nrow(eda_data),
    sum(complete.cases(eda_data[, base_model_vars])),
    sum(complete.cases(eda_data[, supply_model_vars]))
  )
)

knitr::kable(
  sample_summary,
  caption = "Available sample sizes"
)


# ------------------------------------------------------------------------------
# Census non-response
# ------------------------------------------------------------------------------

tnr_summary <- eda_data |>
  summarise(
    `Median short-form TNR` =
      median(TNR_SF, na.rm = TRUE),
    `90th percentile short-form TNR` =
      quantile(TNR_SF, 0.90, na.rm = TRUE),
    `Median long-form TNR` =
      median(TNR_LF, na.rm = TRUE),
    `90th percentile long-form TNR` =
      quantile(TNR_LF, 0.90, na.rm = TRUE)
  )

knitr::kable(
  tnr_summary,
  digits = 1,
  caption = "Census total non-response rates"
)


# ------------------------------------------------------------------------------
# Household denominator
# ------------------------------------------------------------------------------

denominator_summary <- eda_data |>
  summarise(
    `CSDs with denominator` =
      sum(!is.na(n_total)),
    `Median households` =
      median(n_total, na.rm = TRUE),
    `Minimum households` =
      min(n_total, na.rm = TRUE),
    `CSDs with <100 households` =
      sum(n_total < 100, na.rm = TRUE)
  )

knitr::kable(
  denominator_summary,
  digits = 0,
  caption = "Affordability denominator summary"
)


# ------------------------------------------------------------------------------
# CMHC coverage
# ------------------------------------------------------------------------------

cmhc_summary <- eda_data |>
  summarise(
    `Ontario CSDs` =
      n(),
    `CSDs matched to CMHC` =
      sum(cmhc_covered),
    `CMHC coverage (%)` =
      100 * mean(cmhc_covered)
  )

knitr::kable(
  cmhc_summary,
  digits = 1,
  caption = "CMHC housing-start coverage"
)


# ------------------------------------------------------------------------------
# 3. Affordability outcomes
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------

outcome_summary <- eda_data |>
  summarise(
    `30%+ mean` =
      mean(affordability_30plus_pct, na.rm = TRUE),
    `30%+ median` =
      median(affordability_30plus_pct, na.rm = TRUE),
    `30%+ Q1` =
      quantile(affordability_30plus_pct, 0.25, na.rm = TRUE),
    `30%+ Q3` =
      quantile(affordability_30plus_pct, 0.75, na.rm = TRUE),
    `50%+ mean` =
      mean(affordability_50plus_pct, na.rm = TRUE),
    `50%+ median` =
      median(affordability_50plus_pct, na.rm = TRUE),
    `50%+ Q1` =
      quantile(affordability_50plus_pct, 0.25, na.rm = TRUE),
    `50%+ Q3` =
      quantile(affordability_50plus_pct, 0.75, na.rm = TRUE)
  )

knitr::kable(
  outcome_summary,
  digits = 1,
  caption = "Distribution of municipal affordability outcomes (%)"
)


# ------------------------------------------------------------------------------
# Distribution of the 30%+ outcome
# ------------------------------------------------------------------------------

ggplot(
  eda_data,
  aes(x = affordability_30plus_pct)
) +
  geom_histogram(
    bins = 30,
    boundary = 0
  ) +
  labs(
    x = "Households spending ≥30% of income on shelter (%)",
    y = "Number of municipalities",
    title = "Distribution of housing affordability challenges"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# Distribution of severe affordability
# ------------------------------------------------------------------------------

ggplot(
  eda_data,
  aes(x = affordability_50plus_pct)
) +
  geom_histogram(
    bins = 30,
    boundary = 0
  ) +
  labs(
    x = "Households spending ≥50% of income on shelter (%)",
    y = "Number of municipalities",
    title = "Distribution of severe housing affordability challenges"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# Relationship between the two outcomes
# ------------------------------------------------------------------------------

ggplot(
  eda_data,
  aes(
    x = affordability_30plus_pct,
    y = affordability_50plus_pct
  )
) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    x = "30%+ affordability challenge (%)",
    y = "50%+ severe affordability challenge (%)",
    title = "Relationship between affordability and severe affordability"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 4. Predictor distributions
# ------------------------------------------------------------------------------

predictor_summary <- eda_data |>
  summarise(
    across(
      c(
        population_2021,
        population_growth_pct,
        renter_pct,
        median_age,
        median_income_1p,
        population_density,
        average_household_size,
        unsuitable_major_repairs_pct,
        starts_2020_per_1000_clean
      ),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
        q3 = ~ quantile(.x, 0.75, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(mean|median|q1|q3)$"
  )

knitr::kable(
  predictor_summary,
  digits = 1,
  caption = "Summary of candidate predictors"
)


# ------------------------------------------------------------------------------
# Population
# ------------------------------------------------------------------------------

ggplot(
  eda_data |>
    filter(
      !is.na(population_2021),
      population_2021 > 0
    ),
  aes(x = population_2021)
) +
  geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "2021 population (log scale)",
    y = "Number of municipalities",
    title = "Ontario municipalities vary substantially in population"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# Population growth, 2016–2021
# ------------------------------------------------------------------------------

ggplot(
  eda_data,
  aes(x = population_growth_pct)
) +
  geom_histogram(bins = 30) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    x = "Population change, 2016–2021 (%)",
    y = "Number of municipalities",
    title = "Population growth varies substantially across Ontario municipalities"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# Population density
# ------------------------------------------------------------------------------

ggplot(
  eda_data |>
    filter(!is.na(population_density)),
  aes(x = population_density)
) +
  geom_histogram(bins = 30) +
  scale_x_log10(labels = scales::comma) +
  labs(
    x = "Population density (persons/km², log scale)",
    y = "Number of municipalities",
    title = "Population density is strongly right-skewed"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 5. Housing starts
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Raw starts among CMHC-covered municipalities
# ------------------------------------------------------------------------------

starts_summary <- eda_data |>
  filter(cmhc_covered) |>
  summarise(
    `Municipalities with CMHC data` = n(),
    `Median 2020 starts` =
      median(starts_2020_cmhc, na.rm = TRUE),
    `Median 2021 starts` =
      median(starts_2021_cmhc, na.rm = TRUE),
    `Median 2020 starts per 1,000` =
      median(starts_2020_per_1000_clean, na.rm = TRUE)
  )

knitr::kable(
  starts_summary,
  digits = 2,
  caption = "CMHC housing-start summary"
)


# ------------------------------------------------------------------------------
# Starts per 1,000 population
# ------------------------------------------------------------------------------

ggplot(
  eda_data |>
    filter(!is.na(starts_2020_per_1000_clean)),
  aes(x = starts_2020_per_1000_clean)
) +
  geom_histogram(bins = 25) +
  labs(
    x = "2020 housing starts per 1,000 population",
    y = "Number of municipalities",
    title = "Housing-supply activity among CMHC-covered municipalities"
  ) +
  theme_minimal(base_size = 12)


# ------------------------------------------------------------------------------
# 6. Correlations
# ------------------------------------------------------------------------------

correlation_data <- eda_data |>
  select(
    affordability_30plus_pct,
    affordability_50plus_pct,
    population_growth_pct,
    renter_pct,
    median_age,
    median_income_1p,
    population_density,
    average_household_size,
    unsuitable_major_repairs_pct,
    starts_2020_per_1000_clean
  )

correlation_matrix <- cor(
  correlation_data,
  use = "pairwise.complete.obs"
)

knitr::kable(
  round(correlation_matrix, 2),
  caption = "Pairwise Pearson correlations"
)


# ------------------------------------------------------------------------------
# 7. Bivariate relationships with affordability
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# All univariate relationships with 30%+ affordability
# ------------------------------------------------------------------------------

univariate_plot_data <- eda_data |>
  transmute(
    affordability_30plus_pct,
    `Renter share (%)` = renter_pct,
    `Median age (years)` = median_age,
    `Median income ($)` = median_income_1p,
    `Log population density` = log_population_density,
    `Average household size` = average_household_size,
    `Unsuitable + major repairs (%)` = unsuitable_major_repairs_pct,
    `Population growth 2016–2021 (%)` = population_growth_pct,
    `Housing starts per 1,000` = starts_2020_per_1000_clean
  ) |>
  pivot_longer(
    cols = -affordability_30plus_pct,
    names_to = "Predictor",
    values_to = "Value"
  ) |>
  filter(
    !is.na(affordability_30plus_pct),
    !is.na(Value)
  )

univariate_correlations <- univariate_plot_data |>
  group_by(Predictor) |>
  summarise(
    r = cor(
      Value,
      affordability_30plus_pct,
      use = "complete.obs"
    ),
    .groups = "drop"
  ) |>
  mutate(
    facet_label = paste0(
      Predictor,
      "\nr = ",
      sprintf("%.2f", r)
    )
  )

univariate_plot_data <- univariate_plot_data |>
  left_join(
    univariate_correlations,
    by = "Predictor"
  )

ggplot(
  univariate_plot_data,
  aes(
    x = Value,
    y = affordability_30plus_pct
  )
) +
  geom_point(
    alpha = 0.45,
    size = 1.2
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ facet_label,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    x = NULL,
    y = "Households spending ≥30% (%)",
    title = "Univariate relationships with housing affordability burden"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(
      face = "bold",
      size = 9
    ),
    plot.title = element_text(
      face = "bold"
    )
  )

plot_affordability <- function(data, x, x_label) {

  ggplot(
    data,
    aes(
      x = .data[[x]],
      y = affordability_30plus_pct
    )
  ) +
    geom_point(alpha = 0.55) +
    geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    labs(
      x = x_label,
      y = "Households spending ≥30% (%)"
    ) +
    theme_minimal(base_size = 12)
}


# ------------------------------------------------------------------------------
# Renter share
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "renter_pct",
  "Renter households (%)"
)


# ------------------------------------------------------------------------------
# Median age
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "median_age",
  "Median age (years)"
)


# ------------------------------------------------------------------------------
# Median income of one-person households
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "median_income_1p",
  "Median total income of one-person households ($)"
) +
  scale_x_continuous(labels = scales::dollar)


# ------------------------------------------------------------------------------
# Population density
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "log_population_density",
  "log(1 + population density)"
)


# ------------------------------------------------------------------------------
# Average household size
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "average_household_size",
  "Average household size"
)


# ------------------------------------------------------------------------------
# Unsuitable housing and major repairs
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "unsuitable_major_repairs_pct",
  "Not suitable + major repairs needed (%)"
)


# ------------------------------------------------------------------------------
# Population growth
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data,
  "population_growth_pct",
  "Population change, 2016–2021 (%)"
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  )


# ------------------------------------------------------------------------------
# Housing starts per 1,000 population
# ------------------------------------------------------------------------------

plot_affordability(
  eda_data |>
    filter(cmhc_covered),
  "starts_2020_per_1000_clean",
  "2020 housing starts per 1,000 population"
)


# ------------------------------------------------------------------------------
# 8. Municipalities with the highest affordability burden
# ------------------------------------------------------------------------------

highest_affordability <- eda_data |>
  filter(!is.na(affordability_30plus_pct)) |>
  arrange(desc(affordability_30plus_pct)) |>
  transmute(
    Municipality = GEO,
    Population = population_2021,
    `Population growth (%)` = population_growth_pct,
    `30%+ (%)` = affordability_30plus_pct,
    `50%+ (%)` = affordability_50plus_pct,
    `Housing target status` =
      dplyr::coalesce(
        housing_target_status,
        "Not available"
      ),
    `2020 starts per 1,000` =
      starts_2020_per_1000_clean
  ) |>
  slice_head(n = 10)

knitr::kable(
  highest_affordability,
  digits = 1,
  caption = "Ten CSDs with the highest 30%+ affordability burden"
)


# ------------------------------------------------------------------------------
# 9. Housing target status
# ------------------------------------------------------------------------------

target_status <- eda_data |>
  filter(!is.na(housing_target_status)) |>
  count(
    housing_target_status,
    name = "Municipalities"
  ) |>
  arrange(desc(Municipalities))

knitr::kable(
  target_status,
  caption = "Housing target status among municipalities with target information"
)


# ------------------------------------------------------------------------------
# 10. Initial EDA observations
# ------------------------------------------------------------------------------
