
library(dplyr)
library(readr)
library(stringr)

# ---- Paths -------------------------------------------------------------------


# ==============================================================================
# 1. LOCATE 2021 STATISTICS CANADA TABLE
# ==============================================================================
script_path <- "H:/github_repos/Assignment_MMAH/R/"
raw_data_path <- "C:/Users/shahki/Downloads/"
processed_data_path <- "H:/github_repos/Assignment_MMAH/data/processed/"

source(paste0(script_path, "helper_functions.R"))

stats_can_path <-paste0(raw_data_path, "98100247/98100247.csv" )

stats_can_2021 <- data.table::fread(stats_can_path)
 


analysis_2021 <- stats_can_2021[
  str_starts(DGUID, "2021A0005")][
  str_starts(str_sub(DGUID, -7), "35") # Ontario CSD codes begin with 35
  ][
    `Shelter-cost-to-income ratio (9)` %in% c(
      "Total - Shelter-cost-to-income ratio",
      "30% to less than 50%",
      "50% or more"
    )
  ][
    `Statistics (3C)` == "Number of private households"
  ]

data.table::setnames(analysis_2021, old = c(
                     "Shelter-cost-to-income ratio (9)",
                     "Dwelling condition (4)",
                     "Housing suitability (3)",
                     "Core housing need (5)",
                    "Tenure including presence of mortgage payments and subsidized housing (8):Total - Tenure including presence of mortgage payments and subsidized housing[1]"), 
                    new = c("shelter_cost_income_ratio",
                            "dwelling_condition", 
                            "housing_suitability", 
                            "core_housing_need", 
                            "tenure_total"))

analysis_2021_subset <- analysis_2021[,.(GEO, 
                        DGUID, 
                        shelter_cost_income_ratio,
                        dwelling_condition,
                        housing_suitability,
                        core_housing_need,
                        tenure_total
                        )] %>% 
  dplyr::filter(
    core_housing_need == "Total - Core housing need",
    dwelling_condition == "Total - Dwelling condition",
    housing_suitability == "Total - Housing suitability"
  ) %>%
  dplyr::select(
    GEO,
    DGUID,
    shelter_cost_income_ratio,
    tenure_total
  ) %>%
  tidyr::pivot_wider(
    names_from = shelter_cost_income_ratio,
    values_from = tenure_total
  ) %>% 
  dplyr::mutate(
    affordability_30plus =
      100 *
      (
        `30% to less than 50%` +
          `50% or more`
      ) /
      `Total - Shelter-cost-to-income ratio`,
    
    affordability_50plus =
      100 *
      `50% or more` /
      `Total - Shelter-cost-to-income ratio`
  )


#### Predictors: 

full_predictor_set <- data.table::fread(
  "C:/Users/shahki/Downloads/98-401-X2021021_English_CSV_data.csv",
  encoding = "Latin-1"
)
full_predictor_set <- full_predictor_set[, .(DGUID, ALT_GEO_CODE, GEO_NAME, CHARACTERISTIC_NAME, TNR_SF, TNR_LF, C1_COUNT_TOTAL, C10_RATE_TOTAL )]

full_predictor_set %>% 
  dplyr::filter(grepl( "Population, 2021" , CHARACTERISTIC_NAME, ignore.case = T)) %>% 
  dplyr::count(CHARACTERISTIC_NAME) %>% 
  pull(CHARACTERISTIC_NAME)

subset_predictor_set <- full_predictor_set[CHARACTERISTIC_NAME %in% c("  Renter" ,
                                             "Median age of the population",
                                             "      Median total income of one-person households in 2020 ($)",
                                             "Population density per square kilometre",
                                             "Average household size" , 
                                             "    'Not suitable' and 'major repairs needed'",
                                             "Population percentage change, 2016 to 2021",
                                             "Population, 2021") &
                                             !grepl("Indian reserve", GEO_NAME)] %>% 
  .[, value := data.table::fifelse(
    CHARACTERISTIC_NAME %in% c(
      "    'Not suitable' and 'major repairs needed'",
      "  Renter"),
  C10_RATE_TOTAL,
  C1_COUNT_TOTAL)
] %>% 
  dplyr::select(-C10_RATE_TOTAL, -C10_RATE_TOTAL) %>% 
  tidyr::pivot_wider(
    id_cols = c(
      DGUID,
      ALT_GEO_CODE,
      GEO_NAME,
      TNR_SF,
      TNR_LF
    ),
    names_from = CHARACTERISTIC_NAME,
    values_from = value
  )
    

### additional field linkages. 

housing_starts_2021 <- data.table::fread(
  paste0(processed_data_path, "ontario_housing_starts_2020_2021.csv" )
  ) %>% 
  mutate(simplified_city_nm = clean_municipality(municipality)) 

housing_targets <- data.table::fread(
  paste0(processed_data_path, "ontarios_housing_supply_-_january_december_2023_-_en.csv" )
  ) %>%
  mutate(simplified_city_nm = clean_municipality(Municipality )) %>% 
  dplyr::select(simplified_city_nm, `Housing Target Status`)  
  

final_analytic_set <- analysis_2021_subset %>% 
  left_join(subset_predictor_set, by = "DGUID") %>% 
  mutate(simplified_city_nm =clean_municipality(GEO )) %>% 
  left_join(housing_starts_2021, by = "simplified_city_nm") %>% 
  left_join(housing_targets, by = "simplified_city_nm") %>% 
  mutate_at(c("starts_2020","starts_2021"), ~ifelse(is.na(.),0,.))

data.table::fwrite(final_analytic_set, 
                   paste0(processed_data_path,"final_analytic_dataset.csv")
)