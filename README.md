# mmah__assignment

1.  The first step is to get the Statistics Canada 2021 data. You can run the script `01_get_stats_can_data.R`. Note, the script uses the `helper_functions.R`. 
  a.  This creates two statistics canada files (These datasets are two large to push to github): 
    i.  98-401-X2021021_English_CSV_data.csv - Statistics Canada demographic and income data at the census division level.
    ii. 98100247.csv  - Details of the census division levels in Ontario.
2.  After we've downloaded the data, run `02_build_affordability_ds.R` to create the analytic dataset called `final_analytic_dataset.csv`