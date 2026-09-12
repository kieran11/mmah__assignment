# mmah__assignment

1.  The first step is to get the Statistics Canada 2021 data. You can run the script `01_get_stats_can_data.R`. Note, the script uses the `helper_functions.R`. 
  a.  This creates two statistics canada files (These datasets are two large to push to github): 
    i.  98-401-X2021021_English_CSV_data.csv - Statistics Canada demographic and income data at the census division level.
    ii. 98100247.csv  - Details of the census division levels in Ontario.
2.  After we've downloaded the data, run `02_build_affordability_ds.R` to create the analytic dataset called `final_analytic_dataset.csv`. 
3.  We then run the `03_eda_housing_affordability.R`. This doesn't create any of the datasets, but the code here is reused in the presentation.
4. The `04_regression_analysis_updated_with_cv.R` script runs the regressions themselves. It uses the `/data/processed/final_analytic_dataset.csv` as an input. It creates two files. 
  1.  `data/processed/rmse_log_results.csv` which contain the root mean square results for the models. 
  2.  The regression model and averge marginal effect models `/data/processed/avg_marg_effects.csv`
5. The `presentation_mmah.qmd` creates the presentation itself. 