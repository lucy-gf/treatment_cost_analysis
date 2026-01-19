# treatment_cost_analysis

## Meta-analysis of national age-specific influenza treatment costs

Using the `brms` R package to produce a bayesian metaregression of national age-specific influenza hospitalisation and outpatient costs using data from systematic reviews and meta-analyses.

Predictors: GDP per capita or Healthcare expenditure per capita (both tested using `loo_compare()`)

Both using the `WDI` R package (World Development Indicators).

### Data in `/data/`

- `FINAL_Merged_Flu_Cost_Data.xlsx` contains the umbrella review data used for model fitting

### Scripts in `/scripts/`

- `/setup/`:
  - `packages.R` for relevant package installs
  - `aesthetics.R` for ggplot etc. presets
- `init.R` for running analysis
- `data_cleaning.R` for adding WDI indicators to data etc.
- `model_run.R` for running `brms` model
- `plots.R` for creating and saving figures
- `make_outputs.R` to produce estimates of hospitalisation and outpatient costs in each age group, in 190 countries and territories
