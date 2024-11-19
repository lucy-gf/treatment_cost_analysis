# treatment_cost_analysis

## [WIP] 

## Meta-analysis of national age-specific influenza treatment costs

Using the `brms` R package to produce a bayesian metaregression of national age-specific influenza hospitalisation and outpatient costs using data from systematic reviews and meta-analyses.

Predictors: GDP per capita (USD 2022(?)) and Healthcare expenditure per capita as a proportion of GDP per capita.

Both using the `WDI` R package (World Development Indicators).

### Data in `/data/`

- `OLD_DATA.csv` contains old data

### Scripts in `/scripts/`

- `/setup/`:
  - `packages.R` for relevant package installs
  - `aesthetics.R` for ggplot etc. presets
- `init.R` for running analysis
- `data_cleaning.R` for adding WDI indicators to data etc.
- `model_run.R` for running `brms` model
- `plots.R` for creating and saving figures

