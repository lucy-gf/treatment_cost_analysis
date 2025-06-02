
#### RUNNING MODEL ####

interval_width <- 0.5

interval_probs <- c((1 - interval_width)/2, 1 - (1 - interval_width)/2)

cat('\nConf. interval width = ', 100*interval_width, '%\n', sep = '')

## splitting into two models/datasets for hospitalisation vs outpatient

use_hosp <- costs_gdp[outcome=='hosp']
use_outp <- costs_gdp[outcome=='outp']

formula <- bf(cost_usd_main_yr ~ 0 + log(gdpcap):study_pop + hce_prop_gdp:study_pop + (1 | iso3c))

#### RUN BRMS x2 ####
# using Gamma(link = "log") to ensure cost >= 0

lm_hosp <- brms::brm(
  formula = formula,
  data = use_hosp,
  family = Gamma(link = "log"),
  chains = 3, cores = 3, 
  iter = 4000,
  control = list(max_treedepth = 20)
  )

lm_outp <- brms::brm(
  formula = formula,
  data = use_outp,
  family = Gamma(link = "log"),
  chains = 3, cores = 3, 
  iter = 4000,
  control = list(max_treedepth = 20)
)

#### MODEL COMPARISONS (TODO) #### 

loo_with_hce <- loo(lm_hosp)
# TODO then do again without hce
# then compare using loo_compare(loo_with_hce, loo_without_hce)
# loo_compare() reports the difference in expected log predictive density (elpd) between models.
# A positive difference favors the first model (fit_with_HCE).
# Differences larger than about 4–5 are considered meaningful.
# Also check standard errors reported by loo_compare().


#### PREDICTED COSTS ####

# fix hce_prop_gdp at median (or some value)
fixed_HCE <- median(costs_gdp$hce_prop_gdp, na.rm = TRUE)

# fix gdpcap at median (or some value)
fixed_gdpcap <- median(costs_gdp$gdpcap, na.rm = TRUE)

# Create grid for GDPpc varying, HCE fixed
newdata_gdp <- expand.grid(
  gdpcap = seq(min(costs_gdp$gdpcap, na.rm = TRUE), 
               max(costs_gdp$gdpcap, na.rm = TRUE), 
               length.out = 100),
  hce_prop_gdp = fixed_HCE,
  study_pop = unique(costs_gdp$study_pop)
)

# Create grid for HCE varying, GDPpc fixed
newdata_hce <- expand.grid(
  gdpcap = fixed_gdpcap,
  hce_prop_gdp = seq(min(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
                     max(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
                     length.out = 100),
  study_pop = unique(costs_gdp$study_pop)
)

# Create grid with both HCE and GDPpc varying
tile_data <- expand.grid(
  gdpcap = seq(min(costs_gdp$gdpcap, na.rm = TRUE), 
               max(costs_gdp$gdpcap, na.rm = TRUE), 
               length.out = 100),
  hce_prop_gdp = seq(min(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
                     max(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
                     length.out = 100),
  study_pop = unique(costs_gdp$study_pop)
)

#### PRED: HOSPITALISATION ####

pred_gdp_hosp <- predict(lm_hosp, newdata = newdata_gdp, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_gdp)

pred_hce_hosp <- predict(lm_hosp, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)

pred_tile_hosp <- predict(lm_hosp, newdata = tile_data, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(tile_data)


#### PRED: OUTPATIENT ####

pred_gdp_outp <- predict(lm_outp, newdata = newdata_gdp, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_gdp)

pred_hce_outp <- predict(lm_outp, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)

pred_tile_outp <- predict(lm_outp, newdata = tile_data, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(tile_data)










