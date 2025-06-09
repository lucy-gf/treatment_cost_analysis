
#### RUNNING MODEL ####

interval_probs <- c((1 - interval_width)/2, 1 - (1 - interval_width)/2)

cat('\nConf. interval width = ', 100*interval_width, '%\n', sep = '')

## splitting into two models/datasets for hospitalisation vs outpatient

use_hosp <- costs_gdp[outcome=='hosp']
use_outp <- costs_gdp[outcome=='outp']

formula_gdppc <- 
  bf(cost_usd_main_yr ~ 0 + log(gdpcap):study_pop + (1 | iso3c))
formula_gdppc_hceprop <- 
  bf(cost_usd_main_yr ~ 0 + log(gdpcap):study_pop + hce_prop_gdp:study_pop + (1 | iso3c))
formula_gdppc_hcepc <- 
  bf(cost_usd_main_yr ~ 0 + log(gdpcap):study_pop + log(hce_cap):study_pop + (1 | iso3c))
formula_hcepc <- 
  bf(cost_usd_main_yr ~ 0 + log(hce_cap):study_pop + (1 | iso3c))
formula_hceprop <- 
  bf(cost_usd_main_yr ~ 0 + hce_prop_gdp:study_pop + (1 | iso3c))

formulas <- list(formula_gdppc,
                 formula_gdppc_hceprop,
                 formula_gdppc_hcepc,
                 formula_hcepc,
                 formula_hceprop)
names(formulas) <- c('gdppc','gdppc_hceprop','gdppc_hcepc','hcepc','hceprop')

#### RUN BRMS ####
# using Gamma(link = "log") to ensure cost >= 0

## hospitalisations
lms_hosp <- map(
  .x = formulas,
  .f = ~{brms::brm(
    formula = .x,
    data = use_hosp %>% drop_na(),
    family = Gamma(link = "log"),
    chains = 3, cores = 3, 
    iter = 4000,
    control = list(max_treedepth = 20)
  )}
)

## outpatients
lms_outp <- map(
  .x = formulas,
  .f = ~{brms::brm(
    formula = .x,
    data = use_outp %>% drop_na(),
    family = Gamma(link = "log"),
    chains = 3, cores = 3, 
    iter = 4000,
    control = list(max_treedepth = 20)
  )}
)

#### MODEL COMPARISONS #### 

# loo_compare() reports the difference in expected log predictive density (elpd) between models
# a positive difference favors the first model
# differences larger than about 4–5 are considered meaningful

## hospitalisations

loos_hosp <- map(
  .x = lms_hosp,
  .f = loo
)

loos_hosp_scores <- loo_compare(loos_hosp)

## outpatients

loos_outp <- map(
  .x = lms_outp,
  .f = loo
)

loos_outp_scores <- loo_compare(loos_outp)

# print scores
cat('------------------\nHospitalisation LOO compare:\n');loos_hosp_scores;cat('------------------\nOutpatient LOO compare:\n');loos_outp_scores

# pick best model
pref_hosp_model_name <- rownames(loos_hosp_scores)[1]
pref_outp_model_name <- rownames(loos_outp_scores)[1]

hosp_model <- lms_hosp[[pref_hosp_model_name]]
outp_model <- lms_outp[[pref_outp_model_name]]

#### PREDICTED COSTS ####

# # fix hce_prop_gdp at median (or some value)
# fixed_HCE <- median(costs_gdp$hce_prop_gdp, na.rm = TRUE)
# 
# # fix gdpcap at median (or some value)
# fixed_gdpcap <- median(costs_gdp$gdpcap, na.rm = TRUE)
# 
# # Create grid for GDPpc varying, HCE fixed
# newdata_gdp <- expand.grid(
#   gdpcap = seq(min(costs_gdp$gdpcap, na.rm = TRUE), 
#                max(costs_gdp$gdpcap, na.rm = TRUE), 
#                length.out = 100),
#   hce_prop_gdp = fixed_HCE,
#   study_pop = unique(costs_gdp$study_pop)
# )
# 
# # Create grid for HCE varying, GDPpc fixed
# newdata_hce <- expand.grid(
#   gdpcap = fixed_gdpcap,
#   hce_prop_gdp = seq(min(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
#                      max(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
#                      length.out = 100),
#   study_pop = unique(costs_gdp$study_pop)
# )
# 
# # Create grid with both HCE and GDPpc varying
# tile_data <- expand.grid(
#   gdpcap = seq(min(costs_gdp$gdpcap, na.rm = TRUE), 
#                max(costs_gdp$gdpcap, na.rm = TRUE), 
#                length.out = 100),
#   hce_prop_gdp = seq(min(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
#                      max(costs_gdp$hce_prop_gdp, na.rm = TRUE), 
#                      length.out = 100),
#   study_pop = unique(costs_gdp$study_pop)
# )

newdata_hce <- expand.grid(
  hce_cap = seq(min(costs_gdp$hce_cap, na.rm = TRUE),
                     max(costs_gdp$hce_cap, na.rm = TRUE),
                     length.out = 3000),
  study_pop = unique(costs_gdp$study_pop)
)

#### PRED: HOSPITALISATION ####

# pred_gdp_hosp <- predict(hosp_model, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
#   as_tibble() %>%
#   bind_cols(newdata_gdp)

pred_hce_hosp <- predict(hosp_model, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)

# pred_tile_hosp <- predict(hosp_model, newdata = tile_data, re_formula = NA, probs = interval_probs) %>%
#   as_tibble() %>%
#   bind_cols(tile_data)


#### PRED: OUTPATIENT ####

# pred_gdp_outp <- predict(outp_model, newdata = newdata_gdp, re_formula = NA, probs = interval_probs) %>%
#   as_tibble() %>%
#   bind_cols(newdata_gdp)

pred_hce_outp <- predict(outp_model, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)

# pred_tile_outp <- predict(outp_model, newdata = tile_data, re_formula = NA, probs = interval_probs) %>%
#   as_tibble() %>%
#   bind_cols(tile_data)










