
#### RUNNING MODEL ####

interval_probs <- c((1 - interval_width)/2, 1 - (1 - interval_width)/2)

cat('\nConf. interval width = ', 100*interval_width, '%\n', sep = '')

## splitting into two models/datasets for hospitalisation vs outpatient

costs_gdp <- costs_gdp %>% 
  mutate(treatment_type = outcome)

## formulae to run
 
# 1. no interaction, random intercepts
gdp_f1 <- bf(cost_usd_main_yr ~ 0 + log(gdpcap) + treatment_type + study_pop + (1 | iso3c))
# 2. no interaction, random slopes
gdp_f2 <- bf(cost_usd_main_yr ~ 0 + log(gdpcap) + (1 + treatment_type + study_pop | iso3c))
# 3. interaction, no random slopes
gdp_f3 <- bf(cost_usd_main_yr ~ 0 + log(gdpcap):treatment_type + log(gdpcap):study_pop + (1 | iso3c))
# 4. interaction, random slopes
gdp_f4 <- bf(cost_usd_main_yr ~ 0 + log(gdpcap):treatment_type + log(gdpcap):study_pop + 
     (1 + treatment_type + study_pop | iso3c))
# 5. interaction, random slopes, no age
gdp_f5 <- bf(cost_usd_main_yr ~ 0 + log(gdpcap):treatment_type + 
               (1 + treatment_type | iso3c))

## THEN WITH HCEPC INSTEAD OF GDPPC

# 1. no interaction, random intercepts
hce_f1 <- bf(cost_usd_main_yr ~ 0 + log(hce_cap) + treatment_type + study_pop + (1 | iso3c))
# 2. no interaction, random slopes
hce_f2 <- bf(cost_usd_main_yr ~ 0 + log(hce_cap) + (1 + treatment_type + study_pop | iso3c))
# 3. interaction, no random slopes
hce_f3 <- bf(cost_usd_main_yr ~ 0 + log(hce_cap):treatment_type + log(hce_cap):study_pop + (1 | iso3c))
# 4. interaction, random slopes
hce_f4 <- bf(cost_usd_main_yr ~ 0 + log(hce_cap):treatment_type + log(hce_cap):study_pop + 
            (1 + treatment_type + study_pop | iso3c))
# 5. interaction, random slopes, no age
hce_f5 <- bf(cost_usd_main_yr ~ 0 + log(hce_cap):treatment_type + 
               (1 + treatment_type | iso3c))

########

formulas <- list(gdp_f1, gdp_f2, gdp_f3, gdp_f4, gdp_f5,
                 hce_f1, hce_f2, hce_f3, hce_f4, hce_f5)
names(formulas) <- c('GDP_no_interaction_rand_intercept','GDP_no_interaction_rand_slope',
                     'GDP_interaction_rand_intercept','GDP_interaction_rand_slope','GDP_interaction_rand_slope_no_age',
                     'HCE_no_interaction_rand_intercept','HCE_no_interaction_rand_slope',
                     'HCE_interaction_rand_intercept','HCE_interaction_rand_slope','HCE_interaction_rand_slope_no_age')

#### RUN BRMS ####
# using Gamma(link = "log") to ensure cost >= 0

lms <- map(
  .x = formulas,
  .f = ~{brms::brm(
    formula = .x,
    data = costs_gdp %>% drop_na(),
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

loos_ <- map(
  .x = lms,
  .f = loo
)

loos_scores <- loo_compare(loos_)

# print scores
cat('------------------\nLOO compare:\n');loos_scores

write_csv(data.frame(model = rownames(loos_scores), elpd_diff = round(loos_scores[,1],2)),
          here::here('output','LOO_compare.csv'))

# pick best model
pref_model_name <- rownames(loos_scores)[1]

pref_model <- lms[[pref_model_name]]

#### PREDICTED COSTS ####

# plot predictions against observed data
pred_obs <- fitted(pref_model, newdata = costs_gdp %>% drop_na(), probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(costs_gdp %>% drop_na()) %>% 
  mutate(effects = 'rand_eff') %>% 
  rbind(fitted(pref_model, newdata = costs_gdp %>% drop_na(), re_formula = NA, probs = interval_probs) %>% 
          as_tibble() %>% mutate(effects = 'no_rand_eff') %>% bind_cols(costs_gdp %>% drop_na())) %>% 
  drop_na() 

newdata_hce <- expand.grid(
  hce_cap = seq(min(costs_gdp$hce_cap, na.rm = TRUE),
                max(costs_gdp$hce_cap, na.rm = TRUE),
                length.out = 3000),
  study_pop = unique(costs_gdp$study_pop),
  treatment_type = unique(costs_gdp$treatment_type)
)

newdata_gdp <- expand.grid(
  gdpcap = seq(min(costs_gdp$gdpcap, na.rm = TRUE),
               max(costs_gdp$gdpcap, na.rm = TRUE),
               length.out = 3000),
  study_pop = unique(costs_gdp$study_pop),
  treatment_type = unique(costs_gdp$treatment_type)
)

pred_hce <- predict(pref_model, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)

pred_hce_all_models_list <- map(
  .x = 1:length(lms),
  .f = ~{
    predict(lms[[.x]], newdata = if(grepl('HCE', names(lms)[.x])){newdata_hce}else{newdata_gdp}, 
            re_formula = NA, probs = interval_probs) %>%
      as_tibble() %>%
      bind_cols(if(grepl('HCE', names(lms)[.x])){newdata_hce}else{newdata_gdp}) %>% 
      mutate(model = names(lms)[.x])
  }
)

pred_hce_all_models <- rbindlist(pred_hce_all_models_list)





