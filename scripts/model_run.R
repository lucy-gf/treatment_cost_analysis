## RUNNING MODEL ##

## splitting into two

use_hosp <- costs_gdp[outcome=='hosp']
use_outp <- costs_gdp[outcome=='outp']

## brms

lm_hosp <- brms::brm(log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap)) + hce_prop_gdp), 
                     # log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap))) + (1|iso3c), 
                     data = use_hosp,
                     family = gaussian(),
                     chains = 3, cores = 3, 
                     iter = 4000,
                     control = list(max_treedepth = 20))

lm_outp <- brms::brm(#log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap)) + hce_prop_gdp) + (1|iso3c), 
                     log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap)) + hce_prop_gdp), 
                     data = use_outp,
                     family = gaussian(),
                     chains = 3, cores = 3, 
                     iter = 4000,
                     control = list(max_treedepth = 20))

summary(lm_hosp)
summary(lm_outp)
pairs(lm_hosp)
pairs(lm_outp)
brms::conditional_effects(lm_hosp) 
brms::conditional_effects(lm_outp) 
# pairs(lm)

conditions <- data.frame(study_pop = unique(use_hosp$study_pop))
facet_output <- brms::conditional_effects(lm_hosp, conditions = conditions,
                                          method = "posterior_epred")
plot(facet_output, points = F)


exp_out_hosp <- data.table(exp(posterior_epred(lm_hosp,
                                               ndraws = 3000)))

exp_out_hosp_l <- suppressWarnings(melt.data.table(exp_out_hosp))
exp_out_hosp_l[, id := as.numeric(gsub('V','',variable))]
exp_out_hosp_l[, variable := NULL]
exp_out_hosp_l[, sample_id := rep(1:(nrow(exp_out_hosp_l)/n_distinct(exp_out_hosp_l$id)), n_distinct(exp_out_hosp_l$id))]

use_hosp_no_na <- if(grepl('hce_prop_gdp', lm_hosp$formula$formula[3])){
    use_hosp[!is.na(hce_prop_gdp)]
  }else{
    use_hosp
  }
use_hosp_no_na[, id := 1:nrow(use_hosp_no_na)]

exp_out_hosp_l <- exp_out_hosp_l[use_hosp_no_na[, c('id','iso3c','study_year','study_pop','outcome','cost_usd_main_yr','gdpcap','hce_prop_gdp')], on = 'id']

exp_out_hosp_l %>% 
  group_by(id, iso3c, study_year, study_pop, cost_usd_main_yr, gdpcap, hce_prop_gdp) %>% 
  summarise(med = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  ggplot() + 
  geom_point(aes(x = gdpcap, y = cost_usd_main_yr, col = iso3c), shape = 4, size = 2) + 
  geom_point(aes(x = gdpcap, y = med, group = id, col = iso3c), size = 2) +
  geom_errorbar(aes(x = gdpcap, ymin = lower, ymax = upper, group = id, col = iso3c), alpha = 0.4) +
  facet_grid(study_pop~., scales = 'free') + 
  theme_bw() + scale_x_log10() + 
  ggtitle('Hospitalisation costs')

exp_out_outp <- data.table(exp(posterior_epred(lm_outp,
                                               ndraws = 3000)))

exp_out_outp_l <- suppressWarnings(melt.data.table(exp_out_outp))
exp_out_outp_l[, id := as.numeric(gsub('V','',variable))]
exp_out_outp_l[, variable := NULL]
exp_out_outp_l[, sample_id := rep(1:(nrow(exp_out_outp_l)/n_distinct(exp_out_outp_l$id)), n_distinct(exp_out_outp_l$id))]

use_outp_no_na <- if(grepl('hce_prop_gdp', lm_outp$formula$formula[3])){
  use_outp[!is.na(hce_prop_gdp)]
}else{
  use_outp
}
use_outp_no_na[, id := 1:nrow(use_outp_no_na)]

exp_out_outp_l <- exp_out_outp_l[use_outp_no_na[, c('id','iso3c','study_year','study_pop','outcome','cost_usd_main_yr','gdpcap','hce_prop_gdp')], on = 'id']

exp_out_outp_l %>% 
  filter(!is.na(value)) %>% # unsure why some are NA
  group_by(id, iso3c, study_year, study_pop, cost_usd_main_yr, gdpcap, hce_prop_gdp) %>% 
  summarise(med = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>% 
  ggplot() + 
  geom_point(aes(x = gdpcap, y = cost_usd_main_yr, col = iso3c), shape = 4, size = 2) + 
  geom_point(aes(x = gdpcap, y = med, group = id, col = iso3c), size = 2) +
  geom_errorbar(aes(x = gdpcap, ymin = lower, ymax = upper, group = id, col = iso3c), alpha = 0.4) +
  facet_grid(study_pop~., scales = 'free') + 
  theme_bw() + scale_x_log10() + 
  ggtitle('Outpatient costs')





