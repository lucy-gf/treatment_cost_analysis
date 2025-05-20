## RUNNING MODEL ##

## splitting into two

use_hosp <- costs_gdp[outcome=='hosp']
use_outp <- costs_gdp[outcome=='outp']

## brms

# TODO - is lognormal correct?
# TODO - is n_chain = 3 good?

lm_hosp <- brms::brm(log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap)) + hce_prop_gdp) + (1|iso3c), 
                     data = use_hosp,
                     family = gaussian(),
                     chains = 3, cores = 3, iter = 4000)

lm_outp <- brms::brm(log(cost_usd_main_yr) ~ study_pop*(log(log(gdpcap)) + hce_prop_gdp) + (1|iso3c), 
                     data = use_outp,
                     family = gaussian(),
                     chains = 3, cores = 3, iter = 4000)

summary(lm_hosp)
summary(lm_outp)
brms::conditional_effects(lm_hosp) 
brms::conditional_effects(lm_outp) 
# pairs(lm)

conditions <- data.frame(study_pop = unique(use_hosp$study_pop))
facet_output <- brms::conditional_effects(lm_hosp, conditions = conditions,
                                          re_formula = NULL, method = "predict")
plot(facet_output, points = TRUE)

ggplot(data.table(facet_output[[4]])) + 
  geom_ribbon(aes(x=gdpcap, ymin=lower__, ymax=upper__, group=study_pop), alpha=0.2) +
  geom_line(aes(gdpcap, estimate__, col=study_pop), lwd=1) +
  scale_x_log10() + scale_y_log10() +
  facet_grid(cond__~.) + 
  theme_bw() + scale_color_manual(values = age_colors)

ggplot(data.table(facet_output[[5]])) + 
  geom_ribbon(aes(x=hce_prop_gdp, ymin=lower__, ymax=upper__, group=study_pop), alpha=0.2) +
  geom_line(aes(hce_prop_gdp, estimate__, col=study_pop), lwd=1) +
  scale_x_log10() + scale_y_log10() +
  facet_grid(cond__~.) + 
  theme_bw() + scale_color_manual(values = age_colors)











