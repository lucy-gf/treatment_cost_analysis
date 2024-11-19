## TESTING BRMS PACKAGE 19/11 ##

# initialising
library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','aesthetics.R'))

# loading data
data <- data.table(read_csv(here::here('data','OLD_DATA.csv'), show_col_types = F))[,1:14] %>% filter(!is.na(country))

# cleaning data
source(here::here('scripts','data_cleaning.R'))

use <- costs_gdp[, c('iso3c','study_pop','outcome','cost_usd_2022','gdpcap')]

ggplot(use) + 
  geom_point(aes(x=gdpcap, y=cost_usd_2022, col=study_pop)) + 
  facet_grid(outcome~study_pop, scales='free') + 
  theme_bw() + scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = age_colors)

## splitting into two

use_hosp <- use[outcome=='hospital']
use_outp <- use[outcome=='outpatient']

## brms

# TODO - is lognormal correct?
# TODO - is n_chain = 3 good?

lm_hosp <- brms::brm(cost_usd_2022 ~ study_pop*gdpcap + (1|iso3c), 
          data = use_hosp,
          family = lognormal('identity', link_sigma='log'),
          chains = 3, cores = 3, iter = 4000)

lm_outp <- brms::brm(cost_usd_2022 ~ study_pop*gdpcap + (1|iso3c), 
                     data = use_outp,
                     family = lognormal('identity', link_sigma='log'),
                     chains = 3, cores = 3, iter = 4000)

summary(lm_hosp)
summary(lm_outp)
brms::conditional_effects(lm_hosp) 
brms::conditional_effects(lm_outp) 
# pairs(lm)

conditions <- data.frame(study_pop = unique(use$study_pop))
facet_output <- brms::conditional_effects(lm_outp, conditions = conditions,
                                    re_formula = NULL, method = "predict")
plot(facet_output, points = TRUE)

output <- data.table(facet_output[[3]])

ggplot(output) + 
  geom_ribbon(aes(x=gdpcap, ymin=lower__, ymax=upper__), alpha=0.3) +
  geom_line(aes(gdpcap, estimate__), lwd=0.8, col='red') +
  facet_grid(study_pop~., scales='free') +
  # scale_x_log10() + scale_y_log10() +
  theme_bw()

ggplot(output) + 
  geom_ribbon(aes(x=gdpcap, ymin=lower__, ymax=upper__, group=study_pop), alpha=0.2) +
  geom_line(aes(gdpcap, estimate__, col=study_pop), lwd=1) +
  scale_x_log10() + scale_y_log10() +
  facet_grid(cond__~.) + 
  theme_bw() + scale_color_manual(values = age_colors)


