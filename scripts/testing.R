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

## brms

lm <- brm(cost_usd_2022 ~ study_pop + outcome + gdpcap, 
          data = use,
          family = lognormal('identity', link_sigma='log'),
          chains = 3, cores = 3)

# throws errors..:

# Warning messages:
#   1: There were 2860 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# 3: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 

summary(lm)
conditional_effects(lm) # looks about right
pairs(lm)

conditions <- unique(use[,c('study_pop','outcome')])
facet_output <- conditional_effects(lm, conditions = conditions,
                                    re_formula = NULL, method = "predict")
plot(facet_output, points = TRUE)

output <- facet_output[[3]]

ggplot(output) + 
  geom_ribbon(aes(x=gdpcap, ymin=lower__, ymax=upper__), alpha=0.3) +
  geom_line(aes(gdpcap, estimate__), lwd=0.8, col='red') +
  facet_grid(outcome~study_pop, scales='free') +
  # scale_x_log10() + scale_y_log10() +
  theme_bw()

ggplot(output) + 
  geom_ribbon(aes(x=gdpcap, ymin=lower__, ymax=upper__, group=study_pop), alpha=0.2) +
  geom_line(aes(gdpcap, estimate__, col=study_pop), lwd=1) +
  facet_grid(outcome~., scales='free') +
  scale_x_log10() + scale_y_log10() +
  theme_bw() + scale_color_manual(values = age_colors)

# more similar than i was expecting across study_pops..?

