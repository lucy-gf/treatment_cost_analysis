
## RUNNING MODEL ##

interval_width <- 0.5

interval_probs <- c((1 - interval_width)/2, 1 - (1 - interval_width)/2)

cat('\nConf. interval width = ', 100*interval_width, '%\n', sep = '')

## splitting into two models/datasets for hospitalisation vs outpatient

use_hosp <- costs_gdp[outcome=='hosp']
use_outp <- costs_gdp[outcome=='outp']

formula <- bf(cost_usd_main_yr ~ 0 + log(gdpcap):study_pop + hce_prop_gdp:study_pop + (1 | iso3c))

## run both brms
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

# plot predictions against observed data
fitted(lm_hosp, newdata = use_hosp, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(use_hosp) %>% 
  mutate(outcome = 'hosp') %>% 
  rbind(fitted(lm_outp, newdata = use_outp, re_formula = NA, probs = interval_probs) %>%
          as_tibble() %>%
          bind_cols(use_outp) %>% 
          mutate(outcome = 'outp')) %>% 
  ggplot(aes(x = gdpcap, col = iso3c)) +
  geom_point(aes(y = cost_usd_main_yr), 
             shape = 4) +
  geom_point(aes(y = Estimate)) +
  geom_errorbar(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2])))) + 
  labs(x = "GDP per capita", y = "Treatment cost") +
  scale_x_log10() + 
  theme_bw() + 
  facet_grid(outcome ~ study_pop, scales = 'free')


## prediction plots - TODO move these into plots.R

# fix hce_prop_gdp at median (or some value)
fixed_HCE <- median(use_hosp$hce_prop_gdp, na.rm = TRUE)

# fix gdpcap at median (or some value)
fixed_gdpcap <- median(use_hosp$gdpcap, na.rm = TRUE)

# Create grid for GDPpc varying, HCE fixed
newdata_gdp <- expand.grid(
  gdpcap = seq(min(use_hosp$gdpcap, na.rm = TRUE), 
               max(use_hosp$gdpcap, na.rm = TRUE), 
               length.out = 100),
  hce_prop_gdp = fixed_HCE,
  study_pop = unique(use_hosp$study_pop)
)

# Create grid for HCE varying, GDPpc fixed
newdata_hce <- expand.grid(
  gdpcap = fixed_gdpcap,
  hce_prop_gdp = seq(min(use_hosp$hce_prop_gdp, na.rm = TRUE), 
            max(use_hosp$hce_prop_gdp, na.rm = TRUE), 
            length.out = 100),
  study_pop = unique(use_hosp$study_pop)
)

pred_gdp <- predict(lm_hosp, newdata = newdata_gdp, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_gdp)

pred_hce <- predict(lm_hosp, newdata = newdata_hce, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(newdata_hce)


# plot hosp_cost ~ GDPpc for each study_pop
p1 <- ggplot(pred_gdp, aes(x = gdpcap, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted cost by log GDPpc (HCE fixed)",
    x = "GDP per capita",
    y = "Predicted cost",
    fill = 'Age group',
    color = 'Age group'
  ) + scale_x_log10() + 
  theme_minimal() + 
  scale_fill_manual(values = age_colors) + 
  scale_color_manual(values = age_colors)

# plot hosp_cost ~ HCE for each study_pop
p2 <- ggplot(pred_hce, aes(x = hce_prop_gdp, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted cost by HCE (GDPpc fixed)",
    x = "Healthcare expenditure as a proportion of GDP per capita",
    y = "Predicted cost",
    fill = 'Age group',
    color = 'Age group'
  ) +
  theme_minimal() + 
  scale_fill_manual(values = age_colors) + 
  scale_color_manual(values = age_colors)

# print plots
p1 + p2 + plot_layout(nrow = 2, guides = 'collect')

# TODO how is the estimate for low adult cost outside of the CI for low HCE?

tile_data <- expand.grid(
  gdpcap = seq(min(use_hosp$gdpcap, na.rm = TRUE), 
               max(use_hosp$gdpcap, na.rm = TRUE), 
               length.out = 100),
  hce_prop_gdp = seq(min(use_hosp$hce_prop_gdp, na.rm = TRUE), 
                     max(use_hosp$hce_prop_gdp, na.rm = TRUE), 
                     length.out = 100),
  study_pop = unique(use_hosp$study_pop)
)

pred_tile <- predict(lm_hosp, newdata = tile_data, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(tile_data)

ggplot() +
  geom_tile(data = pred_tile, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
  labs(
    title = "Predicted cost by GDPpc and HCE as a proportion of GDPpc",
    y = "Healthcare expenditure as a proportion of GDP per capita",
    x = "GDP per capita",
    fill = "Predicted cost"
  ) +
  geom_point(data = unique(use_hosp %>% select(gdpcap,hce_prop_gdp) %>% 
                             filter(gdpcap > 55000 & hce_prop_gdp > 0.15)),
             aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'white') +
  geom_point(data = unique(use_hosp %>% select(gdpcap,hce_prop_gdp) %>% 
                             filter(gdpcap <= 55000 | hce_prop_gdp <= 0.15)),
             aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'black') +
  facet_grid(study_pop~.) + 
  theme_minimal() + 
  scale_fill_viridis(option = 'A', direction = -1) 


####################################

# correlation between GDP per capita and hce_prop_gdp

unique(costs_gdp %>% 
         select(iso3c, study_year, gdpcap, hce_prop_gdp)) %>% 
  ggplot() +
  geom_point(aes(gdpcap, hce_prop_gdp, col = study_year)) + 
  theme_bw() + 
  labs(x = 'GDP per capita', 
       y = 'Healthcare expenditure as a proportion of GDP per capita') + 
  scale_color_viridis()


loo_with_hce <- loo(lm_hosp)
# TODO then do again without hce
# then compare using loo_compare(loo_with_hce, loo_without_hce)
# loo_compare() reports the difference in expected log predictive density (elpd) between models.
# A positive difference favors the first model (fit_with_HCE).
# Differences larger than about 4–5 are considered meaningful.
# Also check standard errors reported by loo_compare().









