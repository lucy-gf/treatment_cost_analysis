
#### PRODUCING AND SAVING PLOTS AND TABLES ####

# residuals vs fitted values

fitted_vals <- fitted(pref_model)[, "Estimate"]
residuals <- residuals(pref_model)[, "Estimate"]

r_v_f <- data.frame(fitted_vals = fitted_vals, residuals = residuals,
                    study_pop = pref_model$data$study_pop, 
                    treatment_type = pref_model$data$treatment_type)

r_v_f %>%
  ggplot() +
  geom_point(aes(fitted_vals, abs(residuals), col = study_pop, shape = treatment_type),
             size = 3) +
  labs(x = "Fitted values", y = "Absolute residual values", col = 'Age group', shape = 'Treatment type') +
  theme_bw() +
  scale_color_manual(values = age_colors, labels = pop_labels) +
  scale_shape_manual(values = c(16,1), labels = outcome_labels) +
  scale_x_log10() +
  scale_y_log10() +
  theme(text = element_text(size = 10)) 

ggsave(here::here('plots','fitted_vs_residuals.png'),
       width = 8, height = 6)

# colorscale <- c('#a50f15','#fb6a4a','#fcbba1','#9e9ac8','#54278f')
# colorscale <- rev(c('#a50f15','#fb6a4a','#fdae6b','#9ecae1','#6baed6','#08519c'))
# breaks <- seq(min(pred_obs$hce_cap), max(pred_obs$hce_cap), length.out = length(colorscale))

ggplot() +
  geom_line(data = pred_obs %>% filter(effects == 'no_rand_eff'), 
             aes(x = hce_cap, y = Estimate, col = effects, group = effects), 
            lty = 2, lwd = 0.8) +
  geom_ribbon(data = pred_obs %>% filter(effects == 'no_rand_eff'),
              aes(x = hce_cap, fill = effects,
                  ymin = get(paste0('Q', 100*interval_probs[1])), 
                  ymax = get(paste0('Q', 100*interval_probs[2]))),
              alpha = 0.3) + 
  geom_point(data = pred_obs %>% filter(effects == 'rand_eff'),
             aes(x = hce_cap, y = Estimate, col = effects), 
             shape = 1, size = 3) +
  geom_errorbar(data = pred_obs %>% filter(effects == 'rand_eff'),
                aes(x = hce_cap, col = effects,
                    ymin = get(paste0('Q', 100*interval_probs[1])), 
                    ymax = get(paste0('Q', 100*interval_probs[2]))),
                lwd = 0.8) + 
  geom_point(data = pred_obs %>% filter(effects == 'rand_eff'),
             aes(x = hce_cap, y = cost_usd_main_yr), 
             shape = 4, size = 3) +
  labs(x = "Healthcare expenditure per capita ($2023)", 
       y = "Treatment cost ($2023)", 
       col = '', fill = '') +
  # scale_color_gradientn(colors = colorscale, values = scales::rescale(breaks)) +
  scale_x_log10(breaks = c(100,300,1000,3000,10000,30000), limits = c(80,30000)) +
  scale_y_log10() +
  scale_colour_manual(values = effect_colors, labels = effect_labels,  aesthetics = c("colour", "fill")) +
  theme_bw() + theme(text = element_text(size = 12)) + 
  facet_grid(outcome ~ study_pop, scales = 'free', 
             labeller = labeller(outcome = outcome_labels,
                                 study_pop = pop_labels))

ggsave(here::here('plots','observed_vs_predicted.png'),
       width = 14, height = 8)

pred_obs_save <- pred_obs %>% 
  select(country, iso3c, study_pop, outcome, cost_usd_main_yr, Estimate, starts_with('Q')) %>% 
  arrange(outcome, study_pop, country) %>% 
  setnames(paste0('Q', 100*interval_probs[1]), 'lower') %>% 
  setnames(paste0('Q', 100*interval_probs[2]), 'upper') %>% 
  mutate(cost_usd_main_yr = round(cost_usd_main_yr),
         Estimate = round(Estimate),
         lower = round(lower),
         upper = round(upper))

colnames(pred_obs_save) <- c('Country', 'ISO3C', 'Age group', 'Treatment', 'Observed', 'Predicted', 'Lower CI', 'Upper CI')

write_csv(pred_obs_save, 
          here::here('output','observed_vs_predicted.csv'))

ggplot(pred_hce, aes(x = hce_cap, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) + 
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    x = "Healthcare expenditure per capita ($2023)",
    y = "Predicted cost ($2023)",
    fill = 'Age group',
    color = 'Age group'
  ) + 
  scale_x_log10() + 
  # scale_y_log10() + 
  theme_bw() + facet_grid(treatment_type ~ ., scales = 'free', 
                          labeller = labeller(treatment_type = outcome_labels)) +  
  theme(text = element_text(size = 12)) + 
  scale_fill_manual(values = age_colors, labels = pop_labels) + 
  scale_color_manual(values = age_colors, labels = pop_labels)

ggsave(here::here('plots','predicted_costs_line_grid.png'),
       width = 12, height = 8)

ggplot(pred_hce_all_models, aes(x = predictor, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    x = "",
    y = "Predicted cost ($2023)",
    fill = 'Age group',
    color = 'Age group'
  ) + 
  scale_x_log10() + 
  # scale_y_log10() + 
  theme_bw() + facet_wrap(treatment_type ~ model, scales = 'free', 
                          labeller = labeller(treatment_type = outcome_labels)) +  
  theme(text = element_text(size = 12)) + 
  scale_fill_manual(values = age_colors, labels = pop_labels) + 
  scale_color_manual(values = age_colors, labels = pop_labels)

ggsave(here::here('plots','predicted_costs_line_grid_all_models.png'),
       width = 20, height = 14)

# #### HEATMAPS #### 
# 
# h1 <- ggplot() +
#   geom_tile(data = pred_tile_hosp, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
#   labs(
#     y = "Healthcare expenditure as a proportion of GDP per capita",
#     x = "GDP per capita",
#     fill = "Predicted\nhospitalisation\ncost ($2023)"
#   ) +
#   geom_point(data = unique(use_hosp %>% select(gdpcap,hce_prop_gdp,study_pop) %>% drop_na()),
#              aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'white') +
#   facet_grid(study_pop~., labeller = labeller(outcome = outcome_labels,
#                                               study_pop = pop_labels)) + 
#   theme_minimal() + theme(text = element_text(size = 12)) + 
#   scale_fill_viridis(option = 'A', direction = -1, trans = 'pseudo_log',
#                      breaks = c(300,1000,3000,10000,30000), limits = c(NA, 30000)) 
# 
# h2 <- ggplot() +
#   geom_tile(data = pred_tile_outp, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
#   labs(
#     y = "Healthcare expenditure as a proportion of GDP per capita",
#     x = "GDP per capita",
#     fill = "Predicted\noutpatient\ncost (USD )"
#   ) +
#   geom_point(data = unique(use_outp %>% select(gdpcap,hce_prop_gdp,study_pop) %>% drop_na()),
#              aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'white') +
#   facet_grid(study_pop~., labeller = labeller(outcome = outcome_labels,
#                                               study_pop = pop_labels)) + 
#   theme_minimal() + theme(text = element_text(size = 12)) + 
#   scale_fill_viridis(option = 'D', direction = -1, trans = 'pseudo_log',
#                      breaks = c(3,10,30,100,300), limits = c(10, NA)) 
# 
# # print plots
# h1 + h2 + plot_layout(nrow = 1)
# 
# ggsave(here::here('plots','predicted_costs_heatmap_grid.png'),
#        width = 12, height = 10)



