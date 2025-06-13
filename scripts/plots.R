
#### PRODUCING AND SAVING PLOTS AND TABLES ####

# plot predictions against observed data
pred_obs <- fitted(pref_model, newdata = costs_gdp, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(costs_gdp) %>% 
  drop_na() 

# colorscale <- c('#a50f15','#fb6a4a','#fcbba1','#9e9ac8','#54278f')
colorscale <- rev(c('#a50f15','#fb6a4a','#fdae6b','#9ecae1','#6baed6','#08519c'))
breaks <- seq(min(pred_obs$hce_prop_gdp), max(pred_obs$hce_prop_gdp), length.out = length(colorscale))

# pred_obs %>% 
#   ggplot(aes(x = gdpcap, col = hce_prop_gdp)) +
#   geom_point(aes(y = cost_usd_main_yr), 
#              shape = 4, size = 3) +
#   geom_point(aes(y = Estimate), shape = 1, size = 3) +
#   geom_errorbar(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2])))) + 
#   labs(x = "GDP per capita ($XXXX)", y = "Treatment cost ($XXXX)", 
#        col = 'Healthcare expenditure\nper capita as a proportion\nof GDP per capita') +
#   scale_color_gradientn(colors = colorscale, values = scales::rescale(breaks)) +
#   scale_x_log10(breaks = c(1000,3000,10000,30000,100000), limits = c(1000,100000)) + scale_y_log10() + 
#   # scale_colour_continuous() + 
#   theme_bw() + theme(text = element_text(size = 12)) + 
#   facet_grid(outcome ~ study_pop, scales = 'free', 
#              labeller = labeller(outcome = outcome_labels,
#                                  study_pop = pop_labels))

pred_obs %>% 
  ggplot(aes(x = hce_cap, col = study_pop)) +
  geom_point(aes(y = cost_usd_main_yr), 
             size = 3) +
  # geom_point(aes(y = Estimate), shape = 1, size = 3) +
  # geom_errorbar(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2])))) + 
  # labs(x = "GDP per capita ($XXXX)", y = "Treatment cost ($XXXX)") +
  # scale_color_gradientn(colors = colorscale, values = scales::rescale(breaks)) +
  scale_x_log10(breaks = c(100,300,1000,3000,10000,30000)) +
  # scale_y_log10() +
  # scale_colour_continuous() +
  theme_bw() + theme(text = element_text(size = 12)) + 
  facet_grid(outcome ~ ., scales = 'free_y', 
             labeller = labeller(outcome = outcome_labels,
                                 study_pop = pop_labels))

pred_obs %>% 
  ggplot(aes(x = hce_cap, col = gdpcap)) +
  geom_point(aes(y = cost_usd_main_yr), 
             shape = 4, size = 3) +
  geom_point(aes(y = Estimate), shape = 1, size = 3) +
  geom_errorbar(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2])))) + 
  labs(x = "Healthcare expenditure per capita ($XXXX)", y = "Treatment cost ($XXXX)", 
       col = 'GDP per capita') +
  scale_color_gradientn(colors = colorscale, values = scales::rescale(breaks)) +
  scale_x_log10(breaks = c(100,300,1000,3000,10000,30000), limits = c(300,30000)) +
  scale_y_log10() +
  # scale_colour_continuous() +
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
    x = "Healthcare expenditure by capita",
    y = "Predicted cost ($XXXX)",
    fill = 'Age group',
    color = 'Age group'
  ) + scale_x_log10() + ylim(c(0,NA)) +
  theme_bw() + facet_grid(treatment_type ~ ., scales = 'free', 
                          labeller = labeller(treatment_type = outcome_labels)) +  
  theme(text = element_text(size = 12)) + 
  scale_fill_manual(values = age_colors) + 
  scale_color_manual(values = age_colors)

ggsave(here::here('plots','predicted_costs_line_grid.png'),
       width = 12, height = 8)

# TODO What is this actually predicting? Why isn't it one line?

# #### HEATMAPS #### 
# 
# h1 <- ggplot() +
#   geom_tile(data = pred_tile_hosp, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
#   labs(
#     y = "Healthcare expenditure as a proportion of GDP per capita",
#     x = "GDP per capita",
#     fill = "Predicted\nhospitalisation\ncost (USD XXXX)"
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



