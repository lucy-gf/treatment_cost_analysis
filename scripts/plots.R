
#### PRODUCING AND SAVING PLOTS ####

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

ggsave(here::here('plots','observed_vs_predicted.png'),
       width = 12, height = 8)


predicted_costs_gdp <- rbind(pred_gdp_hosp %>% mutate(outp = 'Hospitalisation'),
                             pred_gdp_outp %>% mutate(outp = 'Outpatient'))

predicted_costs_hce <- rbind(pred_hce_hosp %>% mutate(outp = 'Hospitalisation'),
                             pred_hce_outp %>% mutate(outp = 'Outpatient'))

p1 <- ggplot(predicted_costs_gdp, aes(x = gdpcap, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted cost by log GDPpc (HCE fixed)",
    x = "GDP per capita",
    y = "Predicted cost",
    fill = 'Age group',
    color = 'Age group'
  ) + scale_x_log10() + ylim(c(0,NA)) + 
  theme_bw() + facet_grid(outp ~ ., scales = 'free') + 
  scale_fill_manual(values = age_colors) + 
  scale_color_manual(values = age_colors) +
  theme(strip.text = element_blank())

p2 <- ggplot(predicted_costs_hce, aes(x = hce_prop_gdp, y = Estimate, color = study_pop, fill = study_pop)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin = get(paste0('Q', 100*interval_probs[1])), ymax = get(paste0('Q', 100*interval_probs[2]))), alpha = 0.2, color = NA) +
  labs(
    title = "Predicted cost by HCE (GDPpc fixed)",
    x = "HCE as proportion of GDPpc",
    y = "Predicted cost",
    fill = 'Age group',
    color = 'Age group'
  ) + scale_x_log10() + ylim(c(0,NA)) + 
  theme_bw() + facet_grid(outp ~ ., scales = 'free') + 
  scale_fill_manual(values = age_colors) + 
  scale_color_manual(values = age_colors)

# print plots
p1 + p2 + plot_layout(nrow = 1, guides = 'collect', axes = 'collect')

ggsave(here::here('plots','predicted_costs_line_grid.png'),
       width = 12, height = 8)

# TODO how is the estimate for adult hospitalisation cost outside of the CI for low HCE?

#### HEATMAPS #### 

h1 <- ggplot() +
  geom_tile(data = pred_tile_hosp, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
  labs(
    y = "Healthcare expenditure as a proportion of GDP per capita",
    x = "GDP per capita",
    fill = "Predicted\nhospitalisation\ncost (USD XXXX)"
  ) +
  geom_point(data = unique(use_hosp %>% select(gdpcap,hce_prop_gdp,study_pop)),
             aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'white') +
  facet_grid(study_pop~.) + 
  theme_minimal() + 
  scale_fill_viridis(option = 'A', direction = -1, trans = 'pseudo_log',
                     breaks = c(300,1000,3000,10000,30000), limits = c(NA, 30000)) 

h2 <- ggplot() +
  geom_tile(data = pred_tile_outp, aes(x = gdpcap, y = hce_prop_gdp, fill = Estimate)) +
  labs(
    y = "Healthcare expenditure as a proportion of GDP per capita",
    x = "GDP per capita",
    fill = "Predicted\noutpatient\ncost (USD XXXX)"
  ) +
  geom_point(data = unique(use_outp %>% select(gdpcap,hce_prop_gdp,study_pop)),
             aes(x = gdpcap, y = hce_prop_gdp), shape = 4, col = 'white') +
  facet_grid(study_pop~.) + 
  theme_minimal() + 
  scale_fill_viridis(option = 'D', direction = -1, trans = 'pseudo_log',
                     breaks = c(3,10,30,100,300), limits = c(10, NA)) 

# print plots
h1 + h2 + plot_layout(nrow = 1)

ggsave(here::here('plots','predicted_costs_heatmap_grid.png'),
       width = 12, height = 10)



