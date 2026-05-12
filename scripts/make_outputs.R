
### MAKING FINAL OUTPUTS TABLE ###

## load WDI data
if(!exists('gdp_data')){gdp_data <- WDI(indicator='NY.GDP.PCAP.CD', start=main_year, end=main_year)}
if(!exists('hce_data')){hce_data <- data.table(WDI(indicator='SH.XPD.CHEX.PC.CD', start=2022, end=2022))}

## list of countries with available data 

gdp_isos <- unique((gdp_data %>%
                      filter(1 == cumsum(country == "Afghanistan")) %>%
                      drop_na())$iso3c)
hce_isos <- unique((hce_data %>% 
                      filter(1 == cumsum(country == "Afghanistan")) %>% 
                      drop_na())$iso3c)

isos <- hce_isos #intersect(gdp_isos, hce_isos)
isos <- isos[!isos==''] # remove empty value

## add WDI data back in
data_wdi <- data.frame(iso3c = isos) %>% 
  # left_join(gdp_data %>% select(country,iso3c,NY.GDP.PCAP.CD), by = 'iso3c') %>% 
  left_join(hce_data %>% select(country, iso3c,SH.XPD.CHEX.PC.CD), by = 'iso3c') %>% 
  mutate(SH.XPD.CHEX.PC.CD = inflate_USD_22_to_23*SH.XPD.CHEX.PC.CD) # inflate to 2023 HCEpc

colnames(data_wdi) <- c('iso3c','country','hce_cap')

data_wdi_long <- rbind(data_wdi %>% mutate(study_pop = 'adult', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'children', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'elderly', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'adult', treatment_type = 'outp'),
                  data_wdi %>% mutate(study_pop = 'children', treatment_type = 'outp'),
                  data_wdi %>% mutate(study_pop = 'elderly', treatment_type = 'outp')) 

data_wdi_incl_iso <- data_wdi_long %>% filter(iso3c %in% costs_gdp$iso3c)

data_wdi_no_iso <- data_wdi_long %>% filter(iso3c %notin% costs_gdp$iso3c)

## predict model

out_table_incl_iso <- fitted(pref_model, newdata = data_wdi_incl_iso, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_incl_iso) 

out_table_no_iso <- fitted(pref_model, newdata = data_wdi_no_iso, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_no_iso)

out_table <- rbind(out_table_incl_iso,
                   out_table_no_iso)

pred_obs_plot <- pred_obs %>% filter(effects == 'rand_eff') %>% 
  select(!treatment_type) %>% rename(treatment_type = outcome)

un_pred_obs_plot <- unique(pred_obs_plot %>% select(country, study_pop, treatment_type)) %>% 
  mutate(in_upop = T)

out_table_plot <- rbind(out_table_incl_iso %>% mutate(with_iso = 'rand_eff'),
                        out_table_no_iso %>% mutate(with_iso = 'no_rand_eff')) %>% 
  left_join(un_pred_obs_plot, by = c('country', 'study_pop', 'treatment_type')) %>% 
  mutate(in_upop = case_when(is.na(in_upop) ~ F, T ~ T)) %>% 
  rbind(fitted(pref_model, newdata = data_wdi_incl_iso %>% filter(iso3c=='USA'), 
               re_formula = NA, probs = interval_probs) %>%
          as_tibble() %>%
          bind_cols( data_wdi_incl_iso %>% filter(iso3c=='USA')) %>% 
          mutate(with_iso = 'no_rand_eff', in_upop = F))

effect_colors_2 <- effect_colors
names(effect_colors_2) <- c(T, F)

ggplot() +
  geom_line(data = out_table_plot %>% filter(with_iso == 'no_rand_eff'), 
            aes(x = hce_cap, y = Estimate, col = with_iso, group = with_iso), 
            lty = 2, lwd = 0.8) +
  geom_ribbon(data = out_table_plot %>% filter(with_iso == 'no_rand_eff'),
              aes(x = hce_cap, fill = with_iso,
                  ymin = get(paste0('Q', 100*interval_probs[1])), 
                  ymax = get(paste0('Q', 100*interval_probs[2]))),
              alpha = 0.3) + 
  geom_point(data = out_table_plot %>% filter(in_upop),
             aes(x = hce_cap, y = Estimate, col = with_iso), 
             shape = 1, size = 3) +
  geom_errorbar(data = out_table_plot %>% filter(in_upop),
                aes(x = hce_cap, col = with_iso,
                    ymin = get(paste0('Q', 100*interval_probs[1])), 
                    ymax = get(paste0('Q', 100*interval_probs[2]))),
                lwd = 0.8) + 
  geom_point(data = pred_obs_plot,
             aes(x = hce_cap, y = cost_usd_main_yr), 
             shape = 4, size = 3) +
  labs(x = "Healthcare expenditure per capita ($2023)", 
       y = "Treatment cost ($2023)", 
       col = '', fill = '') +
  # scale_color_gradientn(colors = colorscale, values = scales::rescale(breaks)) +
  scale_x_log10(breaks = c(30,100,300,1000,3000,10000,30000), limits = c(min(out_table_plot$hce_cap),30000)) +
  scale_y_log10() +
  scale_colour_manual(values = effect_colors, labels = effect_labels, aesthetics = c("colour", "fill")) +
  theme_bw() + theme(text = element_text(size = 12)) + 
  facet_grid(treatment_type ~ study_pop, scales = 'free', 
             labeller = labeller(treatment_type = outcome_labels,
                                 study_pop = pop_labels))

ggsave(here::here('plots','observed_vs_predicted.png'),
       width = 14, height = 8)

format_number <- function(num){
  
  out <- num
  
  for(i in 1:length(out)){
    if(out[i] < 10){
      out[i] <- round(out[i], 2)
    }else{
      out[i] <- signif(out[i], 4)
    }
  }
  
  for(i in 1:length(out)){
    if(as.numeric(out[i]) < 1000){
      out[i] <- gsub(' ', '', format(as.numeric(out[i]), nsmall = 2))
    }else{
      out[i] <- as.character(out[i])
    }
  }
  
  out
}

out_table_w <- out_table %>% 
  arrange(treatment_type, study_pop, hce_cap) %>% 
  select(iso3c, country, hce_cap, treatment_type, study_pop, Estimate, Est.Error, starts_with('Q')) %>% 
  mutate(treatment_type = case_when(treatment_type == 'hosp' ~ 'Inpatient', T ~ 'Outpatient'),
         study_pop = case_when(study_pop == 'adult' ~ 'Adults', study_pop == 'children' ~ 'Children', T ~ 'Older people')) %>% 
  mutate(hce_cap = format_number(hce_cap),
         Estimate = format_number(Estimate),
         Est.Error = format_number(Est.Error),
         Q2.5 = format_number(Q2.5),
         Q97.5 = format_number(Q97.5)) %>%
  mutate(value = paste0(Estimate, ' (', Q2.5, ', ', Q97.5, ')')) %>%
  select(!c(Estimate, Est.Error, starts_with('Q'))) %>%
  pivot_wider(id_cols = c(iso3c, country, hce_cap),
              names_from = c(treatment_type, study_pop),
              values_from = value)

colnames(out_table_w) <- c('ISO Code','Country','Healthcare expenditure per capita',
                            colnames(out_table_w)[4:length(colnames(out_table_w))])

write_csv(out_table_w, 
          here::here('output','outcome_table.csv'))

who_regions <- read_xlsx(here::here('data','who_regions.xlsx'))
colnames(who_regions) <- c('ISO Code','name','na','na','who_region','na')

hosp_p <- out_table %>% 
  left_join(who_regions %>% 
              select(iso3c, who_region) %>% 
              filter(iso3c %in% unique(out_table_w$`ISO Code`)), 
            by = 'iso3c') %>% 
  mutate(who_region = case_when(iso3c == 'LIE' ~ 'European Region',
                                iso3c == 'PSE' ~ 'Eastern Mediterranean Region',
                                T ~ who_region)) %>% 
  filter(treatment_type == 'hosp') %>% 
  ggplot() + 
  geom_violin(aes(x = who_region, y = Estimate,
                  col = study_pop, fill = study_pop),
              alpha = 0.3) +
  geom_point(aes(x = who_region, group = study_pop, y = Estimate, col = study_pop),
             position = position_dodge(width = 0.9), alpha = 0.8) +
  scale_color_manual(values = age_colors, labels = pop_labels) +
  scale_fill_manual(values = age_colors, labels = pop_labels) +
  labs(col = 'Age group', fill = 'Age group', x = '', y = 'Predicted cost ($2023)',
       title = 'Inpatient') +
  theme_bw() + 
  theme(text = element_text(size = 14),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_y_log10(); hosp_p

outp_p <- out_table %>% 
  left_join(who_regions %>% 
              select(iso3c, who_region) %>% 
              filter(iso3c %in% unique(out_table_w$`ISO Code`)), 
            by = 'iso3c') %>% 
  mutate(who_region = case_when(iso3c == 'LIE' ~ 'European Region',
                                iso3c == 'PSE' ~ 'Eastern Mediterranean Region',
                                T ~ who_region)) %>%
  filter(treatment_type == 'outp') %>% 
  ggplot() + 
  geom_violin(aes(x = who_region, y = Estimate,
                  col = study_pop, fill = study_pop),
              alpha = 0.3) +
  geom_point(aes(x = who_region, group = study_pop, y = Estimate, col = study_pop),
             position = position_dodge(width = 0.9), alpha = 0.8) +
  scale_color_manual(values = age_colors, labels = pop_labels) +
  scale_fill_manual(values = age_colors, labels = pop_labels) +
  labs(col = 'Age group', fill = 'Age group', x = '', y = 'Predicted cost ($2023)',
       title = 'Outpatient') +
  theme_bw() + theme(text = element_text(size = 14)) +
  scale_y_log10(); outp_p

hosp_p + outp_p + plot_layout(nrow = 2, guides = 'collect',
                              axis_title="collect")

ggsave(here::here('plots','cost_by_region.png'),
       width = 20, height = 10)


whoreg <- out_table %>% 
  left_join(who_regions %>% 
              select(iso3c, who_region) %>% 
              filter(iso3c %in% unique(out_table_w$`ISO Code`)), 
            by = 'iso3c') %>% 
  mutate(who_region = case_when(iso3c == 'LIE' ~ 'European Region',
                                iso3c == 'PSE' ~ 'Eastern Mediterranean Region',
                                T ~ who_region)) %>% 
  group_by(treatment_type, study_pop, who_region) %>% 
  summarise(range = paste0('$', round(min(Estimate)), ' - $', round(max(Estimate))))

print(whoreg %>% filter(study_pop == 'elderly',
                        who_region %like% 'Afr|Amer') %>% 
        pivot_wider(names_from = who_region, values_from = range))


# ggplot(pred_obs_plot, aes(x = hce_cap, y = cost_usd_main_yr, col = study_pop)) +
#   geom_point(size = 2, alpha = 0.6) +
#   geom_smooth(method = "lm") +
#   labs(x = "Healthcare expenditure per capita ($2023)", 
#        y = "Treatment cost ($2023)") +
#   scale_x_log10(breaks = c(30,100,300,1000,3000,10000,30000),
#                 limits = c(min(out_table_plot$hce_cap),30000)) +
#   scale_y_log10() +
#   facet_grid(treatment_type ~ ., scales = 'free') + 
#   theme_bw() + theme(text = element_text(size = 12)) 







