
### MAKING FINAL OUTPUTS TABLE ###

## load WDI data
if(!exists('gdp_data')){gdp_data <- WDI(indicator='NY.GDP.PCAP.KD', start=main_year, end=main_year)}
if(!exists('hce_data')){hce_data <- data.table(WDI(indicator='SH.XPD.CHEX.PC.CD', start=2022, end=2022))}

## list of countries with available data 

gdp_isos <- unique((gdp_data %>% 
                      filter(1 == cumsum(country == "Afghanistan")) %>% 
                      drop_na())$iso3c)
hce_isos <- unique((hce_data %>% 
                      filter(1 == cumsum(country == "Afghanistan")) %>% 
                      drop_na())$iso3c)

isos <- intersect(gdp_isos, hce_isos)
isos <- isos[!isos==''] # remove empty value

## add WDI data back in
data_wdi <- data.frame(iso3c = isos) %>% 
  left_join(gdp_data %>% select(country,iso3c,NY.GDP.PCAP.KD), by = 'iso3c') %>% 
  left_join(hce_data %>% select(iso3c,SH.XPD.CHEX.PC.CD), by = 'iso3c')

colnames(data_wdi) <- c('iso3c','country','gdpcap','hce_cap')

data_wdi_long <- rbind(data_wdi %>% mutate(study_pop = 'adult', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'children', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'elderly', treatment_type = 'hosp'),
                  data_wdi %>% mutate(study_pop = 'adult', treatment_type = 'outp'),
                  data_wdi %>% mutate(study_pop = 'children', treatment_type = 'outp'),
                  data_wdi %>% mutate(study_pop = 'elderly', treatment_type = 'outp')) 

data_wdi_incl_iso <- data_wdi_long %>% filter(iso3c %in% costs_gdp$iso3c)

data_wdi_no_iso <- data_wdi_long %>% filter(iso3c %notin% costs_gdp$iso3c)

## predict model

out_table_incl_iso <- predict(pref_model, newdata = data_wdi_incl_iso, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_incl_iso) 

out_table_no_iso <- predict(pref_model, newdata = data_wdi_no_iso, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_no_iso)

out_table <- rbind(out_table_incl_iso,
                   out_table_no_iso)

out_table <- out_table %>% 
  arrange(treatment_type, study_pop, hce_cap) %>% 
  select(iso3c, country, gdpcap, hce_cap, treatment_type, study_pop, Estimate, Est.Error, starts_with('Q')) %>% 
  mutate(treatment_type = case_when(treatment_type == 'hosp' ~ 'Hospitalisation', T ~ 'Outpatient'),
         study_pop = case_when(study_pop == 'adult' ~ 'Adults', study_pop == 'children' ~ 'Children', T ~ 'Elderly')) %>% 
  mutate(Estimate = format(round(Estimate, 2), nsmall = 2), 
         Est.Error = format(round(Est.Error, 2), nsmall = 2),
         Q2.5 = format(round(Q2.5, 2), nsmall = 2),
         Q97.5 = format(round(Q97.5, 2), nsmall = 2))

colnames(out_table) <- c('ISO Code','Country','GDP per capita','Healthcare expenditure per capita',
                         'Treatment type','Age group','Estimated cost','Estimated error','Lower CI','Upper CI')

write_csv(out_table, 
          here::here('output','outcome_table.csv'))










