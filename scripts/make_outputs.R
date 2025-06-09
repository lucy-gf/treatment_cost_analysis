
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

colnames(data_wdi) <- c('iso3c','country','gdpcap','hcecap')

data_wdi <- rbind(data_wdi %>% mutate(study_pop = 'adult'),
                  data_wdi %>% mutate(study_pop = 'children'),
                  data_wdi %>% mutate(study_pop = 'elderly')) %>% mutate(hce_prop_gdp = hcecap/gdpcap)

## predict model

## hospitalisation

hospitalisation_table <- predict(lm_hosp, newdata = data_wdi, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi) %>% 
  mutate(outcome = 'hospitalisation') %>% 
  arrange(study_pop, country)

write_csv(hospitalisation_table, 
          here::here('output','hospitalisation_table.csv'))

## outpatient

outpatient_table <- predict(lm_outp, newdata = data_wdi, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi) %>% 
  mutate(outcome = 'outpatient') %>% 
  arrange(study_pop, country)

write_csv(outpatient_table, 
          here::here('output','outpatient_table.csv'))









