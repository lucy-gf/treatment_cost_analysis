
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

out_table_incl_iso <- predict(pref_model, newdata = data_wdi_incl_iso, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_incl_iso) 

out_table_no_iso <- predict(pref_model, newdata = data_wdi_no_iso, re_formula = NA, probs = interval_probs) %>%
  as_tibble() %>%
  bind_cols(data_wdi_no_iso)

out_table <- rbind(out_table_incl_iso,
                   out_table_no_iso)

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










