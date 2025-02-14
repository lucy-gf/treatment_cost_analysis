## CLEAN DATA ##

costs_dt <- data[, c('th','country','iso3c','currency','currency_iso3c','study_year','currency_year','study_pop',
                         'outcome','cost')]

costs_dt[is.na(study_year), study_year := currency_year]
costs_dt[, study_year := as.numeric(study_year)]

## putting all costs into USD$2023 (year can be changed but this is the last full WDI year)

## back into local currency
# WDIsearch('exchange')
if(!exists('lcu_rates')){lcu_rates <- data.table(WDI(indicator='PA.NUS.FCRF', start=min(costs_dt$study_year), end=main_year))}
lcu_rates[, study_year := year]
costs_dt <- costs_dt[lcu_rates, on = c('iso3c', 'study_year'), lcu_rate_study_year := i.PA.NUS.FCRF]
costs_dt <- costs_dt[lcu_rates[study_year==main_year], on = c('iso3c'), lcu_rate_main_yr := i.PA.NUS.FCRF]

costs_dt[iso3c == 'TWN' & study_year == 2010, lcu_rate_study_year := 31.5]
costs_dt[iso3c == 'TWN' & study_year == 2010, lcu_rate_main_yr := 29.8]

# ggplot(costs_dt) +
#   geom_point(aes(x=lcu_rate, y=lcu_rate_22, col=iso3c, size=currency_year-1990), shape=4) +
#   geom_line(aes(x=lcu_rate, y=lcu_rate), lty=2) +
#   theme_bw() + scale_y_log10() + scale_x_log10()

## inflate to $ main year
if(!exists('deflate_rates')){deflate_rates <- data.table(WDI(indicator='NY.GDP.DEFL.ZS', start=min(costs_dt$study_year), end=main_year))}
deflate_rates[, currency_year := year][, study_year := year][, currency_iso3c := iso3c]
costs_dt <- costs_dt[deflate_rates, on = c('currency_iso3c', 'study_year'), study_yr_defl_rate_c := i.NY.GDP.DEFL.ZS]
costs_dt <- costs_dt[deflate_rates, on = c('currency_iso3c', 'currency_year'), curr_yr_defl_rate_c := i.NY.GDP.DEFL.ZS]
costs_dt <- costs_dt[deflate_rates, on = c('iso3c', 'study_year'), study_yr_defl_rate_i := i.NY.GDP.DEFL.ZS]
costs_dt <- costs_dt[deflate_rates[year==main_year,], on = c('iso3c'), defl_rate_main_yr_i := i.NY.GDP.DEFL.ZS]
costs_dt[, inflator_curr_to_stud_c := curr_yr_defl_rate_c/study_yr_defl_rate_c]
costs_dt[, inflator_stud_to_main_i := defl_rate_main_yr_i/study_yr_defl_rate_i]

if(sum(costs_dt[currency_iso3c != iso3c]$currency_iso3c != 'USA') > 0){
  print('Warning: some costs not in either local currency or USD')
}

# TURN COSTS IN CURRENCY (E.G. USD) INTO STUDY YEAR COSTS IN SAME CURRENCY
costs_dt[, cost_stud_c := cost/inflator_curr_to_stud_c]

# CONVERT TO LCU IN STUDY YEAR
costs_dt[currency_iso3c == 'USA', cost_stud_i := cost_stud_c*lcu_rate_study_year]
costs_dt[currency_iso3c == iso3c, cost_stud_i := cost_stud_c]

# INFLATE TO MAIN YEAR IN LCU
costs_dt[, cost_main_yr_i := cost_stud_i*inflator_stud_to_main_i]

# CONVERT TO MAIN YEAR USD
costs_dt[, cost_usd_main_yr := cost_main_yr_i/lcu_rate_main_yr]

## adding GDP per capita
if(!exists('gdp_data')){gdp_data <- WDI(indicator='NY.GDP.PCAP.KD', start=main_year, end=main_year)}
costs_gdp <- data.table(merge(costs_dt, gdp_data, by = c('iso3c')))
setnames(costs_gdp, 'NY.GDP.PCAP.KD', 'gdpcap')
setnames(costs_gdp, 'country.x','country')
costs_gdp <- costs_gdp[,c('th','country','iso3c','currency_iso3c','study_year','currency_year','study_pop','outcome','cost_usd_main_yr','gdpcap')]

# ggplot(costs_gdp) +
#   geom_point(aes(x=gdpcap, y=cost_usd_main_yr, col=iso3c)) +
#   theme_bw() + #scale_y_log10() + scale_x_log10() +
#   facet_grid(outcome ~ study_pop, scales = 'free_y')

# healthcare expenditure per capita as a proportion of GDP per capita 
# NOTE - using 2021 as is most recent data - need to find other source to update this
if(!exists('hce_data')){hce_data <- data.table(WDI(indicator='SH.XPD.CHEX.PC.CD', start=2021, end=2021))}
costs_gdp <- costs_gdp[hce_data[iso3c %in% costs_gdp$iso3c,][,c('iso3c','SH.XPD.CHEX.PC.CD')], on = c('iso3c')]
setnames(costs_gdp, 'SH.XPD.CHEX.PC.CD', 'hce_cap')
costs_gdp[, hce_prop_gdp := hce_cap/gdpcap]

# ggplot(costs_gdp) +
#     geom_point(aes(x=hce_prop_gdp, y=cost_usd_main_yr, col=iso3c)) +
#     theme_bw() + facet_grid(outcome ~ study_pop, scales = 'free_y')

ggplot(costs_gdp) +
  geom_point(aes(x=hce_prop_gdp, y=gdpcap, col=iso3c)) +
  theme_bw() + facet_grid(outcome ~ study_pop, scales = 'free_y')











