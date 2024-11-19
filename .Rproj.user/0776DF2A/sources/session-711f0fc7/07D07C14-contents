## CLEAN DATA ##

costs_dt <- data[, c('country','iso3c','currency','currency_iso3c','study_year','currency_year','study_pop',
                         'outcome','cost','study')][study_pop%in%c('adults','elderly','children'),]

## putting all costs into USD$2022

## back into local currency
# WDIsearch('exchange')
if(!exists('lcu_rates')){lcu_rates <- data.table(WDI(indicator='PA.NUS.FCRF', start=min(costs_dt$currency_year), end=2022))}
lcu_rates[, currency_year := year]
costs_dt <- costs_dt[lcu_rates, on = c('iso3c', 'currency_year'), lcu_rate := i.PA.NUS.FCRF]
costs_dt <- costs_dt[lcu_rates[currency_year==2022], on = c('iso3c'), lcu_rate_22 := i.PA.NUS.FCRF]
for(i in 1:nrow(costs_dt)){
  if(costs_dt$iso3c[i] %in% c('AUT','BEL','DEU','ESP','FIN','ITA','NLD')){
    costs_dt$lcu_rate[i] <- lcu_rates[iso3c=='EMU' & currency_year == costs_dt$currency_year[i]]$PA.NUS.FCRF
    costs_dt$lcu_rate_22[i] <- lcu_rates[iso3c=='EMU' & currency_year == 2022]$PA.NUS.FCRF
  }
  if(costs_dt$iso3c[i] %in% c('TWN') & costs_dt$currency_year[i] == 2010){
    costs_dt$lcu_rate[i] <- 31.5
    costs_dt$lcu_rate_22[i] <- 29.8
  }
}

# ggplot(costs_dt) + 
#   geom_point(aes(x=lcu_rate, y=lcu_rate_22, col=iso3c)) +
#   geom_line(aes(x=lcu_rate, y=lcu_rate), lty=2) +
#   theme_bw() + scale_y_log10() + scale_x_log10()

## inflate to $2022
if(!exists('deflate_rates')){deflate_rates <- data.table(WDI(indicator='NY.GDP.DEFL.ZS', start=min(costs$currency_year), end=2022))}
deflate_rates[, currency_year := year][, currency_iso3c := iso3c]
costs_dt <- costs_dt[deflate_rates, on = c('currency_iso3c', 'currency_year'), curr_yr_defl_rate := i.NY.GDP.DEFL.ZS]
costs_dt <- costs_dt[deflate_rates[year==2022,], on = c('currency_iso3c'), defl_rate_22 := i.NY.GDP.DEFL.ZS]
costs_dt[, inflator := defl_rate_22/curr_yr_defl_rate]
costs_dt[currency_iso3c == 'USA', cost_lcu := cost*lcu_rate]
costs_dt[currency_iso3c == iso3c, cost_lcu := cost]
costs_dt[, cost_lcu_2022 := cost_lcu*inflator][, cost_usd_2022 := cost_lcu_2022/lcu_rate]

## adding GDP per capita
# using 2022 GDP per capita
gdp_data <- WDI(indicator='NY.GDP.PCAP.KD', start=2022, end=2022)
costs_gdp <- merge(costs_dt, gdp_data, by = c('iso3c'))
setnames(costs_gdp, 'NY.GDP.PCAP.KD', 'gdpcap')
costs_gdp[,c('country.y','iso2c','year', 'curr_yr_defl_rate','defl_rate_22') := NULL]

