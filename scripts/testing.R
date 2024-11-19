## TESTING BRMS PACKAGE 19/11 ##

# initialising
library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','packages.R'))

# loading data
data <- data.table(read_csv(here::here('data','OLD_DATA.csv'), show_col_types = F))[,1:14] %>% filter(!is.na(country))

