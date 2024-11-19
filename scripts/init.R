## RUN ANALYSIS ##

# initialising
library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','aesthetics.R'))

# loading data
data <- data.table(read_csv(here::here('data','OLD_DATA.csv'), show_col_types = F))[,1:14] %>% filter(!is.na(country))

# cleaning data
source(here::here('scripts','data_cleaning.R'))

# producing model
source(here::here('scripts','model_run.R'))

# plots
source(here::here('scripts','plots.R')) 



