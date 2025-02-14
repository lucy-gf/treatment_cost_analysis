## RUN ANALYSIS ##

# initialising
library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','aesthetics.R'))

main_year <- 2023

# loading data
data <- data.table(read_xlsx(here::here('data','Umbrella review data LG.xlsx')))[!is.na(th),] # remove empty rows

# cleaning data
source(here::here('scripts','data_cleaning.R'))
## add a file to merge with extra adult outpatient data

# producing model
source(here::here('scripts','model_run.R'))

# plots
source(here::here('scripts','plots.R')) 



