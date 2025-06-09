
## RUN ANALYSIS ##

# initialising
library(here)
source(here::here('scripts','setup','packages.R'))
source(here::here('scripts','setup','aesthetics.R'))

main_year <- 2023

# loading data
data <- data.table(read_xlsx(here::here('data','FINAL Merged Flu Cost Data.xlsx')))[!is.na(th),] # remove empty rows

# cleaning data
source(here::here('scripts','data_cleaning.R'))

# producing model
source(here::here('scripts','model_run.R'))

# plots
source(here::here('scripts','plots.R')) 

# make outputs table
source(here::here('scripts','make_outputs.R')) 


