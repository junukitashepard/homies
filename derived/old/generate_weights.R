#####################################
# Generate weights for Eora sectors #
#####################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
#####################################
# Import files
E2C <- read.csv(file.path(input, "E2C.csv"), stringsAsFactors = F) # this is already the weight
C2E <- read.csv(file.path(input, "C2E.csv"), stringsAsFactors = F)
energy_production <- read.csv(file.path(input, "energy_production_shares.csv"), stringsAsFactors = F)

# Separate perc_sector_to_crude.ng and perc_crude.ng_to_sector into crude and ng
E2C <- left_join(E2C, energy_production[c('country', 'year', 'shareCrNG.crude', 'shareCrNG.ng')], by = c('country', 'year'))
