#####################################
# Generate weights for Eora sectors #
#####################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

raw <- "/data/jus3/GlobalIO/raw"
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
web <- read.csv(file.path(temp, "web_all.csv"), stringsAsFactors = F)
eora.sectors <- read.csv(file.path(raw, "OECD_SUT/OECD_Eora_Crosswalk.csv"), stringsAsFactors = F)

web.countries <- unique(web$country)

# Energy to energy industries (in physical units)

for (c in web.countries) {
  
  # Set up empty matrices
  pe2e <- matrix(NA, nrow = 4, ncol = 7)
  se2e <- matrix(NA, nrow = 3, ncol = 7)
  
  
  
}


# Separate perc_sector_to_crude.ng and perc_crude.ng_to_sector into crude and ng
E2C <- left_join(E2C, energy_production[c('country', 'year', 'shareCrNG.crude', 'shareCrNG.ng')], by = c('country', 'year'))
