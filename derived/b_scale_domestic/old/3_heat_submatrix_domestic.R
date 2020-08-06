##################################
# Create domestic heat submatrix #
##################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))
source(paste0(wd, "/derived/3a_scale_elhe.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/HEAT_submat"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('RMariaDB')

sink(paste0(wd, "/derived/out/3c_heat_submatrix_domestic.txt"))

# Import files #
################
# Import WEB for electricity
print("Bring in WEB data from SQL")

sql_import(statement = 'SELECT * FROM WEB.WEB_EXTENDED WHERE WEB.WEB_EXTENDED.Product = "Heat" AND WEB.WEB_EXTENDED.Measure = "tj"',
           outname = 'web')

web <- unique(web[c('Country', 'Flow', 'Time', 'Value')])
names(web) <- c('country', 'flow', 'year', 'value')

web$flow <- stringr::str_replace_all(web$flow, '"', '')
web$value <- as.numeric(web$value)

# Import biofuels and energy production share files
bioshares <- read.csv(file.path(input, "bioshares.csv"), stringsAsFactors = F)
energy_production <- read.csv(file.path(input, "energy_production_shares.csv"), stringsAsFactors = F)

# Add ISO country codes
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)

web <- left_join(web, web.countries, by = c('country' = 'web.country'))
web <- subset(web, !is.na(iso.country))

energy_production <- inner_join(energy_production, web.countries, by = c('country'= 'web.country'))
bioshares <- inner_join(bioshares, web.countries, by = c('country' = 'web.country'))

iso.list <- unique(web$iso.country)

energy.names <- c('BIO', 'COAL', 'CRU', 'NG', 'PET', 'RE', 'NUC', 'HYD', 'ELEC_RE', 'ELEC_NU', 'ELEC_HY', 'ELEC_COM', 'HEAT_RE', 'HEAT_NU', 'HEAT_HY', 'HEAT_COM', 'LOSSES')

# Run program (scale_elhe)
for (c in iso.list) {
  print(paste0("Running scale_ELEC for: ", c))
  scale_elhe(c)
}

sink()
