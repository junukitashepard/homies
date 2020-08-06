#########################################
# Create domestic electricity submatrix #
#########################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/ELEC_submat"
figures <- "/data/jus3/GlobalIO/output/figures"
temp <- tempdir()

library('magrittr')
library('dplyr')
library('RMariaDB')
library('hiofunctions')

#sink(paste0(wd, "/derived/out/1_elec_submatrix_domestic.txt"))

# Import files #
################
# Import WEB for electricity
print("Bring in WEB data from SQL")

sql_import(statement = 'SELECT * FROM WEB.WEB_EXTENDED WHERE WEB.WEB_EXTENDED.Product = "Electricity" AND WEB.WEB_EXTENDED.Measure = "tj"',
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

# Run program (scale_elhe)
for (y in year.min:year.max) {
  import_eora(year = y)

  for (c in country.list) { # country.list is made in import_eora
    print(paste0("Running scale_ELEC for: ", c))
    scale_elhe(c, y)
  }
}

#sink()
