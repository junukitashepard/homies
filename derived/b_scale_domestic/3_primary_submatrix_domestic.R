##############################################
# Create domestic energy (primary) submatrix #
##############################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/PRIM_submat"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('RMariaDB')
library('hiofunctions')

# Import files #
################
# Import biofuels and energy production share files
bioshares <- read.csv(file.path(input, "bioshares.csv"), stringsAsFactors = F)
energy_production <- read.csv(file.path(input, "energy_production_shares.csv"), stringsAsFactors = F)

# Import country names
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)

# Import WEB for primary energy sources
for (e in primary.energy) {
  print(paste0('Importing: ', e))

  assign('web', read.csv(file.path(input, paste0("PRIM_inputs/", e, ".csv")), stringsAsFactors = F))

  # Get domestically produced value
  if (e == 'PET') {assign('web.dom', subset(web, flow == 'Oil refineries (transf.)' | flow == 'Production'))
  } else {assign('web.dom', subset(web, flow == 'Production'))}

  if (e != 'NUC') {
    assign('web.im', subset(web, flow == 'Imports'))

    web.dom <- dplyr::group_by(web.dom, country, year, iso.country, baci.country) %>%
               dplyr::summarise(value = sum(value, na.rm = T))
    web.dom$flow <- 'Domestic'

    web.im <- web.im[c('country', 'year', 'iso.country', 'baci.country', 'value', 'flow')]

    web.domim <- dplyr::full_join(web.dom, web.im,
                           by = c('country', 'year', 'iso.country', 'baci.country', 'value', 'flow'))

    web.domim <- dplyr::group_by(web.domim, country, year, iso.country, baci.country) %>%
                 dplyr::mutate(tot.value = sum(value, na.rm = T))

    web.domim$tot.value[web.domim$tot.value == 0] <- 1
    web.domim$share <- web.domim$value/web.domim$tot.value
    web.domim <- subset(web.domim, flow == 'Domestic')
    web.domim <- web.domim[c('country', 'year', 'iso.country', 'baci.country', 'share')]

    web <- web[c('country', 'year', 'iso.country', 'baci.country', 'flow', 'value')]
    web <- left_join(web, web.domim, by = c('country', 'year', 'iso.country', 'baci.country'))

    # Where domestic production = 0, set to 1 to get share
    web$abs_value <- abs(web$value)
    web$use_group[web$value < 0] <- 'Use flow'
    web <- dplyr::group_by(web, country, year, use_group) %>% mutate(total_value = sum(abs_value, na.rm = T)) # Get total for country-year
    web$total_value[web$total_value == 0] <- 1
    web$share_domestic <- web$abs_value/web$total_value
    
    web$value2 <- web$value*web$share
    web$value3 <- web$value2
    web$value3[web$value2 == 0 & web$value != 0] <- web$share_domestic[web$value2 == 0 & web$value != 0]
    
    web$value <- web$value3
    web$value2 <- web$value3 <- web$abs_value <- web$use_group <- web$total_value <- web$share_domestic <- NULL
  }

  # Use incidence factor for non combustion technologies
  if (e == 'RE') {web$value <- web$value/not.eff.RE}
  if (e == 'NUC') {web$value <- web$value/not.eff.NUC}
  if (e == 'HYD') {web$value <- web$value/not.eff.HYD}
  if (e == 'BIO') {web$value <- web$value/mean(not.eff.BIO)}

  # Strip "" from flow
  web$flow <- stringr::str_replace_all(web$flow, '"', '')

  assign(paste0('web.', e), web, envir = parent.frame())
}

# Function: compile domestic primary energy submatrix, by country
scale_prim <- function(country, y) {
  
  for (energy in primary.energy) {
    assign('web', get(paste0('web.', energy)))
    print (paste0('Scaling energy: ', energy))

    # Scale primary energy to generate use vectors
    environment(scale_elhe) <- environment()
    scale_elhe(c = country,
               y = y,
               secondary = FALSE,
               name = energy)
  }

  # Combine vectors of primary energy to matrix, save in output
  assign('mat', readRDS(file.path(temp, paste0('mat_BIO_', country, '_', y, '.rds'))))

  for (e in primary.energy[2:length(primary.energy)]) {
    assign('mat2', readRDS(file.path(temp, paste0('mat_', e, '_', country, '_', y, '.rds'))))
    mat <- rbind(mat, mat2)
  }

  saveRDS(mat, file.path(output, paste0(y, "/mat_", country, ".rds")))
}

# Run programs across all countries in iso.list
for (y in year.min:year.max) {
  import_eora(y, include.Q = T)
  rownames(Q)[1:3] <- c('NG', 'COAL', 'PET')

  for (i in country.list) {
    print(paste0("Running for: ", i))
    scale_prim(i, y)
  }
}

