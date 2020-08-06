#########################################
# Create domestic electricity submatrix #
#########################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))
source(paste0(wd, "derived/3a_scale_elhe.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/ELEC_submat"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('RMariaDB')

sink(paste0(wd, "/derived/out/3_electricity_submatrix.R"))

# Import files #
################
# Import WEB for electricity
print("Bring in WEB data from SQL")
connection <- dbConnect(RMariaDB::MariaDB(),
                        user = 'jus3',
                        password = '$eam0n$teR',
                        dbname = 'GLOBALIO')

web <- dbSendQuery(connection, 'SELECT * FROM WEB.WEB_EXTENDED WHERE WEB.WEB_EXTENDED.Product = "Electricity" AND WEB.WEB_EXTENDED.Measure = "tj"')
web <- dbFetch(web)
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

# Function: Scale electricity submatrix
scale_ELEC <- function(c) {
  
  df <- subset(web, iso.country == c)
  
  min.y <- max(min(energy_production$year[energy_production$iso.country == c]), 
               min(web$year[web$iso.country == c]), na.rm = T)
  max.y <- min(max(energy_production$year[energy_production$iso.country == c]), 
               max(web$year[web$iso.country == c]))
  
  for (y in min.y:max.y) {
    
    suppressWarnings(dir.create(file.path(output, y)))
    
    print(paste0("Running year: ", y))
    # Electricity to biofuels
    elec2agr <- df$value[df$flow == "Agriculture/forestry" & df$year == y]
    elec2agr <- elec2agr * bioshares$share_biofuel[bioshares$iso.country == c & bioshares$year == y]
    
    elec2bkb <- df$value[(df$flow == "BKB/peat briquette plants (energy)") & df$year == y]
    #assert('length(elec2bkb) <= 1')
    if (length(elec2bkb) == 0) {elec2bkb <- 0}
    if (length(elec2agr) == 0) {elec2agr <- 0}
    
    elec2bio <- elec2agr + elec2bkb
    
    # Electricity to coal mining
    elec2coal <- abs(df$value[df$flow == "Coal mines (energy)" & df$year == y])
    if (length(elec2coal) == 0) {elec2coal <- 0}
    
    # Electricity to crude/NG
    s.crude <- energy_production$shareCrNG.crude[energy_production$iso.country == c & energy_production$year == y]
    s.ng <- energy_production$shareCrNG.ng[energy_production$iso.country == c & energy_production$year == y]
    
    elec2crng <- abs(df$value[df$flow == "Oil and gas extraction (energy)" & df$year == y])
    
    elec2crude <- elec2crng * s.crude
    elec2ng <- elec2crng * s.ng
    if (length(elec2crude) == 0) {elec2crude <- 0}
    if (length(elec2ng) == 0) {elec2ng <- 0}
    
    # Electricity RE is 0 (no electricity to produce sun, wind)
    elec2re <- 0 
    
    # Electricity to petroleum
    elec2pet <- abs(df$value[df$flow == "Oil refineries (energy)" & df$year == y])
    if (length(elec2pet) == 0) {elec2pet <- 0}
    
    # Electricity to nuclear
    elec2nuc <- abs(df$value[df$flow == "Nuclear industry  (energy)" & df$year == y])
    if (length(elec2nuc) == 0) {elec2nuc <- 0}
    
    # Electricity to hydro
    elec2hyd <- abs(df$value[df$flow == "Pumped storage plants (energy)" & df$year == y])
    if (length(elec2hyd) == 0) {elec2hyd <- 0}
    
    # Electricity to electricity (must scale)
    elec2elec <- abs(df$value[df$flow == "Main activity producer electricity plants (transf.)" & df$year == y])
    
    se.re <- energy_production$share.elec.RE[energy_production$iso.country == c & energy_production$year == y]
    se.nu <- energy_production$share.elec.NU[energy_production$iso.country == c & energy_production$year == y]
    se.hy <- energy_production$share.elec.HY[energy_production$iso.country == c & energy_production$year == y]
    se.co <- energy_production$share.elec.COM[energy_production$iso.country == c & energy_production$year == y]
    
    elec2elec_re <- elec2elec * se.re
    elec2elec_nu <- elec2elec * se.nu
    elec2elec_hy <- elec2elec * se.hy
    elec2elec_com <- elec2elec * se.co
    if (length(elec2elec_re) == 0) {elec2elec_re <- 0}
    if (length(elec2elec_nu) == 0) {elec2elec_nu <- 0}
    if (length(elec2elec_hy) == 0) {elec2elec_hy <- 0}
    if (length(elec2elec_com) == 0) {elec2elec_com <- 0}
    
    # Electricity to heat (must scale)
    elec2heat <- abs(df$value[df$flow == "Main activity producer CHP plants (transf.)" & df$year == y])
    
    sh.re <- energy_production$share.heat.RE[energy_production$country == c & energy_production$year == y]
    sh.nu <- energy_production$share.heat.NU[energy_production$country == c & energy_production$year == y]
    sh.hy <- energy_production$share.heat.HY[energy_production$country == c & energy_production$year == y]
    sh.co <- energy_production$share.heat.COM[energy_production$country == c & energy_production$year == y]
    
    elec2heat_re <- elec2heat * sh.re
    elec2heat_nu <- elec2heat * sh.nu
    elec2heat_hy <- elec2heat * sh.hy
    elec2heat_com <- elec2heat * sh.co
    if (length(elec2heat_re) == 0) {elec2heat_re <- 0}
    if (length(elec2heat_nu) == 0) {elec2heat_nu <- 0}
    if (length(elec2heat_hy) == 0) {elec2heat_hy <- 0}
    if (length(elec2heat_com) == 0) {elec2heat_com <- 0}
    
    # Electricity to losses
    elec2loss <- abs(df$value[df$flow == "Losses" & df$year == y])
    if (length(elec2loss) == 0) {elec2loss <- 0}
    
    # Scale electricity terms
    elec_list <- c(elec2bio, elec2coal, elec2crude, elec2ng, elec2pet, elec2re, elec2nuc, elec2hyd,
                   elec2elec_re, elec2elec_nu, elec2elec_hy, elec2elec_com, 
                   elec2heat_re, elec2heat_nu, elec2heat_hy, elec2heat_com, elec2loss)
    
    if (sum(elec_list, na.rm = T) == 0) {next()}
    
    ELEC_RE <- elec_list * se.re
    ELEC_NU <- elec_list * se.nu
    ELEC_HY <- elec_list * se.hy
    ELEC_COM <- elec_list * se.co
    
    mat <- rbind(ELEC_RE, ELEC_NU, ELEC_HY, ELEC_COM)
    colnames(mat) <- energy.names
    rownames(mat) <- c('ELEC_RE', 'ELEC_NU', 'ELEC_HY', 'ELEC_COM')
    
    saveRDS(mat, file.path(output, paste0(y, '/mat_', c, '.rds')))
  }
}

# Run program
for (c in iso.list) {
  print(paste0("Running scale_ELEC for: ", c))
  scale_elhe(c)
}

sink()
