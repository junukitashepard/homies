#################################
# Regress C2E on cluster output #
#################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('ggplot2')
library('gridExtra')
library('cluster')
##########################################################
# Import UNSD Energy statistics
# We do not include (but can add): aviationgasoline, motorgasoline,
energy.list <- c('anthracite', 'biodiesel', 'biogases', 'biogasoline', 'browncoal', 'charcoal',
                 'crude', 'crudeoffshore', 'electricity_combustible', 'electricity_geothermal', 'electricity_hydro',
                 'electricity_nuclear', 'electricity_tidal', 'electricity_wind', 'lignite','naturalgas', 'peat')

for (e in energy.list) {
  print("*************************")
  print(paste0("Reading file: ", e))
  print("*************************")
  
  df <- read.csv(file.path(raw, paste0("UNSD_EnergyStats/production/", e, ".csv")), stringsAsFactors = F)
  df <- subset(df, grepl("production", Commodity...Transaction) | grepl("Production", Commodity...Transaction) |
                 grepl("net installed capacity", Commodity...Transaction))
  print("Commodity transaction: ")
  print(table(df$Commodity...Transaction))
  
  df <- df[c('Country.or.Area', 'Year', 'Quantity')]
  names(df) <- c('country', 'year', e)
  isid('df', c('country', 'year'))
  assign(e, df, envir = parent.frame())
}

# Combine files to master file
basedf <- dplyr::full_join(anthracite, biodiesel, by = c('country', 'year'))
templist <- energy.list[!(energy.list %in% c('anthracite', 'biodiesel'))]

for (e in templist) {
  assign('df', get(e))
  basedf <- dplyr::full_join(basedf, df, by = c('country', 'year'))
}
rm(templist)

# Import C2E
c2e <- read.csv(file.path(output, "C2E.csv"), stringsAsFactors = F)
  c2e$X <- NULL
  
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)
  basedf <- left_join(basedf, web.countries, by = c('country' = 'web.country'))
  basedf$country <- NULL

# Import demographics
demo <- read.csv(file.path(raw, "WorldBank/WB_GDP_Pop.csv"), stringsAsFactors = F)

# Combine files    
regdf <- inner_join(c2e, basedf, by = c('country' = 'iso.country', 'year' = 'year'))
regdf <- inner_join(regdf, demo, by = c('country' = 'country_code', 'year' = 'year'))

isid('regdf', c('country', 'year', 'Eora_Sector'))

regdf[is.na(regdf)] <- 0
fit <- lm(perc_sector_to_elec ~ anthracite + biodiesel + biogases + biogasoline + browncoal + charcoal + crude + crudeoffshore + 
            electricity_combustible + electricity_geothermal + electricity_hydro + electricity_nuclear + electricity_tidal + 
            electricity_wind + lignite + naturalgas + peat + 
            GDP + population +
            factor(year) + factor(Eora_Sector) + factor(country),
          data = regdf)
summary(fit)


check <- group_by(regdf, country, Eora_Sector) %>%
         summarise(perc_sector_to_coal = mean(perc_sector_to_coal, na.rm = T),
                   perc_sector_to_crude.ng = mean(perc_sector_to_crude.ng, na.rm = T))

check <- subset(regdf, country == 'USA')
# check$totmine <- check$anthracite + check$crude + check$crudeoffshore + check$lignite + check$naturalgas
# check$share.coal <- (check$anthracite + check$lignite)/check$totmine
# check$share.crng <- (check$crude + check$crudeoffshore + check$naturalgas)/check$totmine


ggplot(aes(x = year, y = perc_sector_to_elec, colour = factor(Eora_Sector)), data = check) + 
  geom_line()


  
