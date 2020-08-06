#####################################
# Energy production by country year #
#####################################
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
#####################################
# Import files
WEB1990_2008 <- read.csv(file.path(raw, "OECD_energybalance/energybalance_1990-2008.csv"), stringsAsFactors = F)
  WEB1990_2008 <- subset(WEB1990_2008, Unit == "TJ")
  WEB1990_2008 <- WEB1990_2008[c('Country', 'Product', 'Time', 'Value', 'Flow')]

for (y in 2009:2015) {
  assign('web', 
         read.csv(file.path(raw, paste0("OECD_energybalance/2009-2016/WEB_", y, ".csv")), stringsAsFactors = F),
         envir = parent.frame())
  web <- subset(web, Unit == "TJ")
  assign(paste0('WEB', y), web[c('Country', 'Product', 'Time', 'Value', 'Flow')])
}

# Combine all years
web <- rbind(WEB1990_2008, WEB2009, WEB2010, WEB2011, WEB2012, WEB2013, WEB2014, WEB2015)
rm(list = c('WEB1990_2008', 'WEB2009', 'WEB2010', 'WEB2011', 'WEB2012', 'WEB2013', 'WEB2014', 'WEB2015'))
names(web) <- c('country', 'energy.type', 'year', 'production', 'flow')

# Share 1: Crude/Crude + NG and NG/Crude + NG
web.crude.ng <- subset(web, (energy.type == 'Crude, NGL and feedstocks' | energy.type == 'Natural gas') &
                            (flow == "Production"))
web.crude.ng <- dplyr::group_by(web.crude.ng, country, year) %>%
                dplyr::mutate(tot.crude.ng = sum(production, na.rm = T))
web.crude.ng$share <- web.crude.ng$production/web.crude.ng$tot.crude.ng

web.crude <- unique(subset(web.crude.ng, energy.type == "Crude, NGL and feedstocks")[c('country', 'year', 'share')])
  names(web.crude) <- c('country', 'year', 'shareCrNG.crude')
web.ng <- unique(subset(web.crude.ng, energy.type == "Natural gas")[c('country', 'year', 'share')])
  names(web.ng) <- c('country', 'year', 'shareCrNG.ng')
web.shareCrNG <- dplyr::full_join(web.crude, web.ng, by = c('country', 'year'))
isid('web.shareCrNG', c('country', 'year'))
rm(list = c('web.crude', 'web.ng', 'web.crude.ng'))

# Share 2: Crude + NG/Mining and quarrying and Coal/Mining and quarrying
# Assumption: Mining and quarrying include coal, NG, crude oil, oil shale
web.mining <- subset(web, (energy.type == "Crude, NGL and feedstocks" | energy.type == "Natural gas" |
                           energy.type == "Coal and coal products" | energy.type == "Oil shale and oil sands") &
                           flow == "Production")
web.mining <- dplyr::group_by(web.mining, country, year) %>%
              dplyr::mutate(tot.mining = sum(production, na.rm = T))
web.mining$share <- web.mining$production/web.mining$tot.mining

web.crude <- unique(subset(web.mining, energy.type == "Crude, NGL and feedstocks")[c('country', 'year', 'share')])
web.ng <- unique(subset(web.mining, energy.type == "Natural gas")[c('country', 'year', 'share')])
web.coal <- unique(subset(web.mining, energy.type == "Coal and coal products")[c('country', 'year', 'share')])

names(web.crude) <- c('country', 'year', 'shareMine.crude')
names(web.ng) <- c('country', 'year', 'shareMine.ng')
names(web.coal) <- c('country', 'year', 'shareMine.coal')

web.shareMine <- dplyr::full_join(web.crude, web.ng, by = c('country', 'year'))
web.shareMine <- dplyr::full_join(web.shareMine, web.coal, by = c('country', 'year'))
isid('web.shareMine', c('country', 'year'))
rm(list = c('web.crude', 'web.ng', 'web.coal', 'web.mining'))

# Share 3: RE electricity/all electricity and heat, non-RE electricity/all electricity and heat
web.elec <- subset(web, flow == "Electricity output (GWh)")

web.RE <- subset(web.elec, energy.type == "Solar/wind/other" | energy.type == "Nuclear")
web.RE <- dplyr::group_by(web.RE, country, year) %>%
          dplyr::summarise(production = sum(production, na.rm = T))

web.tot <- subset(web.elec, energy.type == "Total")

web.elec <- dplyr::full_join(web.RE, web.tot, by = c('country', 'year'))
web.elec$shareElec.RE <- web.elec$production.x/web.elec$production.y
web.shareElec <- unique(web.elec[c('country', 'year', 'shareElec.RE')])
web.shareElec$shareElec.nRE <- 1 - web.shareElec$shareElec.RE
isid('web.shareElec', c('country', 'year'))
rm(list = c('web.elec', 'web.RE', 'web.tot'))

# Combine production to one file
web.Prod <- subset(web, flow == "Production")
web.Prod.crude <- unique(subset(web.Prod, energy.type == "Crude, NGL and feedstocks")[c('country', 'year', 'production')])
web.Prod.coal <- unique(subset(web.Prod, energy.type == "Coal and coal products")[c('country', 'year', 'production')])
web.Prod.ng <- unique(subset(web.Prod, energy.type == "Natural gas")[c('country', 'year', 'production')])

names(web.Prod.coal) <- c('country', 'year', 'coal_production.TJ')
names(web.Prod.crude) <- c('country', 'year', 'crude_production.TJ')
names(web.Prod.ng) <- c('country', 'year', 'ng_production.TJ')

web.Prod <- subset(web, flow == "Electricity output (GWh)")
web.Prod.TOTelec <- unique(subset(web.Prod, energy.type == "Total")[c('country', 'year', 'production')])
web.Prod.REelec <- unique(subset(web.Prod, energy.type == "Solar/wind/other" | energy.type == "Nuclear")[c('country', 'year', 'production')])
  web.Prod.REelec <- dplyr::group_by(web.Prod.REelec, country, year) %>%
                     dplyr::summarise(production = sum(production, na.rm = T))
  
names(web.Prod.TOTelec) <- c('country', 'year', 'total_electricity.GWh')
names(web.Prod.REelec) <- c('country', 'year', 're_electricity.GWh')

web.Elec <- dplyr::full_join(web.Prod.REelec, web.Prod.TOTelec, by = c('country', 'year'))
web.Elec$conv_electricity.GWh <- web.Elec$total_electricity.GWh - web.Elec$re_electricity.GWh

web.Prod <- dplyr::full_join(web.Prod.crude, web.Prod.coal, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.Prod.ng, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.Elec, by = c('country', 'year'))
isid('web.Prod', c('country', 'year'))
rm(list = c('web.Prod.crude', 'web.Prod.coal', 'web.Prod.ng', 'web.Prod.TOTelec', 'web.Prod.REelec', 'web.Elec'))

# Combine production and shares to one file
web.all <- dplyr::full_join(web.Prod, web.shareCrNG, by = c('country', 'year'))
web.all <- dplyr::full_join(web.all, web.shareElec, by = c('country', 'year'))
web.all <- dplyr::full_join(web.all, web.shareMine, by = c('country', 'year'))
isid('web.all', c('country', 'year'))

write.csv(web.all, file.path(output, "energy_production_shares.csv"))




