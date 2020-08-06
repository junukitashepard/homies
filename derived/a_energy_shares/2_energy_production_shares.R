#####################################
# Energy production by country year #
#####################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('hiofunctions')
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

web$flow <- stringr::str_replace_all(web$flow, '"', '')
web$energy.type <- stringr::str_replace_all(web$energy.type, '"', '')

# Write file for later use
write.csv(web, file.path(temp, "web_all.csv"))

# Share 1: Crude/Crude + NG and NG/Crude + NG
web.crude.ng <- subset(web, (energy.type == 'Crude, NGL and feedstocks' | energy.type == 'Natural gas') &
                            (flow == "Production"))
web.crude.ng <- dplyr::group_by(web.crude.ng, country, year) %>%
                dplyr::mutate(tot.crude.ng = sum(production, na.rm = T))
web.crude.ng$share <- web.crude.ng$production/web.crude.ng$tot.crude.ng
web.crude.ng$share[web.crude.ng$tot.crude.ng == 0] <- 0

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

web.mining$type[web.mining$energy.type == "Crude, NGL and feedstocks" | web.mining$energy.type == "Oil shale and oil sands"] <- "crude"
web.mining$type[web.mining$energy.type == "Natural gas"] <- "ng"
web.mining$type[web.mining$energy.type == "Coal and coal products"] <- "coal"
assert('!is.na(web.mining$type)', 'web.mining')

web.mining <- dplyr::group_by(web.mining, country, year, type) %>%
              dplyr::summarise(production = sum(production, na.rm = T))

web.mining <- dplyr::group_by(web.mining, country, year) %>%
              dplyr::mutate(tot.mining = sum(production, na.rm = T))

web.mining$share <- web.mining$production/web.mining$tot.mining
web.mining$share[web.mining$tot.mining == 0] <- 0

web.crude <- unique(subset(web.mining, type == "crude")[c('country', 'year', 'share')])
web.ng <- unique(subset(web.mining, type == "ng")[c('country', 'year', 'share')])
web.coal <- unique(subset(web.mining, type == "coal")[c('country', 'year', 'share')])

names(web.crude) <- c('country', 'year', 'shareMine.crude')
names(web.ng) <- c('country', 'year', 'shareMine.ng')
names(web.coal) <- c('country', 'year', 'shareMine.coal')

web.shareMine <- dplyr::full_join(web.crude, web.ng, by = c('country', 'year'))
web.shareMine <- dplyr::full_join(web.shareMine, web.coal, by = c('country', 'year'))
isid('web.shareMine', c('country', 'year'))
rm(list = c('web.crude', 'web.ng', 'web.coal', 'web.mining'))

# Share 3: RE elec/all elec and heat, nuclear elec/all elec and heat, hydro elec/all elec and heat,non-RE elec/all elec and heat
shares.EH <- function(flow.type, energy, envir = parent.frame()) {

  assign('d', subset(web, flow == flow.type))

  assign('web.RE', unique(subset(d, energy.type == "Solar/wind/other")))
  assign('web.NU', unique(subset(d, energy.type == "Nuclear")))
  assign('web.HY', unique(subset(d, energy.type == "Hydro")))

  for (f in c('RE', 'NU', 'HY')) {
    assign('df', get(paste0('web.', f)))
    df <- dplyr::group_by(df, country, year) %>%
          dplyr::summarise(production = sum(production, na.rm = T))
    assign(paste0('web.', f), df)
  }

  web.tot <- subset(d, energy.type == "Total")[c('country', 'year', 'production')]

  d <- dplyr::full_join(web.tot, web.RE, by = c('country', 'year'))
  d <- dplyr::full_join(d, web.NU, by = c('country', 'year'))
  d <- dplyr::full_join(d, web.HY, by = c('country', 'year'))
  names(d) <- c('country', 'year', 'prod.TOT', 'prod.RE', 'prod.NU', 'prod.HY')
  d <- unique(d)

  d$share.RE <- d$prod.RE/d$prod.TOT
  d$share.NU <- d$prod.NU/d$prod.TOT
  d$share.HY <- d$prod.HY/d$prod.TOT

  d$share.RE[d$prod.TOT == 0] <- d$share.NU[d$prod.TOT == 0] <- d$share.HY[d$prod.TOT == 0] <- 0

  d$share.COM <- 1 - (d$share.RE + d$share.NU + d$share.HY)
  names(d) <- c('country', 'year', paste0('prod.', energy, '.TOT'),
                paste0('prod.', energy, '.RE'), paste0('prod.', energy, '.NU'), paste0('prod.', energy, '.HY'),
                paste0('share.', energy, '.RE'), paste0('share.', energy, '.NU'), paste0('share.', energy, '.HY'),
                paste0('share.', energy, '.COM'))

 # isid('d', c('country', 'year'))
  assign(paste0('web.', energy), d, envir = parent.frame())
}
shares.EH(flow.type = "Electricity output (GWh)", energy = "elec")
shares.EH(flow.type = "Heat output", energy = "heat")

# Share 4: Oil flow to petroleum (compared to chemical/petrochemical, non-metallic minerals)
df.pet <- subset(web, energy.type == 'Oil products' & flow == 'Oil refineries')
  names(df.pet) <- c('country', 'energy.type', 'year', 'petroleum', 't')
df.chem <- subset(web, energy.type == 'Oil products' & flow == 'Chemical and petrochemical')
  names(df.chem) <- c('country', 'energy.type', 'year', 'chemical', 't')
df.nonmet <- subset(web, energy.type == 'Oil products' & flow == 'Non-metallic minerals')
  names(df.nonmet) <- c('country', 'energy.type', 'year', 'nonmetallic', 't')
df.pet$t <- df.chem$t <- df.nonmet$t <- NULL
df.all <- full_join(df.pet, df.chem, by = c('country', 'year', 'energy.type'))
df.all <- full_join(df.all, df.nonmet, by = c('country', 'year', 'energy.type'))
df.all$share.pet <- df.all$petroleum/(df.all$petroleum + df.all$chemical + df.all$nonmetallic)
df.all$share.pet[df.all$petroleum == 0 & df.all$chemical == 0 & df.all$nonmetallic == 0] <- 1
  assert('!is.nan(df.all$share.pet)')
web.pet <- unique(df.all[c('country', 'year', 'share.pet')])
rm(list = c('df.pet', 'df.chem', 'df.nonmet', 'df.all'))

# Combine production to one file
web.Prod <- subset(web, flow == "Production")
web.Prod.crude <- unique(subset(web.Prod, energy.type == "Crude, NGL and feedstocks")[c('country', 'year', 'production')])
web.Prod.coal <- unique(subset(web.Prod, energy.type == "Coal and coal products")[c('country', 'year', 'production')])
web.Prod.ng <- unique(subset(web.Prod, energy.type == "Natural gas")[c('country', 'year', 'production')])

names(web.Prod.coal) <- c('country', 'year', 'coal_production.TJ')
names(web.Prod.crude) <- c('country', 'year', 'crude_production.TJ')
names(web.Prod.ng) <- c('country', 'year', 'ng_production.TJ')

isid('web.elec', c('country', 'year'))
isid('web.heat', c('country', 'year'))
isid('web.pet', c('country', 'year'))

web.Prod <- dplyr::full_join(web.Prod.crude, web.Prod.coal, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.Prod.ng, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.elec, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.heat, by = c('country', 'year'))
web.Prod <- dplyr::full_join(web.Prod, web.pet, by = c('country', 'year'))

isid('web.Prod', c('country', 'year'))
#rm(list = c('web.Prod.crude', 'web.Prod.coal', 'web.Prod.ng', 'web.elec', 'web.heat'))

# Combine production and shares to one file
web.all <- dplyr::full_join(web.Prod, web.shareCrNG, by = c('country', 'year'))
web.all <- dplyr::full_join(web.all, web.shareMine, by = c('country', 'year'))
isid('web.all', c('country', 'year'))

# Fix names
web.all$country[web.all$country == "Côte d'Ivoire"] <- "Cote dIvoire"
web.all$country[web.all$country == "Curaçao"] <- "Curacao"
web.all <- unique(web.all)

# Add ISO countries
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)
web.all <- inner_join(web.all, web.countries, by = c('country'= 'web.country'))

isid('web.all', c('country', 'year'))

# Change later: assumet 2014 shares hold for 2015
shares_2015 <- subset(web.all, year == 2014) 
shares_2015$year <- 2015
web.all <- rbind(web.all, shares_2015)

write.csv(web.all, file.path(output, "energy_production_shares.csv"))




