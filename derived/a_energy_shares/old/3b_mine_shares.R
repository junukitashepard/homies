#####################################
# Mining shares (World Mining Data) #
#####################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('openxlsx')
#######################
# Import files
##############
# Country names
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)

# Mineral names
mine.names <- read.csv(file.path(raw, 'BACI/Mining2HS4.csv'), stringsAsFactors = F)

# Mineral export prices
export.prices <- read.csv(file.path(temp, 'mine_export_prices.csv'), stringsAsFactors = F)
export.prices$X <- NULL
isid('export.prices', c('i', 't', 'mineral'))

# World Mining Data
import_mine <- function(sheetnum, outfile) {
  df <- read.xlsx(file.path(raw, "WorldMining/6.4.Production_of_Mineral_Raw_Materials_of_individual_Countries_by_Minerals.xlsx"),
                    sheet = sheetnum, startRow = 2, colNames = T)
  df$type <- unique(mine.names$mineral)[sheetnum]
  
  df$Country[df$Country == 'Bosnia-Herzegovina'] <- 'Bosnia and Herzegovina'
  df$Country[df$Country == 'Brunei'] <- 'Brunei Darussalam'
  df$Country[df$Country == 'China'] <- "People's Republic of China"
  df$Country[df$Country == 'Congo, D.R.'] <- 'Dem. Republic of the Congo'
  df$Country[df$Country == 'Congo, Rep.'] <- 'Congo'
  df$Country[df$Country == "Cote d'Ivoire"] <- 'Cote dIvoire'
  df$Country[df$Country == "Iran"] <- 'Islamic Republic of Iran'
  df$Country[df$Country == "Korea, North"] <- "Dem. People's Rep. of Korea"
  df$Country[df$Country == "Korea, South"] <- 'Korea'
  df$Country[df$Country == "Macedonia"] <- 'Former Yugoslav Republic of Macedonia'
  df$Country[df$Country == "Russia"] <- 'Russian Federation'
  df$Country[df$Country == "Slovakia"] <- 'Slovak Republic'
  df$Country[df$Country == "Syria"] <- 'Syrian Arab Republic'
  df$Country[df$Country == "Vietnam"] <- 'Viet Nam'
  
  df <- inner_join(df, web.countries, by = c('Country' = 'web.country'))
  assign(outfile, df, envir = parent.frame())
}

# Sheets 1-55: non-energy mining
import_mine(1, 'mine')

for (s in 2:55) {
  import_mine(s, 'df')
  mine <- rbind(mine, df)
}

# Sheets 56-62: energy mining
import_mine(56, 'coal')
for (s in 57:58) {
  import_mine(s, 'df')
  coal <- rbind(coal, df)
}

import_mine(59, 'ng')

import_mine(60, 'crude')
for (s in 61:62) {
  import_mine(s, 'df')
  crude <- rbind(crude, df)
}

import_mine(63, 'nuc')
  
# Combine files
###############
basedf <- rbind(mine, coal, ng, crude, nuc)

alldf <- data.frame(country = character(0), baci.country = numeric(0), 
                    type = character(0), year = numeric(0), production = numeric(0))
for (y in 2012:2016) {
  df <- basedf[c('Country', 'baci.country', 'iso.country', 'unit', 'type', y)]
  names(df) <- c('country', 'baci.country', 'iso.country', 'unit', 'type', 'production')
  
  # Convert units (everything to tons)
  df$production[df$unit == 'ct'] <- df$production[df$unit == 'ct'] * (200*(10^(-9))) # 1ct = 200mg
  df$production[df$unit == 'kg'] <- df$production[df$unit == 'kg'] * .001 # 1kg = 0.001 tons
  df$production[df$unit == 'Mio m³'] <- df$production[df$unit == 'Mio m³'] * 0.0008 * 10^6 # 1m3NG = 0.0008 tons
  
  df$year <- y
  
  df <- df[c('country', 'baci.country', 'iso.country', 'type', 'year', 'production')]
  
  alldf <- rbind(alldf, df)
}

alldf$country <- NULL
alldf <- subset(alldf, !is.na(production))

# Add export values
###################
# Scale mine share by export prices, if mine share data are missing from World Mine Data use BACI total export value to scale
alldf <- full_join(alldf, export.prices, by = c('baci.country' = 'i', 'iso.country' = 'iso.country', 'year' = 't', 'type' = 'mineral'))
alldf$web.country <- NULL

check <- subset(alldf, !is.na(baci.country))
isid('check', c('baci.country', 'type', 'year'))
assert("!is.na(alldf$production) | !is.na(alldf$sum.v)")

# If price is missing for country (no exports, perhaps), don't scale by price
alldf$not_mi_price[!is.na(alldf$price)] <- 1

alldf <- dplyr::group_by(alldf, iso.country, type, year) %>%
         dplyr::mutate(not_mi_price = sum(not_mi_price, na.rm = T))
alldf$price[alldf$not_mi_price == 0] <- 1
alldf$not_mi_price <- NULL

# If production is missing for country, only use sum.v
alldf$not_mi_prod[!is.na(alldf$production)] <- 1
alldf <- dplyr::group_by(alldf, iso.country, type, year) %>%
         dplyr::mutate(not_mi_prod = sum(not_mi_prod, na.rm = T))
alldf$production[alldf$not_mi_prod == 0] <- alldf$sum.v[alldf$not_mi_prod == 0]
alldf$not_mi_prod <- NULL

# Scale by country-year
alldf <- dplyr::group_by(alldf, iso.country, year) %>%
         dplyr::mutate(tot.production = sum(production, na.rm = T))

alldf$prodnum <- alldf$production * alldf$price
alldf$prodden <- alldf$tot.production * alldf$price

alldf$share <- alldf$prodnum/alldf$prodden

# Keep energy
COAL <- subset(alldf, type == 'Steam coal' | type == 'Coking coal' | type == 'Lignite')
CRU <- subset(alldf, type == 'Oil Sands' | type == 'Oil Shales' | type == 'Petroleum')
NG <- subset(alldf, type == 'Natural Gas')
NUC <- subset(alldf, type == 'Uranium')

for (e in c('COAL', 'CRU', 'NG', 'NUC')) {
  assign('df', get(e))
  df <- dplyr::group_by(df, iso.country, year) %>%
        dplyr::summarise(share = sum(share))
  names(df) <- c('iso.country', 'year', paste0('share.', e))
  assign(e, df, envir = parent.frame())
}

# Assign NUC file to obtain domestically produced uranium
nuc.out <- NUC[c('iso.country', 'year', 'production')]
write.csv(nuc.out, file.path(output, 'uranium_production.csv'))

# Combine files
outdf <- full_join(COAL, CRU, by = c('iso.country', 'year'))
outdf <- full_join(outdf, NG, by =c('iso.country', 'year'))
outdf <- full_join(outdf, NUC, by =c('iso.country', 'year'))
outdf[is.na(outdf)] <- 0

write.csv(outdf, file.path(output, 'mine_share.csv'))