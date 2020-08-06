#####################################
# Mining shares (World Mining Data) #
#####################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('openxlsx')
library('hiofunctions')

#######################
# Import files
##############
# Country names
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)

# Mineral names
mine.names <- read.csv(file.path(raw, 'BACI/Mining2HS4.csv'), stringsAsFactors = F)

# Mineral export prices
export.prices <- read.csv(file.path(temp, 'mine_export_prices.csv'), stringsAsFactors = F, strip.white = T)
export.prices$X <- NULL
isid('export.prices', c('i', 't', 'mineral'))
export.prices$mineral[export.prices$mineral == 'Steam coal '] <- 'Steam Coal'
export.prices$mineral[export.prices$mineral == 'Coking coal'] <- 'Coking Coal'

# World Mining Data
df <- read.csv(file.path(raw, 'WorldMining/pivot_countries_long.csv'), stringsAsFactors = F)
df <- subset(df, grepl('(kg)', mineral) == F & grepl('VAL', mineral) == F & grepl('SKE', mineral) == F & grepl('REE', mineral) == F)

# Reassign names
df$country[df$country == 'Bosnia-Herzegovina'] <- 'Bosnia and Herzegovina'
df$country[df$country == 'Brunei'] <- 'Brunei Darussalam'
df$country[df$country == 'China'] <- "People's Republic of China"
df$country[df$country == 'Congo, D.R.'] <- 'Dem. Republic of the Congo'
df$country[df$country == 'Congo, Rep.'] <- 'Congo'
df$country[df$country == "Cote d'Ivoire"] <- 'Cote dIvoire'
df$country[df$country == "Iran"] <- 'Islamic Republic of Iran'
df$country[df$country == "Korea, North"] <- "Dem. People's Rep. of Korea"
df$country[df$country == "Korea, South"] <- 'Korea'
df$country[df$country == "Macedonia"] <- 'Former Yugoslav Republic of Macedonia'
df$country[df$country == "Russia"] <- 'Russian Federation'
df$country[df$country == "Slovakia"] <- 'Slovak Republic'
df$country[df$country == "Syria"] <- 'Syrian Arab Republic'
df$country[df$country == "Vietnam"] <- 'Viet Nam'

df <- inner_join(df, web.countries, by = c('country' = 'web.country'))

df <- unique(df[c('country', 'baci.country', 'iso.country', 'mineral', 'year', 'production')])
df <- dplyr::group_by(df, country, baci.country, iso.country, mineral, year) %>%
      dplyr::summarize(production = sum(production))
isid('df', c('iso.country', 'mineral', 'year'))

# Add export values
###################
# Scale mine share by export prices, if mine share data are missing from World Mine Data use BACI total export value to scale
alldf <- full_join(df, export.prices, by = c('baci.country' = 'i', 'iso.country' = 'iso.country', 'year' = 't', 'mineral' = 'mineral'))
alldf$web.country <- NULL

check <- subset(alldf, !is.na(baci.country))
isid('check', c('baci.country', 'mineral', 'year'))
assert("!is.na(alldf$production) | !is.na(alldf$sum.v)")

# If price is missing for country (no exports, perhaps), don't scale by price
alldf$not_mi_price[!is.na(alldf$price)] <- 1

alldf <- dplyr::group_by(alldf, iso.country, mineral, year) %>%
  dplyr::mutate(not_mi_price = sum(not_mi_price, na.rm = T))
alldf$price[alldf$not_mi_price == 0] <- 1
alldf$not_mi_price <- NULL

# If production is missing for country, only use sum.v
alldf$not_mi_prod[!is.na(alldf$production)] <- 1
alldf <- dplyr::group_by(alldf, iso.country, mineral, year) %>%
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
COAL <- subset(alldf, mineral == 'Steam Coal' | mineral == 'Coking Coal' | mineral == 'Lignite')
CRU <- subset(alldf, mineral == 'Oil Sands' | mineral == 'Oil Shales' | mineral == 'Petroleum')
NG <- subset(alldf, mineral == 'Natural Gas')
NUC <- subset(alldf, mineral == 'Uranium')

# Assign NUC file to obtain domestically produced uranium
nuc.out <- NUC[c('iso.country', 'year', 'production')]
write.csv(nuc.out, file.path(output, 'uranium_production.csv'))

for (e in c('COAL', 'CRU', 'NG', 'NUC')) {
  assign('df', get(e))
  df <- dplyr::group_by(df, iso.country, year) %>%
    dplyr::summarise(share = sum(share))
  names(df) <- c('iso.country', 'year', paste0('share.', e))
  assign(e, df, envir = parent.frame())
}


# Combine files
outdf <- full_join(COAL, CRU, by = c('iso.country', 'year'))
outdf <- full_join(outdf, NG, by =c('iso.country', 'year'))
outdf <- full_join(outdf, NUC, by =c('iso.country', 'year'))
outdf[is.na(outdf)] <- 0

write.csv(outdf, file.path(output, 'mine_share.csv'))
