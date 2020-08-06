###################################
# Add global prices to mine share #
###################################
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
library('hiofunctions')
#######################
# Import BACI from SQL
sql_import(statement = 'SELECT * FROM MINERVA.BACI_ALLYEARS WHERE MINERVA.BACI_ALLYEARS.t > 2011 AND MINERVA.BACI_ALLYEARS.t < 2017',
           outname = 'baci',
           dbname = 'MINERVA')

# Mine product names
mine.names <- read.csv(file.path(raw, 'BACI/Mining2HS4.csv'), stringsAsFactors = F)

baci$hs4 <- substr(baci$hs6, 1, 4)
baci <- subset(baci, hs4 %in% mine.names$hs4)

# Obtain price (per ton)
baci$price <- baci$v/baci$q

# Add mineral names and country names (ISO)
baci$hs4 <- as.numeric(baci$hs4)
baci <- left_join(baci, mine.names, by = c('hs4'))

# Collapse by exporter, year, mineral
baci <- dplyr::group_by(baci, i, t, mineral) %>%
        dplyr::summarise(price = mean(price, na.rm = T),
                         sum.v = sum(v, na.rm = T))

web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)[2:4]
web.countries <- unique(web.countries[c('iso.country', 'baci.country')])

baci <- inner_join(baci, web.countries, by = c('i' = 'baci.country'))

isid('baci', c('i', 't', 'mineral'))

write.csv(baci, file.path(temp, "mine_export_prices.csv"))
