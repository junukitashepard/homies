################
# Import files #
################
# Import BACI
print("Bring in BACI data from SQL")
library(RMariaDB)

baci <- data.frame(t = numeric(0), hs6 = character(0), i = numeric(0), j = numeric(0), v = numeric(0), q = numeric(0))
for (y in 1995:2015) {
  print(paste0('Importing: ', y))
  sql_import(statement = paste0("SELECT * FROM MINERVA.BACI_", y, " ",
                                "WHERE LEFT(MINERVA.BACI_", y, ".hs6, 2) = '27' OR ",
                                "LEFT(MINERVA.BACI_", y, ".hs6, 2) = '38' OR ",
                                "LEFT(MINERVA.BACI_", y, ".hs6, 2) = '28' OR ",
                                "LEFT(MINERVA.BACI_", y, ".hs6, 2) = '22' OR ",
                                "LEFT(MINERVA.BACI_", y, ".hs6, 2) = '26'"),
             dbname = "MINERVA",
             outname = 'df')
  
  df$hs6 <- as.character(df$hs6)
  assert('nchar(df$hs6) == 6')
  baci <- rbind(baci, df)
}

baci$q <- as.numeric(baci$q)

# Import code crosswalk
energyhs4 <- read.csv(file.path(raw, "BACI/energy2hs4.csv"), stringsAsFactors = F)
energyhs4$hs4 <- as.character(energyhs4$hs4)
energyhs4 <- subset(energyhs4, !is.na(hs4))
energyhs4$hs6 <- as.character(energyhs4$hs6)

# Import country crosswalk
countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)
countries <- countries[c('iso.country', 'web.country', 'baci.country')]

# Link BACI to crosswalks
baci <- left_join(baci, energyhs4, by = c('hs6'))
baci <- subset(baci, !is.na(energy))
baci <- left_join(baci, countries, by = c('i' = 'baci.country')) # exporter only

# Convert q to energy (TJ) using NCV from IEA 
ncv <- readRDS(file.path(input, 'ncv_convert.rds'))

ncv.r <- dplyr::group_by(ncv, product, energy) %>%
         dplyr::summarise(ncv = mean(ncv, na.rm = T)) # Where IEA missing country data, use overall average
names(ncv.r) <- c('product', 'energy', 'global.ncv')

baci <- left_join(baci, ncv, by = c('iso.country', 'hs6_desc' = 'product', 'energy'))
baci <- left_join(baci, ncv.r, by = c('hs6_desc' = 'product', 'energy' = 'energy'))

baci$global.ncv[baci$energy == 'NUC'] <- hv.uranium # Uranium is a global heating value

baci$ncv[is.na(baci$ncv)] <- baci$global.ncv[is.na(baci$ncv)]
baci$global.ncv <- NULL

check <- subset(baci, is.na(ncv))
assert('check$energy == "ELEC" | check$energy == "HYD"')

baci$q_e <- baci$ncv * baci$q

# Collapse to i-j-year-energy level
baci <- dplyr::group_by(baci, i, j, t, energy) %>%
        dplyr::summarise(v = sum(v, na.rm = T),
                         q = sum(q, na.rm = T),
                         q_e = sum(q_e, na.rm = T))
names(baci) <- c('i', 'j', 'year', 'energy', 'v', 'q', 'q_e')

# Save file in temp
saveRDS(baci, file.path(temp, "baci_energy.rds"))
saveRDS(baci, file.path(output, "baci_energy.rds"))

