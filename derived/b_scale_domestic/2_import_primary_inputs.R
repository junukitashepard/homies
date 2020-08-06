################################
# Import primary energy inputs #
################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/PRIM_inputs"
temp <- tempdir()
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('RMariaDB')
library('hiofunctions')

# Import WEB for primary energy sources #
#########################################
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)

for (e in primary.energy) {
  infile <- paste0(wd, "/derived/b_scale_domestic/SQL/", e, ".txt")
  sqlstmt <- readChar(infile, file.info(infile)$size)

  print(paste0("Bring in WEB data from SQL: ", e))
  sql_import(statement = sqlstmt,
             outname = 'out')

  print("Collapsing data")
  out$Value <- as.numeric(out$Value)
  out <- dplyr::group_by(out, Country, Time, Flow) %>%
    dplyr::summarise(Value = sum(Value, na.rm = T))
  names(out) <- c('country', 'year', 'flow', 'value')

  print("Link ISO country code")
  out <- left_join(out, web.countries, by = c('country' = 'web.country'))
  out <- subset(out, !is.na(iso.country))

  assign(paste0('web.', e), out, envir = parent.frame())
  write.csv(out, file.path(output, paste0(e, ".csv")))
}

# Import mining data for nuclear #
##################################
mine <- read.csv(file.path(input, 'uranium_production.csv'), stringsAsFactors = F)

# Convert tons to joules using heating value
mine$value <- mine$production * hv.uranium

mine$flow <- 'Main activity producer electricity plants (transf.)' # Assume mined uranium goes to electricity
mine <- left_join(mine, web.countries, by = c('iso.country'))

mine <- mine[c('web.country', 'year', 'flow', 'value', 'iso.country', 'baci.country')]
names(mine) <- c('country', 'year', 'flow', 'value', 'iso.country', 'baci.country')

write.csv(mine, file.path(output, 'NUC.csv'))

