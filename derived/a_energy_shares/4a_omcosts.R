#############################################
# Derive O&M Costs (1990-2003) using EIA-412
#############################################
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
library('ggplot2')
library('zoo')
library('hiofunctions')
#############################################
# Import and sum up O&M costs ($/year)
for (y in 1997:2000) {
  df <- read.csv(file.path(raw, paste0("EIA/EIA-412/eia412", y, ".csv")), stringsAsFactors = F)
  df$AMOUNT <- as.numeric(df$AMOUNT)

  sch7 <- subset(df, SN == 7)[c('SN', 'LN', 'CL', 'AMOUNT')]
  sch7$YEAR <- y

  sch7$energy[sch7$LN == 1] <- 'rankine'
  sch7$energy[sch7$LN == 2] <- 'nuclear'
  sch7$energy[sch7$LN == 3] <- 'hydro'

  sch7$CL[sch7$energy == 'nuclear' & sch7$CL == 1] <- 2 # O&M costs combined in column 1 for nuclear

  sch7 <- subset(sch7, !is.na(energy))

  sch7 <- group_by(sch7, CL, YEAR, energy) %>%
          summarise(AMOUNT = sum(AMOUNT, na.rm = T))

  sch7_ocosts <- subset(sch7, CL == 2)[c('AMOUNT', 'YEAR', 'energy')]
  sch7_mcosts <- subset(sch7, CL == 3)[c('AMOUNT', 'YEAR', 'energy')]

  names(sch7_ocosts) <- c('ocosts', 'year', 'energy')
  names(sch7_mcosts) <- c('mcosts', 'year', 'energy')

  for (t in c('rankine', 'nuclear', 'hydro')) {
    dfo <- subset(sch7_ocosts, energy == t)
    dfm <- subset(sch7_mcosts, energy == t)
    dfo <- dfo[c('ocosts', 'year')]
    dfm <- dfm[c('mcosts', 'year')]
    df <- full_join(dfo, dfm, by = c('year'))
    names(df) <- c(paste0('o_cost.', t), 'year', paste0('m_cost.', t))
    assign(paste0('line.', t), df, envir = parent.frame())
  }

  line <- full_join(line.nuclear, line.rankine, by = c('year'))
  line <- full_join(line, line.hydro, by = c('year'))

  line <- line[c('year', 'o_cost.rankine', 'o_cost.nuclear', 'o_cost.hydro', 'm_cost.rankine', 'm_cost.nuclear', 'm_cost.hydro')]

  assign(paste0('line.', y), line, envir = parent.frame())
}

for (y in 2001:2003) {
  sch7 <- read.csv(file.path(raw, paste0("EIA/EIA-412/eia412", y, "_sch7.csv")), stringsAsFactors = F)

  sch7$energy[sch7$LINENO == 2] <- 'nuclear'
  sch7$energy[sch7$LINENO == 3] <- 'hydro'
  sch7$energy[sch7$LINENO == 1] <- 'rankine'
  sch7 <- subset(sch7, !is.na(energy))

  sch7[grepl('COL', names(sch7))] <- lapply(sch7[grepl('COL', names(sch7))], function(x) as.numeric(x))

  sch7 <- group_by(sch7, energy) %>%
          summarise(col2 = sum(COL2AMOUNT, na.rm = T),
                    col3 = sum(COL3AMOUNT, na.rm = T))

  for (c in c('hydro', 'nuclear', 'rankine')) {
    df <- subset(sch7, energy == c)
    df$energy <- NULL
    names(df) <- c(paste0('o_cost.', c), paste0('m_cost.', c))
    df$year <- y
    assign(paste0('line.', c), df, envir = parent.frame())
  }
  line <- full_join(line.hydro, line.nuclear, by = c('year'))
  line <- full_join(line, line.rankine, by = c('year'))
 # line <- line[c('year', 'o_cost.rankine', 'o_cost.nuclear', 'o_cost.hydro', 'm_cost.rankine', 'm_cost.nuclear', 'm_cost.hydro')]

  assign(paste0('line.', y), line, envir = parent.frame())
}

df <- line.1997
for (y in 1998:2000) {
  assign('df', rbind(df, get(paste0('line.', y))), envir = parent.frame())
}

# Divide by annual generation
gen <- read.csv(file.path(raw, "EIA/eia_EPA_NetGeneration.csv"), stringsAsFactors = F)
gen <- gen[c('Year', 'Rankine', 'Nuclear', 'Hydro')]
names(gen) <- c('year', 'rankine', 'nuclear', 'hydro')

df <- left_join(df, gen, by = c('year'))

df$o_cost.hydro <- df$o_cost.hydro/(df$hydro*10^9)
df$m_cost.hydro <- df$m_cost.hydro/(df$hydro*10^9)

df$o_cost.nuclear <- df$o_cost.nuclear/(df$nuclear*10^9)
df$m_cost.nuclear <- df$m_cost.nuclear/(df$nuclear*10^9)

df$o_cost.rankine <- df$o_cost.rankine/(df$rankine*10^9)
df$m_cost.rankine <- df$m_cost.rankine/(df$rankine*10^9)

# Adjust for CPI inflation
cpi <- read.csv(file.path(raw, "ConversionTables/CPI_inflator.csv"), stringsAsFactors = F)
df <- left_join(df, cpi, by = c('year'))

df[grepl('o_cost.', names(df)) | grepl('m_cost.', names(df))] <-
  lapply(df[grepl('o_cost.', names(df)) | grepl('m_cost.', names(df))], function(x) x * df$cpi.scale)

write.csv(df, file.path(temp, "om_costs_1997-2003.csv"))

