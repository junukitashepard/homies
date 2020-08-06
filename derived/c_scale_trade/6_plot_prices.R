###############
# Plot prices #
###############
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

wwd <- paste0(wd, '/derived/c_scale_trade/')
#sink(paste0(wwd, 'out/Make.txt'))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
input.web <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
figures <- "/data/jus3/GlobalIO/output/figures"
temp <- "/data/jus3/GlobalIO/temp"

library('magrittr')
library('dplyr')
library('RMariaDB')
library('ggplot2')
library('stringr')
library('hiofunctions')

# Import files
source(paste0(wwd, '1_import_BACI.R'))

baci <- subset(baci, q_e > 0)
baci$import_price <- (baci$v*1000)/baci$q_e #$/TJ
baci$import_price_ton <- (baci$v*1000)/baci$q #$/t

# Connect to country names
df <- left_join(baci, countries[c('baci.country', 'iso.country')], by = c('j' = 'baci.country'))

# Collapse to get import prices
df <- group_by(df, iso.country, energy, year) %>% 
      summarize(import_price = weighted.mean(import_price, q_e, na.rm = T),
                import_price_ton = weighted.mean(import_price_ton, q, na.rm = T))

df <- subset(df, !is.na(iso.country) & year == 2015)

# Check USA ($7253.326/TJ domestic crude)
usa <- subset(df, iso.country == 'USA' & energy == 'CRU')

# Check China ($83/t)
chn <- subset(df, iso.country == 'CHN' & energy == 'COAL')

