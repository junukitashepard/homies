# Convert World Bank data to long #
###################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/analysis/"
output <- "/data/jus3/GlobalIO/output/analysis/"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/figures/x_energy_security"

library('magrittr')
library('dplyr')
library('hiofunctions')
library('reshape2')
library('ggsci')
library('lubridate')
library('ggplot2')

# Read file
gdp <- read.csv(file.path(raw, 'WorldBank/WorldBank_GDP_PerCapita.csv'), stringsAsFactors = F)

gdp.out <- data.frame()

for (y in 1960:2018) {
  df <- gdp[c('Country.Code', paste0('X', y))]
  names(df) <- c('country', 'gdp_percapita')
  df$year <- y
  gdp.out <- rbind(as.data.frame(gdp.out), as.data.frame(df))
}

write.csv(gdp.out, file.path(raw, 'WorldBank/WorldBank_GDP_PerCapita.csv'))