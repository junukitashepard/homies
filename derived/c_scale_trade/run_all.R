###################
# Run all scripts #
###################
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

# Run files
source(paste0(wwd, '1_import_BACI.R'))
source(paste0(wwd, '2_elec_importprice.R'))
source(paste0(wwd, '3_elec_convert.R'))
source(paste0(wwd, '4_primary_to_none.R'))
source(paste0(wwd, '5_none_to_energy.R'))

sink()
