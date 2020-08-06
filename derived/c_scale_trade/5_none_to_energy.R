#####################################################
# Scale non-energy to energy, based on domestic use #
#####################################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

input <- paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('hiofunctions')

sink(paste0(wd, '/derived/c_scale_trade/out/5_none_t_energy.txt'))

#######################
# Function: scale non-energy to energy and non-energy matrices
none_to_none <- function(import.country, y) {
  
  suppressWarnings(dir.create(file.path(output, paste0('TRADE_NONE_submat/', y))))
  suppressWarnings(dir.create(file.path(output, paste0('TRADE_NONE_submat/', y, '/', import.country))))
  print(paste0('Calibrating trade data for IMPORTER = ', import.country))
  
  # Import energy to non-energy domestic matrices
  assign('dom.none', readRDS(file.path(output, 'NONE_submat', paste0(y, '/mat_', import.country, '.rds'))))
  assign('total.use', rowSums(dom.none))
  dom.none <- dom.none/total.use # Shares of non-energy industries to energy and non-energy industries
  dom.none[is.nan(dom.none)] <- 0
  
  for (c in iso.list[iso.list != import.country]) {
    
    print(paste0('EXPORTER = ', c))
    assign('trade.mat', eora.T[grepl(c, rownames(eora.T)), grepl(import.country, rownames(eora.T))])
    
    if (nrow(trade.mat) == 26) {
      assign('trade.vec', rowSums(trade.mat)) # Trade flow from i to j
      assign('mat', dom.none * trade.vec)
    } else {
      print('Exporter not in Eora')
      assign('mat', matrix(0, nrow = 26, ncol = length(energy.names) + 26))
    }
    
    # Assign row names
    assign('rnames', list())
    for (i in 1:26) {
      rnames[i] <- paste0(c, '.', i)
    }
    rownames(mat) <- rnames
    
    # Assign column names
    assign('cnames', list())
    for (i in 1:length(energy.names)) {
      cnames[i] <- paste0(import.country, '.', energy.names[i])
    }
    for (i in 1:26) {
      cnames[i + length(energy.names)] <- paste0(import.country, '.', i)
    }
    colnames(mat) <- cnames
    
    saveRDS(mat, file.path(output, paste0('TRADE_NONE_submat/', y, '/', import.country, '/mat_', c, 'to', import.country, '.rds')))
  }
}

# Run program #
###############
for (year in year.min:year.max) {
  print(paste0("Calibrating trade data for YEAR = ", year))
  import_eora(year = year)
  
  for (country in country.list) {
    if (file.exists(file.path(output, 'NONE_submat', paste0(year, '/mat_', country, '.rds'))) == F) {
      print ("NO DOMESTIC NON-ENERGY MATRIX FOUND!")
      next()
    }
    
    none_to_none(import.country = country, y = year)
  }
}

sink()