######################
# Dissect Eora table #
######################
rm(list = ls())

wd <- "~/global_energy/"
cwd <- "~/global_energy/derived/d_combine_matrices/"
setwd(wd)

source(paste0(wd, "/derived/0_globals.R"))
source(paste0(cwd, '1_setup_emptymat.R'))
source(paste0(cwd, '2_insert_domestic_submat.R'))
source(paste0(cwd, '3_insert_trade_submat.R'))

input <- paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('hiofunctions')

sink(paste0(cwd, 'out/out.txt'))

######################################################
# Separate and recombine Eora and energy submatrices #
######################################################
for (year in year.min:year.max) {
  print('###############################')
  print(paste0('YEAR = ', year))
  print('###############################')
  import_eora(year = year) # Import Eora matrices
  assign(paste0('mat.', year), setup.emptymat()) # Set up empty matrix for given year

  for (c in country.list) {
    print(paste0('Inserting ', c))

    quiet(setup.domestic(country = c, year = year)) # Insert domestic submatrix
    quiet(setup.trade(country = c, year = year)) # Insert trade submatrix
  }

  assign('outmat', get(paste0('mat.', year)))
  saveRDS(outmat, file.path(output, paste0('IO_MAT/io_mat_', year, '.rds')))
}


sink()