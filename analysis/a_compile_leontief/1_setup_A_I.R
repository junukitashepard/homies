###################
# Set up A matrix #
###################
rm(list = ls())

wd <- "~/global_energy/"
cwd <- "~/global_energy/derived/d_combine_matrices/"
setwd(wd)

source(paste0(wd, "derived/0_globals.R"))

raw <-  paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/analysis"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('hiofunctions')

######################################################
for (year in year.min:year.max) {

  print(paste0('Setting up, YEAR = ', year))
  print('Import matrix T')
  assign('T', readRDS(file.path(input, paste0('IO_MAT/io_mat_', year, '.rds'))))
  environment(assert) <- environment()
  quiet(assert('nrow(T) == ncol(T)'))

  print('Import labels for Eora T')
  assign('eora.T.labs', read.delim(file.path(raw, paste0(year, "/labels_T.txt")), header = F, stringsAsFactors = F))
    names(eora.T.labs) <- c('country', 'country.code', 'industries', 'sector')
    eora.T.labs <- eora.T.labs[c('country', 'country.code', 'industries', 'sector')]
    eora.T.labs <- dplyr::group_by(eora.T.labs, country, country.code) %>%
                   dplyr::group_by(sector = row_number())
    quiet(assert('max(eora.T.labs$sector) == 26', eora.T.labs))

  print('Import matrix "FD" (final demand)')
  assign('FD', readRDS(file.path(input, paste0('FD_MAT/fd_mat_', year, '.rds'))))

  assert('nrow(FD) == nrow(T)')

  assign('T_FD', cbind(FD, T))
  assign('x', rowSums(T_FD))
    names(x) <- rownames(T_FD)
  assert('length(x) == nrow(T_FD)')

  # Build A = FD * x^-1
  x[x == 0] <- NA
  assign('A', t(t(T)/x))
  A[is.nan(A) | is.infinite(A) | is.na(A)] <- 0
  quiet(assert('nrow(A) == ncol(A)'))

  saveRDS(A, file.path(output, paste0('A_mat/A_', year, '.rds')))
  saveRDS(x, file.path(output, paste0('x_vec/x_', year, '.rds')))
}



