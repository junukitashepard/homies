#################
# Invert matrix #
#################
rm(list = ls())

wd <- "~/global_energy/"
cwd <- "~/global_energy/derived/d_combine_matrices/"
setwd(wd)

source(paste0(wd, "/derived/0_globals.R"))

raw <-  paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
input <- "/data/jus3/GlobalIO/output/analysis"
output <- "/data/jus3/GlobalIO/output/analysis"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('reshape2')
library('hiofunctions')

#############################################################
# Function: convert matrix longways
mat2longdf <- function(in.mat, outfile) {

  assign('mat', get(in.mat))

  # Convert long-way
  assign('df', setNames(melt(mat), c('from', 'to', 'value')))

  df$from.country <- substr(df$from, 1, 3)
  df$to.country <- substr(df$to, 1, 3)

  df$from.sector <- gsub("^.*\\.", "", df$from)
  df$to.sector <- gsub("^.*\\.", "", df$to)

  df$from.unit <- df$to.unit <- NA
  df$from.unit[df$from.sector %in% energy.names] <- df$to.unit[df$to.sector %in% energy.names] <- 'TJ'
  df$from.unit[!(df$from.sector %in% energy.names)] <- df$to.unit[!(df$to.sector %in% energy.names)] <- '$'

  for (i in 1:26) {
    df$from.sector[df$from.sector == i] <- df$to.sector[df$to.sector == i] <- eora.sectors[i]
  }

  #write.csv(df, file.path(output, paste0(outfile)))
  saveRDS(df, file.path(output, paste0(outfile)))
}

for (year in year.min:year.max) {

  print(paste0('Invert, YEAR = ', year))
  assign('A', readRDS(file.path(input, paste0('A_mat/A_', year, '.rds'))))
  A[is.na(A)] <- 0

  # Subset if there is no energy data for country in A
  no_energy.list <- list()
  for (c in iso.list) {
    assign('mat', A[grepl(c, rownames(A)), grepl(c, colnames(A))])
    if (nrow(mat) == 0 & ncol(mat) == 0) {no_energy.list <- c(no_energy.list, c)}
    if (nrow(mat) != 0 & ncol(mat) != 0) {
      mat <- mat[1:length(energy.names), 1:length(energy.names)]
      if (sum(mat) == 0) {no_energy.list <- c(no_energy.list, c)}
    }
  }

  A <- A[!(substr(rownames(A), 1, 3) %in% no_energy.list),
         !(substr(colnames(A), 1, 3) %in% no_energy.list)]

  #
  # # 2011: Finland submatrix is computationally singular
  # if (year == 2011) {A <- A[substr(rownames(A), 1, 3) != 'FIN', substr(colnames(A), 1, 3) != 'FIN']}
  # if (year == 2005) {A <- A[substr(rownames(A), 1, 3) != 'GBR', substr(colnames(A), 1, 3) != 'GBR']}

  I <- diag(1, nrow = nrow(A), ncol = ncol(A))
  environment(assert) <- environment()
  quiet(assert('nrow(A) == ncol(A)'))
  quiet(assert('dim(I) == dim(A)'))

  # Invert
  assign('L', I-A)
  L[L < (-1*2e5)] <- 0

  L <- solve(L)

  saveRDS(L, file.path(output, paste0('L_mat/L_', year, '.rds')))

  # Convert long-way
  mat2longdf(in.mat = 'L', outfile = paste0('L_df/L_', year, '.rds'))
  mat2longdf(in.mat = 'A', outfile = paste0('A_df/A_', year, '.rds'))

}

