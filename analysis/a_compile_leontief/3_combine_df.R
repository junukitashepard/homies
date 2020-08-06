##################################
# Combine data for visualization #
##################################
rm(list = ls())

wd <- "~/global_energy/"
cwd <- "~/global_energy/derived/d_combine_matrices/"
setwd(wd)

source(paste0(wd, "/derived/0_globals.R"))

raw <-  paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
input <- "/data/jus3/GlobalIO/output/analysis"
output <- "/data/jus3/GlobalIO/output/analysis/uses_df"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('reshape2')
library('hiofunctions')

#############################################################
# Function: combine dataframes
combine_df <- function(year) {

  print(paste0('Combining YEAR = ', year))
  print('Reading files')
  L.df <- readRDS(file.path(input, paste0('L_df/L_', year, '.rds')))
  A.df <- readRDS(file.path(input, paste0('A_df/A_', year, '.rds')))

  x <- readRDS(file.path(input, paste0('x_vec/x_', year, '.rds')))
    x <- as.data.frame(x)
    names(x) <- 'total.output'
    x$to <- rownames(x)

  environment(assert) <- environment()
  quiet(assert('nrow(L.df) == nrow(A.df)'))

  print('Separating files')
  for (u in c('L', 'A')) {
    assign('df', get(paste0(u, '.df')))
    df$X <- NULL

    if (u == 'L') {names(df)[names(df) == 'value'] <- 'indirect.use'}
    if (u == 'A') {names(df)[names(df) == 'value'] <- 'direct.use'}
    e2c <- subset(df, from.sector %in% energy.names & to.sector %in% eora.sectors)
    e2e <- subset(df, from.sector %in% energy.names & to.sector %in% energy.names)
    c2c <- subset(df, from.sector %in% eora.sectors & to.sector %in% eora.sectors)
    c2e <- subset(df, from.sector %in% eora.sectors & to.sector %in% energy.names)

    for (d in c('e2c', 'e2e', 'c2c', 'c2e')) {
      assign(paste0(u, '.', d), get(d), envir = parent.frame())
    }
  }

  print('Recombining files')
  for (d in c('e2c', 'e2e', 'c2c', 'c2e')) {
    assign('l', get(paste0('L.', d)))
    assign('a', get(paste0('A.', d)))

    assign('out', inner_join(l, a, by = c('from', 'to', 'from.country', 'to.country', 'from.sector', 'to.sector', 'from.unit', 'to.unit')))

    # Insert total output
    out <- left_join(out, x, by = c('to'))

    saveRDS(out, file.path(output, paste0(d, '_', year, '.rds')))
    assign(paste0(d, '_', year), out, envir = parent.frame())
  }
}

# Run programs
for (y in year.min:year.max) {
  combine_df(year = y)
}


