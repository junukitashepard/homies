#######################################################
# Convert quantity electricity to disaggregated units #
#######################################################
# Import files #
################
baci <- readRDS(file.path(temp, 'baci_energy.rds'))
elecprices <- readRDS(file.path(temp, 'elec_tradeprices.rds'))[c('iso', 'year', 'mean_rate')]
energy_shares <- read.csv(file.path(output, 'energy_production_shares.csv'), stringsAsFactors = F)

# Merge files
baci_elec <- subset(baci, grepl('ELEC', energy))
baci_elec$energy <- NULL
baci_elec <- unique(baci_elec)
baci_elec <- left_join(baci_elec, elecprices, by = c('iso.i' = 'iso', 'year'))
baci_elec <- left_join(baci_elec, elecprices, by = c('iso.j' = 'iso', 'year'))
names(baci_elec) <- c('i', 'j', 'year', 'v', 'q', 'q_e', 'iso.i', 'iso.j', 'rate.i', 'rate.j')

# Get bilateral mean rate
baci_elec$mean_rate <- (baci_elec$rate.i + baci_elec$rate.j)/2
baci_elec$q_e <- baci_elec$v/baci_elec$mean_rate # TJ

# Split into different generation by exporter #
###############################################
# Import and expand electricity shares to cover all years
elec_shares <- energy_shares[c('iso.country', 'year', 
                               'share.elec.RE', 'share.elec.NU', 'share.elec.HY', 'share.elec.COM')]
isid('elec_shares', c('iso.country', 'year'))

baci_elec <- left_join(baci_elec, elec_shares, by = c('iso.i' = 'iso.country', 'year' = 'year'))
baci_elec <- subset(baci_elec, year != 2015 & year != 2016) # WEB does not have 2015 and 2016 data yet
  check <- inner_join(baci_elec, elec_shares, by = c('iso.i' = 'iso.country', 'year' = 'year'))
  assert('nrow(check) == nrow(baci_elec)')
  rm(check)
  
assign('outfile', data.frame(i = numeric(0), j = numeric(0), iso.i = character(0), iso.j = character(0),
                             year = numeric(0), v = numeric(0), q = numeric(0), q_e = numeric(0), energy = character(0)))
for (e in c('RE', 'NU', 'HY', 'COM')) {
  assign('df', baci_elec)
  names(df)[names(df) == paste0('share.elec.', e)] <- 'share'
  df$q_e <- df$share*df$q_e
  df$energy <- paste0('ELEC_', e)
  df <- df[c('i', 'j', 'iso.i', 'iso.j', 'year', 'v', 'q', 'q_e', 'energy')]
  outfile <- rbind(outfile, df)
}

saveRDS(outfile, file.path(temp, 'elec_trade.rds'))



