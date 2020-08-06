##################################################
# Convert quantity (tons) to physical units (TJ) #
##################################################
# Import files #
################
baci <- readRDS(file.path(temp, 'baci_energy.rds'))
elec <- readRDS(file.path(temp, 'elec_trade.rds'))
  elec <- elec[c('i', 'j', 'year', 'energy', 'v', 'q', 'q_e', 'iso.i', 'iso.j')]

# WEB for import/export comparison
web <- data.frame()

for (e in primary.energy) {
  df <- read.csv(file.path(input, paste0('PRIM_inputs/', e, '.csv')), stringsAsFactors = F)
  df <- subset(df, flow == 'Imports' | flow == 'Exports')
  df <- df[c('country', 'year', 'flow', 'value', 'iso.country', 'baci.country')]
  if (nrow(df) > 0) {df$energy <- e}
  web <- rbind(as.data.frame(web), as.data.frame(df))
}

energy_shares <- read.csv(file.path(output, 'energy_production_shares.csv'), stringsAsFactors = F)

# Combine electricity and primary trade data
baci <- rbind(baci, elec)
baci <- subset(baci, energy != 'ELEC')

# Compare baci data to WEB data, adjust as necessary (weight IEA higher) #
##########################################################################
# We adjust based on imports
adjust_baci <- function() {
  print('Adjusting imports based on WEB data')
  environment(isid) <- environment()
  baci.df <- group_by(baci, iso.j, year, energy) %>% summarize(v = sum(v), q = sum(q), q_e = sum(q_e))
  
  names(baci.df) <- c('iso.country', 'year', 'energy', 'baci.v', 'baci.q', 'baci.q_e')
  
  web.df <- subset(web, flow == 'Imports')
  
  # Combine to calculate difference
  outdf <- left_join(web.df, baci.df, by = c('iso.country', 'year', 'energy'))
  outdf$baci.v[is.na(outdf$baci.v)] <- outdf$baci.q[is.na(outdf$baci.q)] <- outdf$baci.q_e[is.na(outdf$baci.q_e)] <- 0
  
  outdf$difference <- (outdf$baci.q_e - outdf$value)/outdf$value
  outdf$difference[is.infinite(outdf$difference)] <- NA
  
  outdf <- subset(outdf, difference >= 1.1) # Adjust if BACI is 10% more than IEA
  
  outdf <- outdf[c('iso.country', 'year', 'energy', 'value')]
  
  # Obtain share of trade, multiply by IEA value
  base.baci <- baci
  isid('base.baci', c('iso.i', 'iso.j', 'year', 'energy'))
  base.baci <- group_by(base.baci, iso.j, year, energy) %>% mutate(tot.q_e = sum(q_e))
  base.baci$share_q_e <- base.baci$q_e/base.baci$tot.q_e
  base.baci <- base.baci[c('iso.i', 'iso.j', 'year', 'energy', 'share_q_e')]
  
  outdf <- inner_join(outdf, base.baci, by = c('iso.country' = 'iso.j', 'year', 'energy'))
  outdf$value <- outdf$share_q_e*outdf$value
  outdf <- outdf[c('iso.i', 'iso.country', 'year', 'energy', 'value')]
  names(outdf) <- c('iso.i', 'iso.j', 'year', 'energy', 'adjusted.q_e')
  
  baci <- left_join(baci, outdf, by = c('iso.i', 'iso.j', 'year', 'energy'))
  baci$q_e[!is.na(baci$adjusted.q_e)] <- baci$adjusted.q_e[!is.na(baci$adjusted.q_e)]
  
  assign('baci', baci, envir = parent.frame())
}

adjust_baci()

# Import primary domestic matrix #
##################################
# Assumption: countries use imported energy products in the same way they use domestic energy products
energy_to_none <- function(import.country, y) {
  suppressWarnings(dir.create(file.path(output, paste0('TRADE_ENERGY_submat/', y))))
  suppressWarnings(dir.create(file.path(output, paste0('TRADE_ENERGY_submat/', y, '/', import.country))))
  
  print(paste0('Calibrating energy to non-energy: IMPORT COUNTRY = ', import.country, ', YEAR = ', y))
  assign('df', subset(baci, iso.j == import.country & year == y))

  assign('dom.primary', readRDS(file.path(output, 'PRIM_submat', paste0(y, '/mat_', import.country, '.rds'))))
  assign('dom.elec', readRDS(file.path(output, 'ELEC_submat', paste0(y, '/mat_', import.country, '.rds'))))
    rownames(dom.elec)[grepl('ELHE', rownames(dom.elec))] <- lapply(rownames(dom.elec)[grepl('ELHE', rownames(dom.elec))], 
                                                                    function(x) str_replace(x, 'ELHE', 'ELEC'))
  assign('dom.tot', rbind(dom.primary, dom.elec))
  environment(assert) = environment()
  assert('!is.nan(dom.tot)')

  assign('vec.sum', rowSums(dom.tot)) # Calculate sum of each row (total use for energy)
  dom.tot <- dom.tot/vec.sum
  dom.tot[is.nan(dom.tot)] <- 0

  # Build empty matrix
  assign('empty.mat', matrix(0, nrow = length(energy.names), ncol = (length(energy.names) + 26)))
  row.names(empty.mat) <- energy.names
    cnames <- list()
    for (s in 1:26) {cnames[[s]] <- paste0(import.country, '.', s)}
    cnames <- c(energy.names, cnames)
  colnames(empty.mat) <- cnames
  
  for (c in iso.list[iso.list != import.country]) {

    assign('df.i', subset(df, iso.i == c))
    assign('mat', empty.mat)
    
    if (nrow(df.i) != 0) {  
      for (e in unique(df.i$energy)) {
        assign('evec', df.i$q_e[df.i$energy == e] * dom.tot[rownames(dom.tot) == e])
        environment(assert) <- environment()
        assert('ncol(mat) == length(evec)')
        mat[rownames(mat) == e] <- evec
      }
    }
    
    rownames(mat)[rownames(mat) %in% energy.names] <- 
      lapply(rownames(mat)[rownames(mat) %in% energy.names], function(x) paste0(c, '.', x))
  
    colnames(mat)[colnames(mat) %in% energy.names] <-
      lapply(colnames(mat)[colnames(mat) %in% energy.names], function(x) paste0(import.country, '.', x))
    
    saveRDS(mat, file.path(output, paste0('TRADE_ENERGY_submat/', y, '/', import.country, '/mat_', c, 'to', import.country, '.rds')))
    
    }
}

# Run program #
###############
for (country in iso.list) {
  for (y in year.min:year.max) {
      if (file.exists(file.path(output, 'PRIM_submat', paste0(y, '/mat_', country, '.rds'))) &
          file.exists(file.path(output, 'ELEC_submat', paste0(y, '/mat_', country, '.rds')))) {
    energy_to_none(import.country = country, y = y)
  }
  }
}


