##########################################
# Function to scale electricity and heat #
##########################################
pull_eora <- function(eora.sector, web.flow) {
  elhe <- sum(df$value[df$flow %in% web.flow & df$year == y], na.rm = T)
  if (length(elhe) == 0) {elhe <- 0}
  assign(paste0('elhe.', eora.sector), elhe, envir = parent.frame())
}

scale_elhe <- function(c) {
  
  df <- subset(web, iso.country == c)
  
  min.y <- max(min(energy_production$year[energy_production$iso.country == c]), 
               min(web$year[web$iso.country == c]), na.rm = T)
  max.y <- min(max(energy_production$year[energy_production$iso.country == c]), 
               max(web$year[web$iso.country == c]))
  
  for (y in min.y:max.y) {
    
    suppressWarnings(dir.create(file.path(output, y)))
    
    print(paste0("Running year: ", y))
    
    # Electricity/heat to agriculture
    pull_eora(eora.sector = 1, web.flow = "Agriculture/forestry")
    
    # Electricity/heat to fishing
    pull_eora(eora.sector = 2, web.flow = "Fishing")
    
    # Electricity/heat to mining and quarrying
    pull_eora(eora.sector = 3, web.flow = "Mining and Quarrying")
    
    # Electricity/heat to food and beverages
    pull_eora(eora.sector = 4, web.flow = "Food and tobacco")
    
    # Electricity/heat to textiles and wearing apparel
    pull_eora(eora.sector = 5, web.flow = "Textile and leather")
    
    # Electricity/heat to wood and paper
    pull_eora(eora.sector = 6, web.flow = c("Wood and wood products", "Paper, pulp and print"))
    
    # Electricity/heat to petroleum, chemical and non-metallic mineral products
    pull_eora(eora.sector = 7, web.flow = c("Oil refineries", "Chemical and petrochemical", "Non-metallic minerals"))
    
    # Electricity/heat to metal products
    pull_eora(eora.sector = 8, web.flow = c("Iron and steel", "Non-ferrous metals"))
    
    # Electricity/heat to electrical and machinery
    pull_eora(eora.sector = 9, web.flow = "Machinery")
    
    # Electricity/heat to transport equipment
    pull_eora(eora.sector = 10, web.flow = "Transport equipment")
    
    # Electricity/heat to other manufacturing
    pull_eora(eora.sector = 11, web.flow = "Non-specified (industry)")
    
    # Electricity/heat to Recycling
    pull_eora(eora.sector = 12, web.flow = "Transfers")
    
    # Electricity/heat to electricity, gas, and water
    pull_eora(eora.sector = '13a', web.flow = "Electricity output (GWh)")
    elhe.13a <- elhe.13a * 3.6 # GWh to TJ
    
    pull_eora(eora.sector = '13b', web.flow = 'Heat output')
    pull_eora(eora.sector = '13c', web.flow = 'Own use in electricity, CHP an dheat plants (energy)')
    
    elhe.13 <- elhe.13a + elhe.13b + elhe.13c
    
    # Electricity/heat to construction
    pull_eora(eora.sector = 14, web.flow = "Construction")
    
    # Electricity/heat to maintenance and repair
    elhe.15 <- 0
    
    # Electricity/heat to wholesale trade
    elhe.16 <- 0
    
    # Electricity/heat to wholesale trade
    elhe.17 <- 0
    
    
    
    
    
    
    # Electricity/heat to coal mining
    elhe2coal <- abs(df$value[df$flow == "Coal mines (energy)" & df$year == y])
    if (length(elhe2coal) == 0) {elhe2coal <- 0}
    
    # Electricity/heat to crude/NG
    s.crude <- energy_production$shareCrNG.crude[energy_production$iso.country == c & energy_production$year == y]
    s.ng <- energy_production$shareCrNG.ng[energy_production$iso.country == c & energy_production$year == y]
    
    elhe2crng <- abs(df$value[df$flow == "Oil and gas extraction (energy)" & df$year == y])
    
    elhe2crude <- elhe2crng * s.crude
    elhe2ng <- elhe2crng * s.ng
    if (length(elhe2crude) == 0) {elhe2crude <- 0}
    if (length(elhe2ng) == 0) {elhe2ng <- 0}
    
    # Electricity/heat RE is 0 (no Electricity/heat to produce sun, wind)
    elhe2re <- 0 
    
    # Electricity/heat to petroleum
    elhe2pet <- abs(df$value[df$flow == "Oil refineries (energy)" & df$year == y])
    if (length(elhe2pet) == 0) {elhe2pet <- 0}
    
    # Electricity/heat to nuclear
    elhe2nuc <- abs(df$value[df$flow == "Nuclear industry  (energy)" & df$year == y])
    if (length(elhe2nuc) == 0) {elhe2nuc <- 0}
    
    # Electricity/heat to hydro
    elhe2hyd <- abs(df$value[df$flow == "Pumped storage plants (energy)" & df$year == y])
    if (length(elhe2hyd) == 0) {elhe2hyd <- 0}
    
    # Electricity/heat to electricity (must scale)
    elhe2elec <- abs(df$value[df$flow == "Main activity producer electricity plants (transf.)" & df$year == y])
    
    se.re <- energy_production$share.elec.RE[energy_production$iso.country == c & energy_production$year == y]
    se.nu <- energy_production$share.elec.NU[energy_production$iso.country == c & energy_production$year == y]
    se.hy <- energy_production$share.elec.HY[energy_production$iso.country == c & energy_production$year == y]
    se.co <- energy_production$share.elec.COM[energy_production$iso.country == c & energy_production$year == y]
    
    elhe2elec_re <- elhe2elec * se.re
    elhe2elec_nu <- elhe2elec * se.nu
    elhe2elec_hy <- elhe2elec * se.hy
    elhe2elec_com <- elhe2elec * se.co
    if (length(elhe2elec_re) == 0) {elhe2elec_re <- 0}
    if (length(elhe2elec_nu) == 0) {elhe2elec_nu <- 0}
    if (length(elhe2elec_hy) == 0) {elhe2elec_hy <- 0}
    if (length(elhe2elec_com) == 0) {elhe2elec_com <- 0}
    
    # Electricity/heat to heat (must scale)
    elhe2heat <- abs(df$value[df$flow == "Main activity producer CHP plants (transf.)" & df$year == y])
    
    sh.re <- energy_production$share.heat.RE[energy_production$country == c & energy_production$year == y]
    sh.nu <- energy_production$share.heat.NU[energy_production$country == c & energy_production$year == y]
    sh.hy <- energy_production$share.heat.HY[energy_production$country == c & energy_production$year == y]
    sh.co <- energy_production$share.heat.COM[energy_production$country == c & energy_production$year == y]
    
    elhe2heat_re <- elhe2heat * sh.re
    elhe2heat_nu <- elhe2heat * sh.nu
    elhe2heat_hy <- elhe2heat * sh.hy
    elhe2heat_com <- elhe2heat * sh.co
    if (length(elhe2heat_re) == 0) {elhe2heat_re <- 0}
    if (length(elhe2heat_nu) == 0) {elhe2heat_nu <- 0}
    if (length(elhe2heat_hy) == 0) {elhe2heat_hy <- 0}
    if (length(elhe2heat_com) == 0) {elhe2heat_com <- 0}
    
    # Electricity/heat to losses
    elhe2loss <- abs(df$value[df$flow == "Losses" & df$year == y])
    if (length(elhe2loss) == 0) {elhe2loss <- 0}
    
    # Scale electricity/heat terms
    elhe_list <- c(elhe2bio, elhe2coal, elhe2crude, elhe2ng, elhe2pet, elhe2re, elhe2nuc, elhe2hyd,
                   elhe2elec_re, elhe2elec_nu, elhe2elec_hy, elhe2elec_com, 
                   elhe2heat_re, elhe2heat_nu, elhe2heat_hy, elhe2heat_com, elhe2loss)
    
    if (sum(elhe_list, na.rm = T) == 0) {
      print("All NA in matrix")
      next()
    }
    
    ELHE_RE <- elhe_list * se.re
    ELHE_NU <- elhe_list * se.nu
    ELHE_HY <- elhe_list * se.hy
    ELHE_COM <- elhe_list * se.co
    
    mat <- rbind(ELHE_RE, ELHE_NU, ELHE_HY, ELHE_COM)
    colnames(mat) <- energy.names
    rownames(mat) <- c('ELHE_RE', 'ELHE_NU', 'ELHE_HY', 'ELHE_COM')
    
    saveRDS(mat, file.path(temp, paste0('/ELHE_', c, '_', y, '.rds')))
  }
}