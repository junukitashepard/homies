##########################################
# Function to scale electricity and heat #
##########################################
scale_elhe <- function(c, y, secondary = TRUE, name = 'secondary', domestic = TRUE) {

  df <- subset(web, iso.country == c)

  #suppressWarnings(dir.create(file.path(output, y)))

  print(paste0("Running year: ", y))

  # Electricity/heat to biofuels
  elhe2agr <- df$value[df$flow == "Agriculture/forestry" & df$year == y]
  elhe2agr <- elhe2agr * bioshares$share_biofuel[bioshares$iso.country == c & bioshares$year == y]

  elhe2bkb <- df$value[(df$flow == "BKB/peat briquette plants (energy)") & df$year == y]

  if (length(elhe2bkb) == 0) {elhe2bkb <- 0}
  if (length(elhe2agr) == 0) {elhe2agr <- 0}

  elhe2bio <- elhe2agr + elhe2bkb

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
  elhe2pet <- sum(abs(df$value[(df$flow == "Oil refineries (energy)" | df$flow == "Oil refineries (transf.)") & df$year == y]), na.rm = T)
  if (length(elhe2pet) == 0) {elhe2pet <- 0}

  # Electricity/heat to nuclear
  elhe2nuc <- abs(df$value[df$flow == "Nuclear industry  (energy)" & df$year == y])
  if (length(elhe2nuc) == 0) {elhe2nuc <- 0}

  # Electricity/heat to hydro
  elhe2hyd <- abs(df$value[df$flow == "Pumped storage plants (energy)" & df$year == y])
  if (length(elhe2hyd) == 0) {elhe2hyd <- 0}

  # Electricity/heat to electricity (must scale)
  elhe2elec <- unique(abs(df$value[df$flow == "Main activity producer electricity plants (transf.)" & df$year == y]))

  if (secondary == TRUE) {
    elhe2elec <- abs(df$value[df$flow == 'Own use in electricity, CHP and heat plants (energy)' & df$year == y])

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

  } else {
    if (name == 'RE') {elhe2elec_re <- elhe2elec} else {elhe2elec_re <- 0}
    if (name == 'NUC') {elhe2elec_nu <- elhe2elec} else {elhe2elec_nu <- 0}
    if (name == 'HYD') {elhe2elec_hy <- elhe2elec} else {elhe2elec_hy <- 0}
    if (name == 'BIO' | name == 'COAL' | name == 'CRU' | name == 'NG' | name == 'PET') {elhe2elec_com <- elhe2elec} else {elhe2elec_com <- 0}

  }

  # Electricity/heat to losses
  elhe2loss <- abs(df$value[df$flow == "Losses" & df$year == y])
  if (length(elhe2loss) == 0) {elhe2loss <- 0}

  # Electricity/heat to non-energy
  environment(scale_elhe_none) <- environment()
  elhe2none <- scale_elhe_none(c = c, y = y, secondary = secondary, name = name, domestic = domestic)
  none.names <- names(elhe2none)
  for (i in 1:length(elhe2none)) {
    none.names[i] <- paste0(c, '.', i)
  }

  # Scale electricity/heat terms
  for (i in c('bio', 'coal', 'crude', 'ng', 'pet', 're', 'nuc', 'hyd', 'elec_re', 'elec_nu', 'elec_hy', 'elec_com')) {
    assign('sec', get(paste0('elhe2', i)))
    if (length(sec) == 0) {sec <- 0}
    assign(paste0('elhe2', i), sec)
  }

  elhe_list <- c(elhe2bio, elhe2coal, elhe2crude, elhe2ng, elhe2pet, elhe2re, elhe2nuc, elhe2hyd,
                 elhe2elec_re, elhe2elec_nu, elhe2elec_hy, elhe2elec_com, elhe2loss, elhe2none)
  elhe_list[is.na(elhe_list)] <- 0

  if (secondary == TRUE) {
    if (sum(elhe_list) == 0 | length(se.co) == 0) {
      ELHE_RE <- ELHE_NU <- ELHE_HY <- ELHE_COM <- elhe_list
    } else {
      ELHE_RE <- elhe_list * se.re
      ELHE_NU <- elhe_list * se.nu
      ELHE_HY <- elhe_list * se.hy
      ELHE_COM <- elhe_list * se.co
    }

    mat <- rbind(ELHE_RE, ELHE_NU, ELHE_HY, ELHE_COM)
    colnames(mat) <- c(energy.names, none.names)
    rownames(mat) <- c('ELHE_RE', 'ELHE_NU', 'ELHE_HY', 'ELHE_COM')
    saveRDS(mat, file.path(output, paste0(y, '/mat_', c, '.rds')))

  } else if (secondary == FALSE) {
    mat <- t(as.matrix(elhe_list))
    colnames(mat) <- c(energy.names, none.names)
    rownames(mat) <- name
    saveRDS(mat, file.path(temp, paste0('mat_', name, '_', c, '_', y, '.rds')))
  }
}
