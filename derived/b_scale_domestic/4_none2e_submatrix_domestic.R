#############################################
# Organize Eora files: Domestic submatrices #
#############################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version)
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived/NONE_submat"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('hiofunctions')

#######################
# Import energy production shares
bioshares <- read.csv(file.path(input, "bioshares.csv"), stringsAsFactors = F)
energy_production <- read.csv(file.path(input, "energy_production_shares.csv"), stringsAsFactors = F)
mine_share <- read.csv(file.path(input, "mine_share.csv"), stringsAsFactors = F)
lcoe_scale <- read.csv(file.path(temp, 'lcoe_scale.csv'), stringsAsFactors = F)

isid('energy_production', c('country', 'year'))
isid('bioshares', c('country', 'year'))
isid('mine_share', c('iso.country', 'year'))

# Scale domestic transaction tables by country
##############################################
dom.hybrid_scale <- function(c, y) {

  assign(paste0('dom.mat'), eora.T[grepl(c, rownames(eora.T)), grepl(c, colnames(eora.T))], envir = parent.frame())
  if(dim(dom.mat)[1] == 0 & dim(dom.mat)[2] == 0) {
    print(paste0("Country ", c, " Year ", y, ": Doesn't exist in Eora, skipping"))
    return(NULL)}
  if(nrow(subset(energy_production, iso.country == c & year == y)) == 0) {
    print(paste0('Country ', c, ' Year ', y, ': No rows in energy production, skipping'))
    return(NULL)}

  print(paste0("Scaling: COUNTRY ", c, ", YEAR ", y))
  suppressWarnings(dir.create(file.path(output, y))) # Write directory

  # Scale biofuels #
  ##################
  BIO.vec <- dom.mat[, 1]
  bioshare <- bioshares$share_biofuel[bioshares$iso.country == c & bioshares$year == y]
    if (length(bioshare) == 0) {bioshare <- 0}

  BIO.vec <- BIO.vec*bioshare

  # Scale mining (coal, crude, ng) #
  ##################################
  mine.vec <- dom.mat[, 3]

  # If mine data exist, use World Mine Data
  coal.share <- mine_share$share.COAL[mine_share$iso.country == c & mine_share$year == y]
  crude.share <- mine_share$share.CRU[mine_share$iso.country == c & mine_share$year == y]
  ng.share <- mine_share$share.NG[mine_share$iso.country == c & mine_share$year == y]
  nuc.share <- mine_share$share.NUC[mine_share$iso.country == c & mine_share$year == y]

  if (length(coal.share) == 0 & length(crude.share) == 0 & length(ng.share) == 0 & length(nuc.share) == 0) {
    coal.share <- energy_production$shareMine.coal[energy_production$iso.country == c & energy_production$year == y]
    crude.share <- energy_production$shareMine.crude[energy_production$iso.country == c & energy_production$year == y]
    ng.share <- energy_production$shareMine.ng[energy_production$iso.country == c & energy_production$year == y]
    nuc.share <- 0
  }

  COAL.vec <- mine.vec * coal.share
  CRU.vec <- mine.vec * crude.share
  NG.vec <- mine.vec * ng.share
  NUC.vec <- mine.vec * nuc.share

  # Scale petroleum #
  ###################
  PET.vec <- dom.mat[, 7]

  pet.share <- energy_production$share.pet[energy_production$iso.country == c & energy_production$year == y]
  if (length(pet.share) == 0 | is.na(pet.share)) {pet.share <- 0.95} # Global average

  PET.vec <- PET.vec * pet.share

  # Scale electricity #
  #####################
  elec.vec <- dom.mat[, 13] * 0.90 # Assume 90% of electricity, gas, water goes to electricity supply

  share.COM <- energy_production$share.elec.COM[energy_production$iso.country == c & energy_production$year == y]
  share.RE <- energy_production$share.elec.RE[energy_production$iso.country == c & energy_production$year == y]
  share.NUC <- energy_production$share.elec.NU[energy_production$iso.country == c & energy_production$year == y]
  share.HYD <- energy_production$share.elec.HY[energy_production$iso.country == c & energy_production$year == y]

  scale <- lcoe_scale$share_lcoe[lcoe_scale$type == 'COM' & lcoe_scale$year == y]
  weight.COM <- lcoe_scale$share_lcoe[lcoe_scale$type == 'COM' & lcoe_scale$year == y]/scale
  weight.RE <- lcoe_scale$share_lcoe[lcoe_scale$type == 'RE' & lcoe_scale$year == y]/scale
  weight.HYD <- lcoe_scale$share_lcoe[lcoe_scale$type == 'HYD' & lcoe_scale$year == y]/scale
  weight.NUC <- lcoe_scale$share_lcoe[lcoe_scale$type == 'NUC' & lcoe_scale$year == y]/scale

  scale.COM <- share.COM*weight.COM
  scale.RE <- share.RE*weight.RE
  scale.HYD <- share.HYD*weight.HYD
  scale.NUC <- share.NUC*weight.NUC

  ELEC_COM.vec <- elec.vec*scale.COM
  ELEC_RE.vec <- elec.vec*scale.RE
  ELEC_HY.vec <- elec.vec*scale.HYD
  ELEC_NU.vec <- elec.vec*scale.NUC

  # RE and HYD (extraction) does not require monetary input
  RE.vec <- as.vector(matrix(0, nrow = 26, ncol = 1))
  HYD.vec <- as.vector(matrix(0, nrow = 26, ncol = 1))
  names(RE.vec) <- names(HYD.vec) <- names(BIO.vec)

  # Nothing intentionally goes to LOSSES
  LOSSES.vec <- as.vector(matrix(0, nrow = 26, ncol = 1))

  # Assign non-energy to energy domestic matrix
  assign('none2e.dom', get(paste0(energy.names[1], '.vec')))
  for (e in energy.names[2:length(energy.names)]) {assign('none2e.dom', cbind(none2e.dom, get(paste0(e, '.vec'))))}
  for (i in 1:length(energy.names)) {colnames(none2e.dom)[i] <- energy.names[i]}

  # Revise non-e matrix (subtract flow to e)
  vec.1 <- dom.mat[, 1] - BIO.vec
  vec.3 <- dom.mat[, 3] - (COAL.vec + CRU.vec + NG.vec + NUC.vec)
  vec.7 <- dom.mat[, 7] - PET.vec
  vec.13 <- dom.mat[, 13] - (ELEC_RE.vec + ELEC_NU.vec + ELEC_HY.vec + ELEC_COM.vec)

  dom.hybrid <- cbind(none2e.dom,
                      vec.1, dom.mat[, 2], vec.3, dom.mat[, 4:6], vec.7, dom.mat[, 8:12], vec.13, dom.mat[, 14:26])

  for (i in 1:26) {
    colnames(dom.hybrid)[ncol(none2e.dom) + i] <- colnames(dom.mat)[i]
  }

  saveRDS(dom.hybrid, file.path(output, paste0(y, '/mat_', c, '.rds')))
}

# Run program
for (year in year.min:year.max) {
  import_eora(year)

  for (country in country.list) {
    dom.hybrid_scale(c = country, y = year)
}}



