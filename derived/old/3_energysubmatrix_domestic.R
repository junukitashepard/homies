########################################
# Compile energy submatrices: domestic #
########################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
#####################################
# Import files
energy_production <- read.csv(file.path(input, "energy_production_shares.csv"), stringsAsFactors = F)
web <- read.csv(file.path(temp, "web_all.csv"), stringsAsFactors = F)
web.hio <- read.csv(file.path(raw, "OECD_energybalance/HIO_WEB_EnergyNames.csv"), stringsAsFactors = F)
bioshares <- read.csv(file.path(input, "bioshares.csv"), stringsAsFactors = F)

web.countries <- unique(web$country)
energy.names <- c('BIO', 'COAL', 'CRU', 'NG', 'PET', 'RE', 'NUC', 'HYD', 'ELEC_RE', 'ELEC_NU', 'ELEC_HY', 'ELEC_COM', 'HEAT_RE', 'HEAT_NU', 'HEAT_HY', 'HEAT_COM', 'LOSSES')

# # Energy to energy industries (in physical units)
# 
# for (c in web.countries) {
#   
#   # Set up empty matrices
#   pe2e <- matrix(NA, nrow = 4, ncol = 7) # Primary energy ->  Primary + Secondary
#   se2e <- matrix(NA, nrow = 3, ncol = 7) # Secondary energy -> Primary + Secondary
#   
#   # Use for 
#   
# }

# Function: Scale electricity submatrix
c <- 'United States'
y <- 2010
mat.base <- matrix(0, nrow = 1, ncol = 17)
mat.post <- matrix(0, nrow = 4, ncol = 17)
colnames(mat.post) <- colnames(mat.base) <- energy.names
rownames(mat.post) <- c('ELEC_RE', 'ELEC_NU', 'ELEC_HY', 'ELEC_COM')
rownames(mat.base) <- 'ELEC_TOT'

df <- subset(web, country == c & energy.type == "Electricity")

# Electricity to biofuels
elec2agr <- df$production[df$flow == "Agriculture/forestry" & df$year == y]
  elec2agr <- elec2agr * bioshares$share_biofuel[bioshares$country == c & bioshares$year == y]
  
elec2bkb <- df$production[(df$flow == "BKB/peat briquette plants") & df$year == y]
  assert('length(elec2bkb) <= 1')
  if (length(elec2bkb) == 0) {elec2bkb <- 0}

elec2bio <- elec2agr + elec2bkb

# Electricity to mining (-> coal, crude, natural gas)
s.coal <- energy_production$shareMine.coal[energy_production$country == c & energy_production$year == y]
s.crude <- energy_production$shareMine.crude[energy_production$country == c & energy_production$year == y]
s.ng <- energy_production$shareMine.ng[energy_production$country == c & energy_production$year == y]

elec2mine <- df$production[(df$flow == "Mining and quarrying") & df$year == y]
  assert('length(elec2mine) <= 1')
  if (length(elec2mine) == 0) {elec2mine <- 0}
elec2coal <- elec2mine * s.coal
elec2crude <- elec2mine * s.crude
elec2ng <- elec2mine * s.ng

# Electricity to petroleum
elec2pet <- df$production[df$flow == "Oil refineries (transf.)" & df$year == y]

  
