###########################################
# Reshape OECD SUT long, keep only energy #
###########################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')

sut.list <- c('Australia', 'Austria', 'Belgium', 'Canada', 'Chile', 'Denmark', 'Spain', 'Estonia',
              'Finland', 'France', 'United Kingdom', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Korea',
              'Lithuania', 'Luxembourg', 'Latvia', 'Mexico', 'Netherlands', 'Norway', 'New Zealand',
              'Poland', 'Portugal', 'Slovak', 'Slovenia', 'Sweden', 'Turkey', 'United States')
sut.list.abv <- c('AUS', 'AUT', 'BEL', 'CAN', 'CHL', 'DNK', 'ESP', 'EST', 'FIN', 'FRA', 'GBR',
                  'GRC', 'HUN', 'IRL', 'ITA', 'KOR', 'LTU', 'LUX', 'LVA', 'MEX', 'NLD', 'NOR', 'NZL',
                  'POL', 'PRT', 'SVK', 'SVN', 'SWE', 'TUR', 'USA')
###########################################
# Import crosswalks
cc_transact <- read.csv(file.path(raw, "OECD_SUT/CodeCrosswalk_Transactions.csv"), stringsAsFactors = F)
cc_flow <- read.csv(file.path(raw, "OECD_SUT/CodeCrosswalk_Flows.csv"), stringsAsFactors = F)
cc_product <- read.csv(file.path(raw, "OECD_SUT/CodeCrosswalk_Products.csv"), stringsAsFactors = F)
oecd_eora <- read.csv(file.path(raw, "OECD_SUT/OECD_Eora_Crosswalk.csv"), stringsAsFactors = F)

# Keep Eora sectors
inEora.Tran <- oecd_eora$OECD_TransactionCode
inEora.Prod <- oecd_eora$OECD_ProductCode

# Extract codes of interest (transactions = specific energy resources)
coal.Tran <- cc_transact$TransactionCode[grepl("coal and lignite", cc_transact$TransactionDescription)]
crude.ng.Tran <- cc_transact$TransactionCode[grepl("crude petroleum and natural gas", cc_transact$TransactionDescription)]
petrol.Tran <- cc_transact$TransactionCode[grepl("petroleum products", cc_transact$TransactionDescription)]
elec.Tran <- cc_transact$TransactionCode[grepl("Electricity", cc_transact$TransactionDescription)]

coal.Prod <- cc_product$ProductCode[grepl("Coal and lignite", cc_product$ProductDescription)]
crude.ng.Prod <- cc_product$ProductCode[grepl("Crude petroleum and natural gas", cc_product$ProductDescription)]
petrol.Prod <- cc_product$ProductCode[grepl("petroleum products", cc_product$ProductDescription)]
elec.Prod <- cc_product$ProductCode[grepl("Electricity", cc_product$ProductDescription)]

# Extract denominator code
coal.crude.ng.Tran <- cc_transact$TransactionCode[grepl("Mining and quarrying", cc_transact$TransactionDescription)]
coal.crude.ng.Prod <- cc_product$ProductCode[grepl("Mining and quarrying", cc_product$ProductDescription)]

refining.Tran <- cc_transact$TransactionCode[grepl("petroleum products", cc_transact$TransactionDescription) |
                                             grepl("chemical products", cc_transact$TransactionDescription) |
                                             grepl("basic pharm", cc_transact$TransactionDescription) |
                                             grepl("plastics product", cc_transact$TransactionDescription) |
                                             grepl("non-metallic", cc_transact$TransactionDescription)]
refining.Prod <- cc_product$ProductCode[grepl("petroleum products", cc_product$ProductDescription) |
                                        grepl("chemical products", cc_product$ProductDescription) |
                                        grepl("Basic pharm", cc_product$ProductDescription) |
                                        grepl("plastic product", cc_product$ProductDescription) |
                                        grepl("non-metallic", cc_product$ProductDescription)]

elec.water.Tran <- cc_transact$TransactionCode[grepl("Electricity", cc_transact$TransactionDescription) |
                                         grepl("Water supply", cc_transact$TransactionDescription)]
elec.water.Prod <- cc_product$ProductCode[grepl("Electricity", cc_product$ProductDescription) |
                                    grepl("Water supply", cc_product$ProductDescription)]

###########################################
# Function: ShareEnergy
ShareEnergy <- function(energy.type, denom.type) {
  
  # Set up energy codes
  print(paste0("Set up energy codes for ", energy.type))
  assign('energy.ProdCode', get(paste0(energy.type, '.Prod')))
  assign('energy.TranCode', get(paste0(energy.type, '.Tran')))
  
  assign('denom.ProdCode', get(paste0(denom.type, ".Prod")))
  assign('denom.TranCode', get(paste0(denom.type, ".Tran")))
  
  # Keep relevant variables
  df <- df[c('TRANSACT', 'PRODUCT', 'FLOW', 'obsTime', 'obsValue')]
  names(df) <- c('transaction', 'product', 'flow', 'year', 'value')
  
  # Total use
  df <- subset(df, flow == "FTOT")
  
  # Aggregate to Eora sector level
  E2C.df <- subset(df, transaction %in% inEora.Tran)
  C2E.df <- subset(df, product %in% inEora.Prod)
  
  # Subset to energy (numerator and denominator) for both energy -> industry and industry -> energy
  E2C.energy <- subset(E2C.df, product == energy.ProdCode)
  C2E.energy <- subset(C2E.df, transaction == energy.TranCode)
  
  E2C.energy.denom <- subset(E2C.df, product %in% denom.ProdCode)
  C2E.energy.denom <- subset(C2E.df, transaction %in% denom.TranCode)
  
  # For the energy num. and denom.: Aggregate by Eora code and year (E2C = energy to industry, C2E = commodity to industry)
  E2C.energy <- left_join(E2C.energy, oecd_eora[c('Eora_Sector', 'OECD_TransactionCode')], 
                          by = c('transaction' = 'OECD_TransactionCode')) 
  C2E.energy <- left_join(C2E.energy, oecd_eora[c('Eora_Sector', 'OECD_ProductCode')],
                          by = c('product' = 'OECD_ProductCode'))
  
  E2C.energy.denom <- left_join(E2C.energy.denom, oecd_eora[c('Eora_Sector', 'OECD_TransactionCode')],
                                by = c('transaction' = 'OECD_TransactionCode'))
  C2E.energy.denom <- left_join(C2E.energy.denom, oecd_eora[c('Eora_Sector', 'OECD_ProductCode')],
                                by = c('product' = 'OECD_ProductCode'))
  
  for (e in c('E2C.energy', 'C2E.energy', 'E2C.energy.denom', 'C2E.energy.denom')) {
    assign('df', get(e))
    df <- group_by(df, year, Eora_Sector) %>%
      summarise(value = sum(value, na.rm = T))
    if (grepl('E2C', e)) {names(df)[names(df) == 'value'] <- 'energy_to_sector'}
    if (grepl('C2E', e)) {names(df)[names(df) == 'value'] <- 'sector_to_energy'}
    assign(e, df)
  }
  
  # Combine numerator and denominator files
  E2C <- full_join(E2C.energy, E2C.energy.denom, by = c('year', 'Eora_Sector'))
  E2C$share <- E2C$energy_to_sector.x/E2C$energy_to_sector.y
    E2C$share[E2C$energy_to_sector.x == 0 & E2C$energy_to_sector.y == 0] <- NA
  E2C <- E2C[c('Eora_Sector', 'year', 'share')]
  names(E2C) <- c('Eora_Sector', 'year', paste0('perc_', energy.type, '_to_sector'))
  
  C2E <- full_join(C2E.energy, C2E.energy.denom, by = c('year', 'Eora_Sector'))
  C2E$share <- C2E$sector_to_energy.x/C2E$sector_to_energy.y
    C2E$share[C2E$sector_to_energy.x == 0 & C2E$sector_to_energy.y == 0] <- NA
  C2E <- C2E[c('Eora_Sector', 'year', 'share')]
  names(C2E) <- c('Eora_Sector', 'year', paste0('perc_sector_to_', energy.type))
  
  # Assign to country files
  assign('E2C', E2C, envir = parent.frame())
  assign('C2E', C2E, envir = parent.frame())
}
###########################################
# Run by country
# Germany (DEU) only has FTOT
for (k in sut.list.abv) {

  # Import file
  country <- k
  assign('df', read.csv(file.path(raw, paste0("OECD_SUT/Use_BasicPrices_", country, ".csv")), stringsAsFactors = F))
  print(paste0("###################################"))
  print(paste0("Running for: ", k))

  # Coal, Crude/NG
  for (a in c('coal', 'crude.ng')) {
      
    ShareEnergy(energy.type = a, denom.type = 'coal.crude.ng')
    E2C$country <- C2E$country <- k
    assign(paste0('E2C.', a), E2C, envir = parent.frame())
    assign(paste0('C2E.', a), C2E, envir = parent.frame())
    E2C <- C2E <- NULL
  }
    
  # Petroleum
  ShareEnergy(energy.type = 'petrol', denom.type = 'refining')
  E2C$country <- C2E$country <- k
  assign(paste0('E2C.petrol'), E2C, envir = parent.frame())
  assign(paste0('C2E.petrol'), C2E, envir = parent.frame())
  E2C <- C2E <- NULL
  
  # Electricity
  ShareEnergy(energy.type = 'elec', denom.type = 'elec.water')
  E2C$country <- C2E$country <- k
  assign(paste0('E2C.elec'), E2C, envir = parent.frame())
  assign(paste0('C2E.elec'), C2E, envir = parent.frame())
  E2C <- C2E <- NULL
  
  # Combine by country
  E2C <- dplyr::full_join(E2C.coal, E2C.crude.ng, by = c('country', 'year', 'Eora_Sector'))
    E2C <- dplyr::full_join(E2C, E2C.petrol, by = c('country', 'year', 'Eora_Sector'))
    E2C <- dplyr::full_join(E2C, E2C.elec, by = c('country', 'year', 'Eora_Sector'))
    E2C <- E2C[c('country', 'year', 'Eora_Sector', 
                 'perc_coal_to_sector', 'perc_crude.ng_to_sector', 'perc_petrol_to_sector', 'perc_elec_to_sector')]
    
  C2E <- dplyr::full_join(C2E.coal, C2E.crude.ng, by = c('country', 'year', 'Eora_Sector'))
    C2E <- dplyr::full_join(C2E, C2E.petrol, by = c('country', 'year', 'Eora_Sector'))
    C2E <- dplyr::full_join(C2E, C2E.elec, by = c('country', 'year', 'Eora_Sector'))
    C2E <- C2E[c('country', 'year', 'Eora_Sector',
                 'perc_sector_to_coal', 'perc_sector_to_crude.ng', 'perc_sector_to_petrol', 'perc_sector_to_elec')]
    
  assign(paste0('E2C.', country), E2C, envir = parent.frame())  
  assign(paste0('C2E.', country), C2E, envir = parent.frame())
}

# Combine all E2C, C2E files
E2C <- E2C.AUS[0,]
C2E <- C2E.AUS[0,]

for (c in sut.list.abv) {
  assign("E2C", rbind(E2C, get(paste0("E2C.", c))), envir = parent.frame())
  assign("C2E", rbind(C2E, get(paste0("C2E.", c))), envir = parent.frame())
}

# Write files
write.csv(E2C, file.path(output, "E2C.csv"))
write.csv(C2E, file.path(output, "C2E.csv"))

