# Global variables #
####################
# Eora version and satelite accounts
eora.version <- 'v199.82'
q.energynames <- c('NaturalGas', 'Coal', 'Petroleum', 'NuclearElectricity', 'HydroelectricElectricity', 
                   'GeothermalElectricity', 'WindElectricity', 'SolarTideandWaveElectricity', 'BiomassandWasteElectricity')
# Eora sectors
eora.sectors <- c('Agriculture', 'Fishing', 'Mining and Quarrying', 'Food and Beverages', 'Textiles and Wearing Apparel', 'Wood and Paper',
                  'Petroleum, Chemical, and Non-Metallic Mineral Products', 'Metal Products', 'Electrical and Machinery', 'Transport Equipment',
                  'Other Manufacturing', 'Recycling', 'Electricity, Gas, Water', 'Construction', 'Maintenance and Repair', 'Wholesale Trade',
                  'Retail Trade', 'Hotels and Restaurants', 'Transport', 'Post and Telecommunications', 'Financial Intermediates and Business',
                  'Public Administration', 'Education, Health, and Other Services', 'Private Households', 'Others', 'Re-export and Re-import')

commercial.sectors <- c('Electricity, Gas, Water', 'Construction', 'Maintenance and Repair', 'Wholesale Trade',
                        'Retail Trade', 'Hotels and Restaurants', 'Transport', 'Post and Telecommunications', 'Financial Intermediates and Business',
                        'Public Administration', 'Education, Health, and Other Services', 'Private Households')
industrial.sectors <- c('Agriculture', 'Fishing', 'Mining and Quarrying', 'Food and Beverages', 'Textiles and Wearing Apparel', 'Wood and Paper',
                        'Petroleum, Chemical, and Non-Metallic Mineral Products', 'Metal Products', 'Electrical and Machinery', 'Transport Equipment',
                        'Other Manufacturing', 'Recycling', 'Others')

# Time period of analysis
year.min <- 1995
year.max <- 2015

# Lists
energy.names <- c('BIO', 'COAL', 'CRU', 'NG', 'PET', 'RE', 'NUC', 'HYD', 
                  'ELEC_RE', 'ELEC_NU', 'ELEC_HY', 'ELEC_COM', 'LOSSES')
primary.energy <- c('BIO', 'COAL', 'CRU', 'NG', 'PET', 'RE', 'NUC', 'HYD')

web.list.BIO <- c('Bio jet kerosene', 'Biodiesels', 'Biogases', 'Biogasoline', 'Charcoal', 'Peat')
web.list.COAL <- c('Anthracite', 'Brown coal (if no detail)', 'Coke oven coke', 'Hard coal (if no detail', 'Lignite', 
                   'Other bituminous coal', 'Sub-bituminous coal')
web.list.CRU <- c('Crude oil', 'Oil shale and oil sands')
web.list.NG <- 'Natural gas'
web.list.PET <- c('Liquefied petroleum gases (LPG)', 'Other oil products', 'Gas/diesel oil excl. biofuels', 'Gasoline type jet fuel')
web.list.RE <- c('Wind', 'Solar thermal', 'Solar photovoltaics', 'Geothermal')
web.list.NUC <- 'Nuclear'
web.list.HYD <- 'Hydro'

# Countries in model:
iso.list <- c(
"ALB", "DZA", "AGO", "ARG", "ARM", "AUS", "AUT", "AZE", "BHR", "BGD", "BLR", "BEL", "BEN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "KHM", "CMR",
"CAN", "CHL", "CHN", "COL", "COG", "CRI", "CIV", "HRV", "CUB", "CYP", "PRK", "COD", "DNK", "DOM", "ECU", "EGY", "SLV", "ERI", "EST",
"ETH", "FIN", "MKD", "FRA", "GAB", "GEO", "DEU", "GHA", "GRC", "GTM", "HTI", "HND", "HKG", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL",
"ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", "KOR", "KWT", "KGZ", "LVA", "LBN", "LBY", "LTU", "LUX", "MYS", "MLT", "MUS", "MEX", "MDA", "MNG",
"MNE", "MAR", "MOZ", "MMR", "NAM", "NPL", "NLD", "NZL", "NIC", "NER", "NGA", "NOR", "OMN", "PAK", "PAN", "PRY", "PER", "PHL", "POL", "PRT", "QAT",
"ROU", "RUS", "SAU", "SEN", "SRB", "SGP", "SVK", "SVN", "ZAF", "SSD", "ESP", "LKA", "SDN", "SUR", "SWE", "CHE", "SYR", "TJK", "TZA", "THA", "TGO",
"TTO", "TUN", "TUR", "TKM", "UKR", "ARE", "GBR", "USA", "URY", "UZB", "VEN", "VNM", "YEM", "ZMB", "ZWE", "YUG"
)

# LCOE calculation variables:
discount.rate <- 0.05 # Discount rate
lifetime <- 30 # Lifetime
tcr.tcp.ratio <- 1.15 # Assume TCR = TCP * 1.15
nameplate <- 500 # NP in MW

# Heat values/energy content for fuels (TJ/ton)
# References: World Nuclear Association, FAO
hv.ng <- 0.048
hv.crude <- 0.045
hv.petroleum <- 0.045
hv.lignite <-0.015
hv.subbit <-0.018
hv.coke <- 0.025
hv.uranium <- 500
hv.peat <- 0.02
hv.biodiesel <- 0.037 
  
# Notional efficiencies of non-combusion energy sources (EIA)
not.eff.RE <- mean(0.12, 0.21, 0.26, 0.16)
not.eff.HYD <- 0.9
not.eff.NUC <- 0.4
not.eff.BIO <- c(mean(0.414, 0.525, 0.072, 0.365), mean(0.55, 0.708), mean(0.464, 0.494, 0.501, 0.484, 0.556, 0.509, 0.549),
                 0.35, mean(0.414, 0.525), 0.35)
not.eff.BIO.names <- c('Biodiesel', 'Biogases', 'Biogasoline', 'Charcoal', 'Other liquid biofuels', 'Solid excluding charcoal')

# Chokepoints
chokepoint_list <- c('Panama.Canal', 'Gilbraltar.Strait', 'English.Channel', 'Denmark.Strait', 'Bosporos.Strait',
                     'Suez.Canal', 'Bab.el.Mandeb.Strait', 'Strait.of.Hormuz', 'Malacca.Strait', 'South.China.Sea',
                     'East.China.Sea', 'Makassar.Strait', 'Ombai.Strait')