#####################
# Scale electricity #
#####################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/derived/0_globals.R"))

raw <- "/data/jus3/GlobalIO/raw"
input <- "/data/jus3/GlobalIO/output/derived"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('ggplot2')
library('zoo')
library('hiofunctions')

#######################
# Compiled cost component data
costs <- openxlsx::read.xlsx(file.path(raw, "LCOE_base.xlsx"), sheet = 2)
costs <- subset(costs, !is.na(year))

# Convert O&M costs to $/kWh:
costs[c('o_cost.rankine', 'm_cost.rankine', 'o_cost.nuclear', 'm_cost.nuclear', 'o_cost.hydro', 'm_cost.hydro')] <-
  lapply(costs[c('o_cost.rankine', 'm_cost.rankine', 'o_cost.nuclear', 'm_cost.nuclear', 'o_cost.hydro', 'm_cost.hydro')],
         function(x) x/1000)

# Separate fuel costs #
#######################
fuel_costs <- costs[names(costs) == 'year' | grepl('fuel', names(costs)) | grepl('hr', names(costs))]

fuel_costs$fuel_cost.nuclear <- fuel_costs$fuel_cost.nuclear * (10^-6) * mean(fuel_costs$hr.nuc, na.rm = T)
fuel_costs$fuel_cost.coal <- fuel_costs$fuel_cost.coal * (10^-6) * mean(fuel_costs$hr.coal, na.rm = T)
fuel_costs$fuel_cost.ng <- fuel_costs$fuel_cost.ng * (10^-6) * mean(fuel_costs$hr.natgas, na.rm = T)
fuel_costs$fuel_cost.petroleum <- fuel_costs$fuel_cost.petroleum * (10^-6) * mean(fuel_costs$hr.pet, na.rm = T)

fuel_costs <- fuel_costs[names(fuel_costs) == 'year' | grepl('fuel', names(fuel_costs))]

# Fuel costs for hydro, solar, wind = 0
fuel_costs$fuel_cost.solar = 0
fuel_costs$fuel_cost.wind = 0
fuel_costs$fuel_cost.hydro = 0

# O&M Costs #
#############
# Add O&M Costs (1997-2003) from EIA-412
eia412 <- read.csv(file.path(temp, "om_costs_1997-2003.csv"), stringsAsFactors = F)
eia412 <- eia412[,2:8]
costs <- left_join(costs, eia412, by = c('year'))

costs$o_cost.hydro.x[is.na(costs$o_cost.hydro.x)] <- costs$o_cost.hydro.y[is.na(costs$o_cost.hydro.x)]
costs$m_cost.hydro.x[is.na(costs$m_cost.hydro.x)] <- costs$m_cost.hydro.y[is.na(costs$m_cost.hydro.x)]
costs$o_cost.nuclear.x[is.na(costs$o_cost.nuclear.x)] <- costs$o_cost.nuclear.y[is.na(costs$o_cost.nuclear.x)]
costs$m_cost.nuclear.x[is.na(costs$m_cost.nuclear.x)] <- costs$m_cost.nuclear.y[is.na(costs$m_cost.nuclear.x)]
costs$o_cost.rankine.x[is.na(costs$o_cost.rankine.x)] <- costs$o_cost.rankine.y[is.na(costs$o_cost.rankine.x)]
costs$m_cost.rankine.x[is.na(costs$m_cost.rankine.x)] <- costs$m_cost.rankine.y[is.na(costs$m_cost.rankine.x)]

costs <- costs[, 1:34]

names(costs) <- lapply(names(costs), function(x) stringr::str_replace(x, ".x", ""))

# Interpolate O&M costs #
om_costs <- costs[c('year', names(costs)[grepl("o_cost", names(costs)) | grepl("m_cost", names(costs))])]
om_costs[is.na(om_costs)] <- 0

om_costs$om_nuc <- om_costs$o_cost.nuclear + om_costs$m_cost.nuclear
om_costs$om_ff <- om_costs$o_cost.rankine + om_costs$m_cost.rankine
om_costs$om_hyd <- om_costs$o_cost.hydro + om_costs$m_cost.hydro

om_costs <- om_costs[c('year', 'om_nuc', 'om_ff', 'om_hyd')]
om_costs[om_costs == 0] <- NA

om_costs <- as.data.frame(na.approx(om_costs))

# For rankine: 21% of fuel costs where missing
om_costs <- left_join(om_costs, fuel_costs[c('year', 'fuel_cost.coal', 'fuel_cost.ng')], by = c('year'))
om_costs$om_ff[is.na(om_costs$om_ff)] <-
  0.21*((om_costs$fuel_cost.coal[is.na(om_costs$om_ff)] + om_costs$fuel_cost.ng[is.na(om_costs$om_ff)])/2)

om_costs <- om_costs[c('year', 'om_nuc', 'om_ff', 'om_hyd')]
om_costs.plotdf <- reshape::melt(om_costs, id = c('year'))

# Plot O&M costs (1990-2016)
om_plot <- ggplot(data = om_costs.plotdf,
                  aes(x = year, y = value, color = as.character(variable))) +
           geom_line()

# Assumption: O&M costs for solar and wind = O&M for wind
om_costs$om_sol = om_costs$om_ff
om_costs$om_win = om_costs$om_ff

# Capital costs #
#################
capital <- costs[grepl("capital", names(costs))]
capital <- na.approx(capital)
capital <- as.data.frame(cbind(costs$year, capital))
names(capital) <- c('year', 'solar', 'wind', 'coal', 'ng', 'petroleum', 'biomass', 'nuclear', 'hydro')
capital$petroleum <- NULL
capital <- reshape::melt(capital, id = c('year'))
capital$variable <- as.character(capital$variable)

cc_plot <- ggplot(data = capital,
                  aes(x = year, y = value, color = as.character(variable))) +
           geom_line()

###############################
# Calculate LCOE (assume 100MW)
###############################
lcoe_calc <- function(fuel) {
  if (fuel == 'coal' | fuel == 'ng') {om_name = 'ff'}
  if (fuel == 'hydro') {om_name = 'hyd'}
  if (fuel == 'nuclear') {om_name = 'nuc'}
  if (fuel == 'solar') {om_name = 'sol'}
  if (fuel == 'wind') {om_name = 'win'}

  # Calculate annual generation (kWh)
  capacity.factor <- mean(costs[, c(paste0('cf.', fuel))], na.rm = T) / 100
  annual.gen <- nameplate * 1000 * 8760 * capacity.factor

  # EAC ($/kWh)
  eac.capex <- subset(capital, variable == fuel)[c('year', 'value')]
  eac.capex$value <- eac.capex$value * tcr.tcp.ratio * (nameplate * 1000)

  crf <- discount.rate/(1-(1+discount.rate)^(-1*lifetime)) # Capital recovery factor
  eac.capex$value <- eac.capex$value * crf
  eac.capex$value <- eac.capex$value / annual.gen # $/kWh

  # Add variable costs
  lcoe <- eac.capex
  lcoe <- left_join(lcoe, om_costs[c('year', paste0('om_', om_name))], by = c('year'))
  lcoe <- left_join(lcoe, fuel_costs[c('year', paste0('fuel_cost.', fuel))], by = c('year'))

  names(lcoe) <- c('year', 'cap', 'om', 'fuel')
  lcoe$lcoe <- lcoe$cap + lcoe$om + lcoe$fuel
  lcoe <- lcoe[c('year', 'lcoe')]
  lcoe$energy <- fuel

  assign(paste0('lcoe.', fuel), lcoe, envir = parent.frame())
}

lcoe_calc(fuel = 'coal')
lcoe_calc(fuel = 'ng')
lcoe_calc(fuel = 'hydro')
lcoe_calc(fuel = 'nuclear')
lcoe_calc(fuel = 'solar')
lcoe_calc(fuel = 'wind')

# Combine LCOE dataframes
# lcoe.all <- full_join(lcoe.coal, lcoe.ng, by = c('year'))
# lcoe.all <- full_join(lcoe.all, lcoe.hydro, by = c('year'))
# lcoe.all <- full_join(lcoe.all, lcoe.nuclear, by = c('year'))
# lcoe.all <- full_join(lcoe.all, lcoe.solar, by = c('year'))
# lcoe.all <- full_join(lcoe.all, lcoe.wind, by = c('year'))

lcoe.all <- rbind(lcoe.coal, lcoe.ng, lcoe.hydro, lcoe.nuclear, lcoe.solar, lcoe.wind)

# Plot LCOE in USA over time
plotdf <- lcoe.all
plotdf$energy[plotdf$energy == 'coal'] <- 'Coal'
plotdf$energy[plotdf$energy == 'hydro'] <- 'Hydro'
plotdf$energy[plotdf$energy == 'ng'] <- 'Natural gas'
plotdf$energy[plotdf$energy == 'nuclear'] <- 'Nuclear'
plotdf$energy[plotdf$energy == 'solar'] <- 'Solar'
plotdf$energy[plotdf$energy == 'wind'] <- 'Wind'

lcoe.plot <- ggplot(data = plotdf,
                    aes(x = year, y = lcoe*1000, colour = energy)) +
             geom_line(size = 1) +
             labs(x = 'Year',
                  y = 'Approximate LCOE ($/MWh)',
                  colour = 'Resource') +
             theme(text = element_text(size = 18))
lcoe.plot

# Write file
write.csv(lcoe.all, file.path(temp, "lcoe_usa.csv"))


