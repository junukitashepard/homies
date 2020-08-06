############################################
# Get electricity import price, by country #
############################################
# Import files
baci <- readRDS(file.path(temp, 'baci_energy.rds'))

sql_import(statement = paste0('SELECT * FROM WEB.WEB_EXTENDED ', 
                              'WHERE WEB.WEB_EXTENDED.Product = "Electricity" AND WEB.WEB_EXTENDED.Measure = "tj" ',
                              'AND (WEB.WEB_EXTENDED.Flow = "Imports" OR WEB.WEB_EXTENDED.Flow = "Exports")'),
           outname = 'electricity')

web_countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F)

# Link iso.country names to BACI and electricity
baci <- inner_join(baci, web_countries[c('baci.country', 'iso.country')], by = c('i' = 'baci.country'))
baci <- inner_join(baci, web_countries[c('baci.country', 'iso.country')], by = c('j' = 'baci.country'))
names(baci) <- c('i', 'j', 'year', 'energy', 'v', 'q', 'q_e', 'iso.i', 'iso.j')
baci <- unique(baci)
baci$v <- as.numeric(baci$v)
saveRDS(baci, file.path(temp, 'baci_energy.rds'))

electricity <- inner_join(electricity, web_countries[c('web.country', 'iso.country')], by = c('Country' = 'web.country'))
electricity <- electricity[c('Country', 'Time', 'Value', 'Flow', 'iso.country')]
names(electricity) <- c('web.country', 'year', 'value', 'flow', 'iso.country')
electricity$value <- as.numeric(electricity$value)
electricity <- unique(electricity)

isid('baci', c('iso.i', 'iso.j', 'year', 'energy'))
isid('electricity', c('iso.country', 'year', 'flow'))

# Separate electricity exports and imports
elec_imports <- subset(electricity, flow == "Imports")
elec_exports <- subset(electricity, flow == "Exports")
elec_imports$flow <- elec_exports$flow <- NULL
names(elec_imports) <- c('web.country', 'year', 'importsj', 'iso.country')
names(elec_exports) <- c('web.country', 'year', 'exportsi', 'iso.country')

# Subset BACI to just electricity (remove duplicates)
baci <- unique(subset(baci, grepl('ELEC', energy))[c('iso.i', 'iso.j', 'i', 'j', 'year', 'v', 'q', 'q_e')])
isid('baci', c('iso.i', 'iso.j', 'year'))

# Link BACI and electricity
baci_elec <- left_join(baci, elec_imports, by = c('iso.j'='iso.country', 'year'))
baci_elec <- left_join(baci_elec, elec_exports, by = c('iso.i' = 'iso.country', 'year'))

# Collapse to get import price and export price
import_price <- dplyr::group_by(baci_elec, j, iso.j, year) %>%
                dplyr::summarize(tot.v = sum(v, na.rm = T),
                                 tot.q = sum(importsj, na.rm = T))
export_price <- dplyr::group_by(baci_elec, i, iso.i, year) %>%
                dplyr::summarise(tot.v = sum(v, na.rm = T),
                          tot.q = sum(exportsi, na.rm = T))

import_price$import_rate <- (import_price$tot.v)/import_price$tot.q # $1000/TJ
import_price$import_rate_kwh <- import_price$import_rate*(1.0E-6)*3.6

export_price$export_rate <- (export_price$tot.v)/abs(export_price$tot.q) # $1000/TJ
export_price$export_rate_kwh <- export_price$export_rate * (1.0E-6)*3.6

import_price <- subset(import_price, !is.infinite(import_rate_kwh) & !is.infinite(import_rate))
export_price <- subset(export_price, !is.infinite(export_rate_kwh) & !is.infinite(export_rate))

# Combine rates
import_price <- import_price[c('iso.j', 'year', 'import_rate', 'import_rate_kwh')]
export_price <- export_price[c('iso.i', 'year', 'export_rate', 'export_rate_kwh')]
all_prices <- full_join(import_price, export_price, by = c('iso.j' = 'iso.i', 'year'))
names(all_prices) <- c('iso', 'year', 'import_rate', 'import_rate_kwh', 'export_rate', 'export_rate_kwh')

# Plot histograms of electricity rates
# Remove (for plot) rates > 0.1 (outliers, 16)
plotdf <- subset(all_prices, (import_rate_kwh < 0.1 | is.na(import_rate_kwh)) & (export_rate_kwh < 0.1 | is.na(export_rate_kwh)))
plotdf1 <- plotdf[c('iso', 'year', 'import_rate_kwh')]
  plotdf1$type <- 'Imports'
plotdf2 <- plotdf[c('iso', 'year', 'export_rate_kwh')]
  plotdf2$type <- 'Exports'
names(plotdf1) <- names(plotdf2) <- c('iso', 'year', 'rate', 'type')
plotdf <- rbind(plotdf1, plotdf2)

plot <- ggplot(data = plotdf,
               aes(x = rate, fill = type)) +
        geom_density(alpha = 0.4) +
        labs(y = 'Density',
             x = 'Electricity price ($/kWh)',
             fill = '') +
        theme(text = element_text(size = 16))

plot

# Write file
all_prices$mean_rate <- (all_prices$import_rate + all_prices$export_rate) / 2
  all_prices$mean_rate[is.na(all_prices$import_rate)] <- all_prices$export_rate[is.na(all_prices$import_rate)]
  all_prices$mean_rate[is.na(all_prices$export_rate)] <- all_prices$import_rate[is.na(all_prices$export_rate)]
  
all_prices$mean_rate[is.na(all_prices$import_rate) & !is.na(all_prices$export_rate)] <- all_prices$export_rate[is.na(all_prices$import_rate) & !is.na(all_prices$export_rate)]
all_prices$mean_rate[is.na(all_prices$export_rate) & !is.na(all_prices$import_rate)] <- all_prices$import_rate[is.na(all_prices$export_rate) & !is.na(all_prices$import_rate)]

saveRDS(all_prices, file.path(temp, 'elec_tradeprices.rds'))

