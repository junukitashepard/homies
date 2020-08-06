#####################################
# Scale electricity by world region #
#####################################
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
library('ggthemes')
library('hiofunctions')

#######################
# Import LCOE files
lcoe_usa <- read.csv(file.path(temp, "lcoe_usa.csv"), stringsAsFactors = F)

# Collapse to RE, NUC, HYD, COM for electricity
lcoe_usa$type[lcoe_usa$energy == 'solar' | lcoe_usa$energy == 'wind' | lcoe_usa$energy == 'biomass'] <- "RE"
lcoe_usa$type[lcoe_usa$energy == 'hydro'] <- "HYD"
lcoe_usa$type[lcoe_usa$energy == 'nuclear'] <- "NUC"
lcoe_usa$type[lcoe_usa$energy == 'coal' | lcoe_usa$energy == 'ng'] <- "COM"
assert("!is.na(lcoe_usa$type)")

lcoe_usa <- group_by(lcoe_usa, type, year) %>%
            summarize(mean.lcoe = mean(lcoe, na.rm = T))
lcoe_usa$mean.lcoe[is.nan(lcoe_usa$mean.lcoe)] <- NA

# Fill in with closes value for mean.lcoe
lcoe_usa <- arrange(lcoe_usa, type, year)

for (i in 1:12) {
  lcoe_usa <- group_by(lcoe_usa, type) %>%
    mutate(mean.lcoe.lag = lag(mean.lcoe),
           mean.lcoe.lead = lead(mean.lcoe))

  lcoe_usa$mean.lcoe[is.na(lcoe_usa$mean.lcoe) & !is.na(lcoe_usa$mean.lcoe.lag)] <-
    lcoe_usa$mean.lcoe.lag[is.na(lcoe_usa$mean.lcoe) & !is.na(lcoe_usa$mean.lcoe.lag)]
  lcoe_usa$mean.lcoe[is.na(lcoe_usa$mean.lcoe) & !is.na(lcoe_usa$mean.lcoe.lead)] <-
    lcoe_usa$mean.lcoe.lead[is.na(lcoe_usa$mean.lcoe) & !is.na(lcoe_usa$mean.lcoe.lead)]
  lcoe_usa$mean.lcoe.lag <- lcoe_usa$mean.lcoe.lead <- NULL
}

lcoe_usa <- group_by(lcoe_usa, year) %>%
            mutate(tot.value = sum(mean.lcoe, na.rm = T))

lcoe_usa$share_lcoe <- lcoe_usa$mean.lcoe/lcoe_usa$tot.value
lcoe_usa$tot.value <- NULL

# Plot scale
lcoe_usa$type[lcoe_usa$type == 'COM'] <- 'Fossil fuel'
lcoe_usa$type[lcoe_usa$type == 'HYD'] <- 'Hydro'
lcoe_usa$type[lcoe_usa$type == 'NUC'] <- 'Nuclear'
lcoe_usa$type[lcoe_usa$type == 'RE'] <- 'Renewables'

ggplot(data = lcoe_usa,
       aes(x = year, y = share_lcoe, colour = type)) +
  geom_line(size = 1) +
  labs(y = "LCOE Scale",
       x = "Year",
       colour = "Resource") +
  scale_fill_tableau(palette = 'Tableau 10') +
  theme(text = element_text(size = 18))

# Save scale file
write.csv(lcoe_usa, file.path(temp, "lcoe_scale.csv"))




