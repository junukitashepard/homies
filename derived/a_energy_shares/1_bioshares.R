###########################################################
# Approximate % of crops used for energy, by year-country #
###########################################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output"
temp <- "/data/jus3/GlobalIO/temp"
figures <- '/data/jus3/GlobalIO/output/figures'

library('magrittr')
library('dplyr')
library('ggplot2')
library('ggthemes')
library('hiofunctions')

# Import IEA Renewables + Waste
df1990 <- read.csv(file.path(raw, "OECD_RE/RENWORLDBES_1990-2014.csv"), stringsAsFactors = F)
df2008 <- read.csv(file.path(raw, "OECD_RE/RENWORLDBES_2008-2017.csv"), stringsAsFactors = F)
assert(names(df1990) == names(df2008), df1990)

df1990 <- subset(df1990, Time < 2008)
re.waste <- rbind(df1990, df2008)
df1990 <- df2008 <- NULL

# Import FAO Crops
df1990 <- read.csv(file.path(raw, "FAO/FAOSTAT_data_1990-1999.csv"), stringsAsFactors = F)
df2000 <- read.csv(file.path(raw, "FAO/FAOSTAT_data_2000-2009.csv"), stringsAsFactors = F)
df2010 <- read.csv(file.path(raw, "FAO/FAOSTAT_data_2010-2016.csv"), stringsAsFactors = F)
fao.crops <- rbind(df1990, df2000, df2010)
df1990 <- df2000 <- df2010 <- NULL

fao.crops <- dplyr::group_by(fao.crops, Area, Year) %>%
             dplyr::summarise(total.crop = sum(Value, na.rm = T))
names(fao.crops) <- c('Country', 'Time', 'total.crop')

# Total biofuels
re.waste <- subset(re.waste, Flow == "Domestic Supply")
re.waste <- subset(re.waste, Product == "Biodiesels (kt)" | Product == "Biogases" | Product == "Biogasoline (kt)" |
                             Product == "Charcoal (kt)" | Product == "Other Liquid Biofuels (kt)" |
                             Product == "Solid Biofuel excluding charcoal (TJ - net)")

# Convert to input amount (tons)
eff.biodiesel <- mean(0.414, 0.52, 0.072, 0.365)
eff.biogasoline <- mean(0.464, 0.494, 0.501, 0.484, 0.556, 0.509, 0.549)
eff.charcoal <- mean(0.33, 0.42)
eff.othbiofuels <- mean(eff.biodiesel, eff.biogasoline)
ec.biogas <- mean(10.6, 10.6, 7.1) * 10^9 # GJ/ton to J/ton
ec.solidbiofuel <- (9.14*1000)/mean(0.50, 0.79) # (TJ-net/m3)/(t/m3)

re.waste$input <- NA
re.waste$input[re.waste$Product == "Biodiesels (kt)"] <- (re.waste$Value[re.waste$Product == "Biodiesels (kt)"] / eff.biodiesel) * 1000
re.waste$input[re.waste$Product == "Biogases"] <- (re.waste$Value[re.waste$Product == "Biogases"] / ec.biogas) * 10^12
re.waste$input[re.waste$Product == "Biogasoline (kt)"] <- (re.waste$Value[re.waste$Product == "Biogasoline (kt)"] / eff.biogasoline) * 1000
re.waste$input[re.waste$Product == "Charcoal (kt)"] <- (re.waste$Value[re.waste$Product == "Charcoal (kt)"] / eff.charcoal) * 1000
re.waste$input[re.waste$Product == "Other Liquid Biofuels (kt)"] <- (re.waste$Value[re.waste$Product == "Other Liquid Biofuels (kt)"] / eff.othbiofuels) * 1000
re.waste$input[re.waste$Product == "Solid Biofuel excluding charcoal (TJ - net)"] <- (re.waste$Value[re.waste$Product == "Solid Biofuel excluding charcoal (TJ - net)"] / ec.solidbiofuel)


# Plot stacked bar to illustrate share of biofuel types
plotdf <- re.waste
plot.regions <- c('OECD Americas', 'Non-OECD Americas',
                  'Africa', 'Other Africa',
                  'Asia excluding China', 'China',
                  'Middle East',
                  'OECD Europe',
                  'OECD Asia Oceania')
plotdf$region <- plotdf$Country
plotdf <- subset(plotdf, region %in% plot.regions)

plotdf$region[plotdf$region == "OECD Americas" | plotdf$region == "Non-OECD Americas"] <- "Americas"
plotdf$region[plotdf$region == "Africa" | plotdf$region == "Other Africa"] <- "Africa"
plotdf$region[plotdf$region == "Asia excluding China" | plotdf$region == "China"] <- "Asia"
plotdf$region[plotdf$region == "OECD Europe"] <- "Europe"
plotdf$region[plotdf$region == "OECD Asia Oceania"] <- "Oceania"

plotdf <- dplyr::group_by(plotdf, region, Product) %>%
          dplyr::summarise(mean.input = mean(input, na.rm = T))

ggplot(data = plotdf, aes(y = mean.input, x = region, fill = Product)) +
  geom_bar(stat = "identity")

# Link to FAO crop yield data
#############################
country.fao <- unique(fao.crops$Country)
country.oecd <- unique(re.waste$Country)
country.join <- unique(c(country.fao, country.oecd))

# Fix country names to match
for (c in c('Belgium', 'Bolivia', 'Hong Kong', 'Taiwan', 'China', 'Ethiopia', 'Sudan', 'Venezuela', 'Iran', 'United States')) {
  fao.crops$Country[grepl(c, fao.crops$Country)] <- c
  re.waste$Country[grepl(c, fao.crops$Country)] <- c
}

fao.crops <- dplyr::group_by(fao.crops, Country, Time) %>%
             dplyr::summarise(total.crop = sum(total.crop, na.rm = T))
biofuels <- dplyr::group_by(re.waste, Country, Product, Time) %>%
            dplyr::summarise(input = sum(input, na.rm = T))

fao.bio <- inner_join(biofuels, fao.crops, by = c('Country', 'Time'))

# Share of crop going to biofuels
fao.bio$share_biofuel <- fao.bio$input/fao.bio$total.crop

# Remove Hong Kong and Taiwan, Luxembourg
fao.bio <- subset(fao.bio, Country != "Hong Kong" & Country != "Taiwan" & Country != "Luxembourg")

# Write file
outfile <- fao.bio
names(outfile) <- c('country', 'bioprod', 'year', 'input', 'total.crop', 'share_biofuel')
outfile <- dplyr::group_by(outfile, country, year) %>%
           dplyr::summarise(share_biofuel = sum(share_biofuel, na.rm = T))

outfile$country[outfile$country == "China"] <- "People's Republic of China"
outfile$country[outfile$country == "Côte d'Ivoire"] <- "Cote dIvoire"
outfile$country[outfile$country == "Iran"] <- "Islamic Republic of Iran"

# Add ISO countries
web.countries <- read.csv(file.path(raw, "ConversionTables/web_countries.csv"), stringsAsFactors = F, strip.white = T)
outfile <- inner_join(outfile, web.countries, by = c('country' = 'web.country'))
isid('outfile', c('country', 'year'))

write.csv(outfile, file.path(output, "derived/bioshares.csv"))

# Plot crop-biofuels
####################
plotdf <- dplyr::group_by(fao.bio, Country, Product) %>%
          dplyr::summarise(mean.share = mean(share_biofuel, na.rm = T))

# Top 5 countries with crop-biofuel ratio
rank <- dplyr::group_by(plotdf, Country) %>%
        dplyr::summarise(tot.share = sum(mean.share, na.rm = T))
rank <- dplyr::arrange(rank, desc(tot.share))
country.top5 <- rank$Country[1:5]

plot.countries <- c("United States", "China", "Australia", "India", country.top5)

# Subset plot
plotdf <- subset(plotdf, Country %in% plot.countries)

# Plot
ggplot(data = plotdf, aes(x = Country, y = mean.share*100, fill = Product)) +
       geom_bar(stat = "identity") +
       scale_fill_tableau(palette = 'Tableau 10') +
       labs(x = "Country",
            y = "% of annual crop yield for biofuels",
            fill = "") +
       theme(text = element_text(size = 18),
             legend.position = 'bottom')
ggsave(file.path(figures, 'bioshares_cropyield.jpg'), width = 12, height = 8, units = 'in')


# Plot USA over time
usa <- subset(fao.bio, Country == "United States")

ggplot(data = usa,
       aes(x = Time, y = share_biofuel * 100, colour = Product)) +
  geom_line() +
  labs(x = "Year",
       y = "% of annual crop yield for biofuels",
       colour = "") +
  theme(text = element_text(size = 18),
        legend.position = 'bottom')



