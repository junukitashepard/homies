##########################################################
# Cluster countries to X centroids based on energy data #
##########################################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
library('ggplot2')
library('gridExtra')
library('cluster')
##########################################################
# Import UNSD Energy statistics
# We do not include (but can add): aviationgasoline, motorgasoline,
energy.list <- c('anthracite', 'biodiesel', 'biogases', 'biogasoline', 'browncoal', 'charcoal',
                 'crude', 'crudeoffshore', 'electricity_combustible', 'electricity_geothermal', 'electricity_hydro',
                 'electricity_nuclear', 'electricity_tidal', 'electricity_wind', 'lignite','naturalgas', 'peat')

for (e in energy.list) {
  print("*************************")
  print(paste0("Reading file: ", e))
  print("*************************")
  
  df <- read.csv(file.path(raw, paste0("UNSD_EnergyStats/production/", e, ".csv")), stringsAsFactors = F)
  df <- subset(df, grepl("production", Commodity...Transaction) | grepl("Production", Commodity...Transaction) |
                   grepl("net installed capacity", Commodity...Transaction))
  print("Commodity transaction: ")
  print(table(df$Commodity...Transaction))
  
  df <- df[c('Country.or.Area', 'Year', 'Quantity')]
  names(df) <- c('country', 'year', e)
  isid('df', c('country', 'year'))
  assign(e, df, envir = parent.frame())
}

# Combine files to master file
basedf <- dplyr::full_join(anthracite, biodiesel, by = c('country', 'year'))
templist <- energy.list[!(energy.list %in% c('anthracite', 'biodiesel'))]

for (e in templist) {
  assign('df', get(e))
  basedf <- dplyr::full_join(basedf, df, by = c('country', 'year'))
}
rm(templist)

# Import population data
population <- read.csv(file.path(raw, "WorldBank/Population_1960-2017.csv"), stringsAsFactors = F)

crosswalk <- read.csv(file.path(raw, "WorldBank/WB_UNSD_crosswalk.csv"), stringsAsFactors = F)[, 1:2]
  names(crosswalk) <- c('country.unsd', 'country')
  population <- left_join(population, crosswalk, by = c('Country.Name' = 'country'))
  population$Country.Name[!is.na(population$country.unsd)] <- population$country.unsd[!is.na(population$country.unsd)]
  
pop.long <- population[c('Country.Name', 'Country.Code', 'X1960')]
pop.long$year <- 1960
names(pop.long) <- c('country', 'country.code', 'population', 'year')

for (y in 1960:2017) {
  assign('df', population[c('Country.Name', 'Country.Code', paste0('X', y))])
  df$year <- y
  names(df) <- c('country', 'country.code', 'population', 'year')
  assign('pop.long', rbind(pop.long, df))
}
population <- pop.long
rm(pop.long)

basedf <- dplyr::inner_join(basedf, population, by = c('country', 'year'))
basedf$country.code <- NULL

# Replace all NA with 0
assert("!is.na(year) & !is.na(country)", basedf)
basedf[is.na(basedf)] <- 0

# Add latitude and longitude of country capitals
latlong <- read.csv(file.path(raw, "Google/Capitals.csv"), stringsAsFactors = F)
  crosswalk <- read.csv(file.path(raw, "WorldBank/WB_UNSD_crosswalk.csv"), stringsAsFactors = F)[, c(1, 3)]
  names(crosswalk) <- c('country_UNSD', 'name')
  latlong <- left_join(latlong, crosswalk, by = c('name'))
  latlong$name[!is.na(latlong$country_UNSD)] <- latlong$country_UNSD[!is.na(latlong$country_UNSD)]
  latlong <- latlong[c('name', 'latitude', 'longitude')]
  
basedf <- inner_join(basedf, latlong, by = c('country' = 'name'))
isid('basedf', c('country', 'year'))

# Standardize variables (z-score)
std.df <- basedf
std.df[, c(energy.list, 'population')] <- 
  as.data.frame(lapply(std.df[, c(energy.list, 'population')], function(x) (x - mean(x))/sd(x)))

# Cluster by energy variables
clust.df <- std.df[FALSE,]
plist <- list()

for (y in min(std.df$year):max(std.df$year)) {
  
  print(paste0("Clustering for year ", y))
  
  assign('df', std.df)
  df <- subset(df, year == y)
  
  set.seed(182730)
  
  cdf <- df[!(names(df) %in% c('country', 'year'))]
  
  wss.list <- c(NA)
  for (i in 2:31) {
    clusters <- kmeans(cdf, i)
    wss.list[i] <- clusters$tot.withinss
  }
  
  cpdf <- as.data.frame(wss.list)
  cpdf$cluster <- as.numeric(rownames(cpdf))
  
  # Plot WSS
  plotname <- paste0('plot.', y)
  plist[[plotname]] <- ggplot(data = cpdf,
                        aes(x = cluster,
                            y = wss.list)) +
                 geom_line() +
                 geom_point() + 
                 labs(title = y,
                      x = "Clusters",
                      y = "WSS")
  
  #plist[[i]] <- clusterplot
  
  # Assign number of clusters based on "kink"
  X <- cpdf$cluster
  Y <- cpdf$wss.list
  
  del1 <- diff(Y)/diff(X)
  del2 <- diff(del1)/diff(X[-1])
  
  del2 <- abs(del2[1:15])
  n.cluster <- which(del2 == min(del2, na.rm = T))
  
  # Run k-means on selected number of clusters
  clusters <- kmeans(cdf, n.cluster)
  clusters <- as.data.frame(kmeans(cdf, n.cluster)$cluster)
    names(clusters) <- 'cluster'
  cdf <- cbind(cdf, clusters)
  cdf <- cbind(df[c('country', 'year')], cdf)
  
  assign(paste0('cdf.', y), cdf, envir = parent.frame())
  #assign(paste0('cplot.', y), clusterplot, envir = parent.frame())
  
  i = i + 1
}

# Combine annual cluster data
clust.df <- rbind(cdf.1990, cdf.1991)
for (y in 1992:2017) {
  print(paste0("Row-binding: ", y))
  assign('clust.df', rbind(clust.df,
                           get(paste0('cdf.', y))), envir = parent.frame())
}

write.csv(clust.df, file.path(temp, "cluster_for_tableau.csv"))

# Arrange plot
allplot <- do.call(grid.arrange, c(plist, ncol = 5))
ggsave(file.path(figures, "NClusters_WSS.jpg"), allplot, width = 11, height = 11, units = 'in')

# Indicator for whether a country has a SUT
sut.list <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Denmark", "Estonia", "Finland",
              "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Korea", "Latvia",
              "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway", 
              "Poland", "Portugal", "Sweden", "Spain", "Turkey", "Great Britain", "United States", 
              "Brazil", "Bulgaria", "Colombia", "Cyprus", "Malta", "Romania", 
              "Kuwait", "India", "China", "South Africa", "Mali", "Uganda", "Malaysia", "Congo", "Pakistan")

clust.df <- clust.df[c('country', 'year', 'cluster')]

clust.df$SUT <- 0
for (s in sut.list) {
  clust.df$SUT[grepl(s, clust.df$country)] <- 1
}

# Make sure that all cluters have representative country
check <- dplyr::group_by(clust.df, cluster, year) %>%
         dplyr::mutate(max.SUT = max(SUT))
check <- subset(check, max.SUT == 0)





