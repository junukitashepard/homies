##############################################
# Graph of Mining shares (World Mining Data) #
##############################################
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
# Import file
minedf <- read.csv(file.path(input, 'mine_share.csv'), stringsAsFactors = F)

minedf$share.NONENERGY <- 1 - (minedf$share.COAL + minedf$share.CRU + minedf$share.NG + minedf$share.NUC)

# Subset top 20 energy intensive mining countries (2016)
minedf <- subset(minedf, year == 2015)

energy.mine <- dplyr::group_by(minedf, iso.country) %>%
               dplyr::summarise(energy = sum(share.COAL + share.CRU + share.NG + share.NUC, na.rm = T))
energy.mine <- dplyr::arrange(energy.mine, desc(energy))
top10 <- energy.mine$iso.country[1:20]

minedf <- subset(minedf, iso.country %in% top10)

# Reshape long
plotdf <- data.frame(country = character(0), production.share = numeric(0), type = character(0))
for (v in c('COAL', 'CRU', 'NG', 'NUC', 'NONENERGY')) {
  df <- minedf[c('iso.country', paste0('share.', v))]
  names(df) <- c('country', 'production.share')
  df$type <- v
  plotdf <- rbind(plotdf, df)
}

# Graph
plotdf$type[plotdf$type == 'COAL'] <- 'Coal'
plotdf$type[plotdf$type == 'CRU'] <- 'Crude'
plotdf$type[plotdf$type == 'NG'] <- 'Natural Gas'
plotdf$type[plotdf$type == 'NONENERGY'] <- 'Non-energy'
plotdf$type[plotdf$type == 'NUC'] <- 'Uranium'

plot <- ggplot(data = plotdf,
               aes(x = country, y = production.share * 100, fill = type)) +
        geom_bar(stat = 'identity') +
        scale_fill_tableau(palette = 'Tableau 10') +
        labs(x = "Country",
             y = "% of mining production",
             fill = "") +
        coord_flip() +
        theme(text = element_text(size = 20))
plot
ggsave(file.path(figures, 'mining_shares.jpg'), height = 10, width = 10, units = 'in')





