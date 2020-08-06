#############################################
# Organize Eora files: Domestic submatrices #
#############################################
rm(list = ls())

wd <- "~/global_energy"
setwd(wd)
source(paste0(wd, "/functions.R"))

input <- "/data/jus3/GlobalIO/raw/EORA"
output <- "/data/jus3/GlobalIO/output/derived"
temp <- "/data/jus3/GlobalIO/temp"
figures <- "/data/jus3/GlobalIO/output/figures"

library('magrittr')
library('dplyr')
#######################
# Import tables
eora.T <- read.delim(file.path(input, paste0(2014, "/Eora26_2014_bp_T.txt")), header = F, stringsAsFactors = F)
eora.T <- as.matrix(eora.T)

eora.T.labs <- read.delim(file.path(input, paste0(2014, "/labels_T.txt")), header = F, stringsAsFactors = F)
names(eora.T.labs) <- c('country', 'country.code', 'industries', 'sector')
eora.T.labs <- eora.T.labs[c('country', 'country.code', 'industries', 'sector')]
eora.T.labs <- dplyr::group_by(eora.T.labs, country, country.code) %>%
               dplyr::group_by(sector = row_number())
assert('max(eora.T.labs$sector) == 26', eora.T.labs)

# Assign row and column names
eora.T.labs$names <- paste0(eora.T.labs$country.code, ".", eora.T.labs$sector)
rownames(eora.T) <- eora.T.labs$names
colnames(eora.T) <- eora.T.labs$names

# Remove ROW
eora.T <- eora.T[rownames(eora.T) != "ROW.1", colnames(eora.T) != "ROW.1"]

mat.names <- eora.T.labs$names[grepl("ROW.1", eora.T.labs$names) == F] # reassign matrix column/row names
country.names <- unique(subset(eora.T.labs, country != "ROW")[c('country.code')])

#####################
# Add energy values #
#####################
# Separate domestic transaction tables by country
for (c in country.names) {
  assign(paste0('domestic.', c), eora.T[grepl(c, rownames(eora.T)), grepl(c, colnames(eora.T))], envir = parent.frame())
}




# Convert to coefficient tables
total.output <- rowSums(eora.T)
diag.TO <- diag(1/total.output)

eora.A <- crossprod(eora.T, diag.TO)
rownames(eora.A) <- colnames(eora.A) <- mat.names

# Separate domestic coefficient tables by country
for (c in country.names) {
  assign(paste0('domestic.', c), eora.A[grepl(c, rownames(eora.A)), grepl(c, colnames(eora.A))], envir = parent.frame())
}
     
table(eora.T.labs$country.code)


eora.Y <- read.delim(file.path(input, paste0(2014, "/labels_FD.txt")), header = F, stringsAsFactors = F)
