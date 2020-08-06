####################################################
# Import and compile OECD SUT for IO recalibration #
####################################################
rm(list = ls())

raw <- "/data/jus3/GlobalIO/raw"
output <- "/data/jus3/GlobalIO/output"
temp <- "/data/jus3/GlobalIO/temp"

# Import file
df <- read.csv(file.path(raw, "OECD_SUT/Use_BasicPrices_AUS.csv"), stringsAsFactors = F)

# Separate domestic and import flows
df.dom <- subset(df, FLOW == "FDOM")
df.imp <- subset(df, FLOW == "FP7")

# 

