
# TIDYING SUA DATA FOR STIMULANT COMMODITIES
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# SHOULD RENAME: 15_stim_SUA_tidy

# Load necessary libraries
library(tidyverse)
library("data.table")
library(dplyr)
source("R/01_tidy_functions.R")

# Set paths
project_path <- getwd()
fabio_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\FABIO Development\\fabio_stimulants"

# Load Data

regions <- fread("inst/regions_full.csv")

cat("Fetching all SUA crop and livestock data for stimulant commodities.\n")
sua_full_path <- paste0(fabio_path,
                        "\\input\\fao\\SUA_Crops_Livestock_E_All_Data_(Normalized).csv")
sua_full <- read.csv(sua_full_path)#, encoding = 'ISO-8859-1')# = 'UTF-8')

#Tidy SUA data------------------------------------------------------------------

cat("Tidying SUA data for stimulant commodities.\n")

# Changing names of Mate items based in item codes
sua_full <- sua_full %>%
  mutate(Item = ifelse(Item.Code..CPC. == "'01630", "Mate leaves", Item))
sua_full <- sua_full %>%
  mutate(Item = ifelse(Item.Code..CPC. == "'23914", "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate", Item))

# Subset for stimulant commodities
sua_full <- sua_full %>%
  filter(Item %in% c('Chocolate products nes',
                     'Cocoa beans',
                     'Cocoa butter, fat and oil',
                     'Cocoa husks and shells',
                     'Cocoa paste not defatted',
                     'Cocoa powder and cake',
                     'Tea leaves',
                     'Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate',
                     'Coffee, green',
                     'Coffee, decaffeinated or roasted',
                     'Coffee extracts',
                     'Coffee substitutes',
                     'Mate leaves')) %>%
  droplevels()

# Change column names
colnames(sua_full) <- c("area_code",
                        "area_code_drop",
                        "area",
                        "item_code_drop",
                        "item_code",
                        "item",
                        "element_code",
                        "element",
                        "year_code",
                        "year",
                        "unit",
                        "value",
                        "flag")

# Drop unnecessary columns
sua_full$area_code_drop <- NULL
sua_full$item_code_drop <- NULL
sua_full$year_code <- NULL
sua_full$unit <- NULL
sua_full$flag <- NULL

# Eliminate unnecessary characters
sua_full$area_code<-gsub("'", "", sua_full$area_code)
sua_full$item_code<-gsub("'", "", sua_full$item_code)
sua_full$item_code<-gsub("^0+", "", sua_full$item_code)
sua_full$item_code<-gsub("F", "", sua_full$item_code)
sua_full$item_code<-gsub("\\.", "", sua_full$item_code)

# Country / Area adjustments
# sua_full <- area_kick(sua_full, code = 351, pattern = "China", groups = TRUE)
# sua_full <- area_merge(sua_full, orig = 62, dest = 238, pattern = "Ethiopia")
# sua_full <- area_merge(sua_full, orig = 206, dest = 276, pattern = "Sudan")
# sua_full <- area_fix(sua_full, regions)

sua_full <- sua_full %>%
  mutate(area = ifelse(area_code == "107", "Côte d'Ivoire", area))
sua_full <- sua_full %>%
  mutate(area = ifelse(area_code == "223", "Turkey", area))


#SUA -> CBS format--------------------------------------------------------------

# SUA data format should match the cbs data in the Rscript by the end

cat("Converting stimulant SUA data into CBS format.\n")

# Create a version of the SUA data with the 'element', or 'uses', as column names
stim_sua <- reshape2::dcast(sua_full,
                             area_code + area + item_code + item + year ~ element,
                             value.var = "value")

# Drop some unnecessary columns (according to CBS)
stim_sua <- stim_sua[, !colnames(stim_sua) %in% c("Fat supply quantity (g/capita/day)",
                                                     "Protein supply quantity (g/capita/day)",
                                                     "Food supply (kcal/capita/day)",
                                                     "Food supply quantity (g/capita/day)",
                                                     "Opening stocks")]

# Rename columns to match CBS data
# Stock variation == stock addition in SUA
colnames(stim_sua) <- c('area_code', 'area', 'item_code', 'item', 'year',
                         'Food supply (kcal)', 'exports',
                         'Fat supply quantity (t)', 'feed', 'food',
                         'imports', 'losses', 'other', 'processing',
                         'production', 'Protein supply quantity (t)',
                         'residuals','stock_addition', 'tourist')

# Add some columns to match CBS, empty for now
stim_sua$total_supply <- 0
stim_sua$balancing <- 0
# stim_sua$unspecified <- NA
stim_sua$seed <- 0 #No seed included in SUA for stimulant commodities..

# Add an intuitive 'stock_withdrawal' column
stim_sua$stock_withdrawal <- -stim_sua$stock_addition

# Reorder columns to match CBS
cbs_order <- c('area_code', 'area', 'item_code', 'item', 'year', 'total_supply', 'exports',
               'Fat supply quantity (t)', 'feed', 'food', 'Food supply (kcal)', 'imports',
               'losses', 'other', 'processing', 'production', 'Protein supply quantity (t)',
               'residuals', 'seed', 'stock_withdrawal', 'tourist', 'stock_addition',
               'balancing')#, 'unspecified')

stim_sua <- stim_sua[cbs_order]

# Tidying data --------------------------------------------------------------

cat("Tidying stimulant data.\n")

# Add values into the total_supply columns
stim_sua$total_supply <- na_sum(stim_sua$production, stim_sua$imports)

# Replace NA values with 0
stim_sua <- dt_replace(stim_sua, is.na, value = 0)

# Make sure values are not negative
stim_sua <- dt_replace(stim_sua, function(x) {`<`(x, 0)}, value = 0,
                  cols = c("imports", "exports", "feed", "food", "losses",
                           "other", "processing", "production", "seed"))

# Rebalance uses, with 'total_supply' and 'stock_additions' treated as given
stim_sua$balancing <- na_sum(stim_sua$total_supply,
                          -stim_sua$stock_addition,
                          -stim_sua$exports, 
                          -stim_sua$food,
                          -stim_sua$feed,
                          -stim_sua$seed,
                          -stim_sua$losses,
                          -stim_sua$processing,
                          -stim_sua$other,
                          -stim_sua$residuals,
                          -stim_sua$tourist)
stim_sua$balancing <- round(stim_sua$balancing)

# Add 'processing' of Tea leaves into food consumption category
stim_sua <- stim_sua %>%
  mutate(food = ifelse(item == 'Tea leaves', processing + food, food),
         processing = ifelse(item == 'Tea leaves', 0, processing))


# Save --------------------------------------------------------------------

saveRDS(stim_sua, "data/stim_sua.rds")


#### TO DO ####

# -Country name concordance table?
# -Mate name concordance
# -Aggregate Tea leaves and Tea/Mate extracts
#   > Probably best to do this after trade linking
# -Include Tobacco commodities
#   > Aggregate all tobacco commodities for domestic supply and use
#   > Includes cigarettes, cigars, unmanufactured tobacco
#   > Do not include stock variation or residuals for tobacco
#   > For trade data, add 'manufactured tobacco' on top of the aggregation
#   > Balance afterwards
# -Food supply from Cocoa powder is counted as food supply of chocolate products
#   > NOTE: Data seems to overestimate chocolate consumption in cocoa
#     producing countries.
# -Data clean up
#   > China tea 2021 is 100x too large




