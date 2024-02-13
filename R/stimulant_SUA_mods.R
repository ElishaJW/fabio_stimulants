
# TIDYING SUA DATA FOR STIMULANT COMMODITIES
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# SHOULD RENAME: 15_stim_SUA_tidy

# Load necessary libraries
library(tidyverse)

# Set paths
project_path <- getwd()
fabio_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\Git\\fabio"

# Fetching data on just cocoa production in Cote d'Ivoire (test data)
# data_cocoa_cotedivoire_2021_path <- paste0(project_path,
#                                            "/SUA data/FAOSTAT_data_cocoa_cotedivoire_2021_table.csv")
# data_cocoa_cotedivoire_2021 <- read.csv(data_cocoa_cotedivoire_2021_path)

cat("Fetching all SUA crop and livestock data for stimulant commodities.\n")
sua_full_path <- paste0(fabio_path,
                        "\\input\\fao\\SUA_Crops_Livestock_E_All_Data_(Normalized).csv")
sua_full <- read.csv(sua_full_path, encoding = 'ISO-8859-1')


#Tidy SUA data------------------------------------------------------------------

cat("Tidying SUA data for stimulant commodities.\n")
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
                     'Maté leaves')) %>%
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
sua_full$item<-gsub("é", "e", sua_full$item)
sua_full$area_code<-gsub("'", "", sua_full$area_code)
sua_full$item_code<-gsub("'", "", sua_full$item_code)
sua_full$item_code<-gsub("^0+", "", sua_full$item_code)
sua_full$item_code<-gsub("F", "", sua_full$item_code)
sua_full$item_code<-gsub("\\.", "", sua_full$item_code)

# Set SUA data to the SUA_full dataset (can be tested with cocoa cotedivoire data) 
# sua <- sua_full

# List of Items (full)
items_full_path <- paste0(fabio_path, "\\inst\\items_full_new.csv")
items <- read.csv(items_full_path)


#SUA -> CBS format--------------------------------------------------------------

# SUA data format should match the cbs data in the Rscript by the end

cat("Converting stimulant SUA data into CBS format.\n")

# Merge SUA data with the 'items' list to include data on comm_group, feedtype, etc.
# PROBABLY NOT NECESSARY HERE
sua_with_item_info <- merge(sua_full,
                            items[items$item_code %in% sua_full$item_code, ],
                            by = c("item_code", "item"),
                            all.x = TRUE,
                            indicator = TRUE)

# Create a version of the SUA data with the 'element', or 'uses', as column names
sua_pivot <- reshape2::dcast(sua_full,
                             area_code + area + item_code + item + year ~ element,
                             value.var = "value")

# Drop some unnecessary columns (according to CBS)
sua_pivot <- sua_pivot[, !colnames(sua_pivot) %in% c("Fat supply quantity (g/capita/day)",
                                                     "Protein supply quantity (g/capita/day)",
                                                     "Food supply (kcal/capita/day)",
                                                     "Food supply quantity (g/capita/day)",
                                                     "Opening stocks")]

# Rename columns to match CBS data
# Stock variation == stock addition in SUA
colnames(sua_pivot) <- c('area_code', 'area', 'item_code', 'item', 'year',
                         'Food supply (kcal)', 'exports',
                         'Fat supply quantity (t)', 'feed', 'food',
                         'imports', 'losses', 'other', 'processing',
                         'production', 'Protein supply quantity (t)',
                         'residuals','stock_addition', 'tourist')

# Add some columns to match CBS, empty for now
sua_pivot$total_supply <- 0
sua_pivot$balancing <- 0
# sua_pivot$unspecified <- NA
sua_pivot$seed <- 0 #No seed included in SUA for stimulant commodities..

# Add an intuitive 'stock_withdrawal' column
sua_pivot$stock_withdrawal <- -sua_pivot$stock_addition

# Reorder columns to match CBS
cbs_order <- c('area_code', 'area', 'item_code', 'item', 'year', 'total_supply', 'exports',
               'Fat supply quantity (t)', 'feed', 'food', 'Food supply (kcal)', 'imports',
               'losses', 'other', 'processing', 'production', 'Protein supply quantity (t)',
               'residuals', 'seed', 'stock_withdrawal', 'tourist', 'stock_addition',
               'balancing')#, 'unspecified')

stim_sua <- sua_pivot[cbs_order]

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


# # Show discrepancies of stock additions with 'total_supply'
# cat("Found ", stim_sua[stim_sua$stock_addition > stim_sua$total_supply, .N],
#     " occurences of 'stock_addition' exceeding 'total_supply'.\n",
#     "Keeping values as is.\n", sep = "")

# Save --------------------------------------------------------------------

saveRDS(stim_sua, "data/stim_sua.rds")



