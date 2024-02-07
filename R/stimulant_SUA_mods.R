
# SUPPLY-USE TABLE CONSTRUCTION FOR STIMULANT COMMODITIES
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# Still very much not functional. Converted directly from python code via 
#  chatGPT and still requires debugging. No trade data yet. No tobacco products 
#  yet. Modify as needed.

# Load necessary libraries
library(tidyverse)

# Set paths
project_path <- getwd()
fabio_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\Git\\fabio"

# Fetching data on just cocoa production in Cote d'Ivoire (test data)
#data_cocoa_cotedivoire_2021_path <- paste0(project_path, "/SUA data/FAOSTAT_data_cocoa_cotedivoire_2021_table.csv")
#data_cocoa_cotedivoire_2021 <- read.csv(data_cocoa_cotedivoire_2021_path)

cat("Fetching all SUA crop and livestock data for stimulant commodities.\n")
sua_full_path <- paste0(fabio_path, "\\input\\fao\\SUA_Crops_Livestock_E_All_Data_(Normalized).csv")
sua_full <- read.csv(sua_full_path, encoding = 'ISO-8859-1')

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
colnames(sua_full) <- c("area_code_drop",
                        "area_code",
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

# Rename columns, eliminate unnecessary characters

sua_full$area_code_drop <- NULL
sua_full$item_code_drop <- NULL
sua_full$year_code <- NULL
sua_full$unit <- NULL
sua_full$flag <- NULL

sua_full$item<-gsub("é", "e", sua_full$item)
sua_full$area_code<-gsub("'", "", sua_full$area_code)
sua_full$item_code<-gsub("'", "", sua_full$item_code)
sua_full$item_code<-gsub("^0+", "", sua_full$item_code)


# Set SUA data to the SUA_full dataset. 
# Alternatively can be tested with smaller Cote d'Ivoire cocoa data.
sua <- sua_full

# filtered_data <- btd_full %>%
#   filter(from == "Tanzania", grepl('Cotton', item), year == 2021)

# List of Items (full)
items_full_path <- paste0(fabio_path, "\\inst\\items_full_new.csv")
items <- read.csv(items_full_path)

# # Importing supply items
# items_supply_path <- paste0(fabio_path, "\\inst\\items_supply_new.csv")
# supply_items <- read.csv(items_supply_path)
# 
# # Importing use items 
# items_use_path <- file.path(fabio_path, "inst", "items_use_new.csv")
# use_items <- read.csv(items_use_path)

#SUA -> CBS format--------------------------------------------------------------

# SUA data format should match the cbs data in the Rscript by the end

# Merge SUA data with the 'items' list to include data on comm_group, feedtype, etc.
# NOT NECESSARY HERE
sua_with_item_info <- merge(sua,
                            items[items$item_code %in% sua$item_code, ],
                            by = c("item_code", "item"),
                            all.x = TRUE,
                            indicator = TRUE)

# Create a version of the SUA data with the 'element', or 'uses', as column names
sua_pivot <- reshape2::dcast(sua,
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
sua_pivot$total_supply <- ''
sua_pivot$balancing <- ''
sua_pivot$unspecified <- ''
sua_pivot$stock_withdrawal <- ''
sua_pivot$seed <- '' #No seed included in SUA for stimulant commodities..

# Reorder columns to match CBS
cbs_order <- c('area_code', 'area', 'item_code', 'item', 'year', 'total_supply', 'exports',
               'Fat supply quantity (t)', 'feed', 'food', 'Food supply (kcal)', 'imports',
               'losses', 'other', 'processing', 'production', 'Protein supply quantity (t)',
               'residuals', 'seed', 'stock_withdrawal', 'tourist', 'stock_addition',
               'balancing', 'unspecified')

sua_pivot <- sua_pivot[cbs_order]



