

# Load the libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(colorspace)
library(dichromat)
library(scales) #color stuff
library(ggpattern)
library(svglite)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

# Load Miscellaneous
items <- fread("inst/items_full.csv")
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" | items$comm_group == "Tobacco, rubber" & items$comm_code != 'c060']$comm_code
regions <- fread("inst/regions_fabio.csv", )

# Load Characterization Factors
# landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF_country.csv")
landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF_domain_country.csv")
water_cf <- read.xlsx("X:/Eli/DATA/cf/water consumption/water.xlsx", sheet="Country_CF")
N_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/N/Country_CF_N.xlsx")
P_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/P/Country_CF_P.xlsx")

# Identify all stimulant commodities
items_coff <- c("Coffee, green",
                "Coffee, decaffeinated or roasted",
                "Coffee extracts",
                "Coffee substitutes")
items_tea <- c("Tea leaves",
               "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
               "Mate leaves")
items_coco <- c("Cocoa beans",
                "Cocoa butter, fat and oil",
                "Cocoa husks and shells",
                "Cocoa paste not defatted",
                "Cocoa powder and cake",
                "Chocolate products nes")
items_tobac <- c("Unmanufactured tobacco",
                 "Cigars and cheroots",
                 'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
                 "Cigarettes")


# Load stressor footprints for given year
year <- 2020
str_fp_landuse <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['landuse']]
str_fp_water <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['blue']]
str_fp_N <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['n_application']]
str_fp_P <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['p_application']]


# World map with country info
world_blank <- ne_countries(scale = "medium", returnclass = "sf")


# Combine FABIO country code data with income classifications:
# country_codes <- fread("X:/Eli/PROJECTS/fabio_stimulants/results_data/country_codes.csv")
# 
# country_codes <- country_codes %>%
#   left_join(world_blank %>% select(income_grp, iso_a3_eh), by = c("iso3c" = "iso_a3_eh"))  %>%
#   dplyr::select(-c(geometry)) %>%
#   filter(!is.na(income_grp) | area_code == 999)
# 
# row_row <- country_codes %>%
#   filter(area_code == 999) %>%
#   mutate(continent = NA)
# 
# country_codes <- country_codes %>%
#   arrange(area, income_grp) %>%  # Ensures preferred row comes first
#   distinct(area, .keep_all = TRUE) %>% # Keeps first occurrence per country
#   mutate(income_grp = str_remove(income_grp, "^\\d+\\. ")) %>%
#   mutate(income_grp = str_extract(income_grp, ".*income"))
# 
# country_codes <- bind_rows(country_codes, row_row)
# 
# write.csv(country_codes, "X:/Eli/PROJECTS/fabio_stimulants/results_data/country_codes.csv", row.names = FALSE)


############### LAND USE BD FOOTPRINT BY COUNTRY ###############################

# PROCESSING CFs FOR LATER USE--------------------------------------------------
# Filter rows where the 'HABITAT' column contains the string 'cropland'
landuse_cf <- landuse_cf[grepl("Cropland", landuse_cf$habitat), ]
# Filter rows where the 'HABITAT' column contains the string 'intense'
landuse_cf <- landuse_cf[grepl("Intense", landuse_cf$habitat), ]
# Rename column 'iso3cd' to 'iso3c'
colnames(landuse_cf)[colnames(landuse_cf) == "iso3cd"] <- "iso3c"
# Drop unnecessary columns
landuse_cf <- landuse_cf[, !c("CF_occ_avg_reg",
                                "CF_occ_avg_reg_rsd",
                                "CF_occ_avg_glo_rsd",
                                "CF_occ_mar_reg",
                                "CF_occ_mar_reg_rsd",
                                "CF_occ_mar_glo",
                                "CF_occ_mar_glo_rsd",
                                "CF_tra_avg_reg",
                                "CF_tra_avg_reg_rsd",
                                "CF_tra_avg_glo",
                                "CF_tra_avg_glo_rsd",
                                "CF_tra_mar_reg",
                                "CF_tra_mar_reg_rsd",
                                "CF_tra_mar_glo",
                                "CF_tra_mar_glo_rsd",
                                "quality_reg",
                                "quality_glo" )]


# PROCESSING FOOTRPINT MATRIX---------------------------------------------------
# Create unique list of commodity-country combos
item_country_combinations <- expand.grid(items$comm_code, regions$area_code)
# Combine the country and commodity codes to create row names for matrix
rownames(str_fp_landuse) <- paste(item_country_combinations$Var1,
                                           item_country_combinations$Var2,
                                           sep = "_")
# Now filter for only stim commodities and subset the matrix to keep only the relevant rows
# str_fp_landuse <- str_fp_landuse[grep(paste(items_stim, collapse = "|"),
#                                             rownames(str_fp_landuse)), , drop = FALSE]
# Convert dgeMatrix to a data frame
str_fp_landuse <- as.data.frame(as.matrix(str_fp_landuse))


# MULTIPLY FOOTPRINT MATRIX WITH CF---------------------------------------------
# Extract the country code from row names
str_fp_landuse$area_code <- as.numeric(sub(".*_", "", rownames(str_fp_landuse)))
# Extract commodity name as well
str_fp_landuse$comm_code <- sub("_.*", "", rownames(str_fp_landuse))
# Add column for corresponding iso3 code
str_fp_landuse <- str_fp_landuse %>%
  left_join(regions[, c('area_code', 'iso3c')], by = "area_code")
# Join the land use dataframe with the characterization factors dataframe by country code
str_fp_landuse <- str_fp_landuse %>%
  left_join(landuse_cf, by = "iso3c")
# Multiply the footprint values by the characterization factors
# Multiply all columns except 'country_code', 'comm_code' and rownames contained in the CF
cols_to_multiply <- colnames(str_fp_landuse)[!colnames(str_fp_landuse) %in% c("area_code", "comm_code", colnames(landuse_cf))]
str_fp_landuse[cols_to_multiply] <- str_fp_landuse[cols_to_multiply] * str_fp_landuse$CF_occ_avg_glo
# Rename rows to correspond to comm, country, species group
rownames(str_fp_landuse) <- paste(str_fp_landuse$comm_code,
                                           str_fp_landuse$area_code,
                                           str_fp_landuse$species_group,
                                           sep = "_")
# Drop unnecessary columns, create new bd dataframe
bd_fp_landuse <- str_fp_landuse[cols_to_multiply]


# GROUPING THE LAND USE FOOTPRINT BY SPECIES GROUP - AVERAGE!-------------------
# Create a new row corresponding to the commodity_country combinations
comm_country <- sub("_[^_]+$", "", rownames(bd_fp_landuse))
bd_fp_landuse$comm_country <- comm_country
# Group by the product-country combination and calculate the mean across rows
bd_fp_landuse_grouped_by_sp <- bd_fp_landuse %>%
  group_by(comm_country) %>%
  summarise(across(everything(), mean, .groups = 'drop',  na.rm = TRUE))
# Create a factor to maintain original order and use it to sort the results
bd_fp_landuse_grouped_by_sp$comm_country <- factor(bd_fp_landuse_grouped_by_sp$comm_country, levels = unique(comm_country))
# Arrange by the new factor levels to maintain the original order
bd_fp_landuse_grouped_by_sp <- bd_fp_landuse_grouped_by_sp %>% arrange(comm_country)
# Remove the comm_country column from the original and new matrices
bd_fp_landuse <- bd_fp_landuse %>%
  dplyr::select(-comm_country)
bd_fp_landuse_grouped_by_sp <- bd_fp_landuse_grouped_by_sp %>%
  dplyr::select(-comm_country)
# Add product-country names back as rownames
# item_country_combinations <- expand.grid(items$comm_code, regions$area_code)
# rownames(bd_fp_landuse_grouped_by_sp) <- paste(item_country_combinations$Var1,
#                                                             item_country_combinations$Var2,
#                                                              sep = "_")


############### WATER USE BD FOOTPRINT BY COUNTRY ###############################

# PROCESSING CF-----------------------------------------------------------------
# Drop unnecessary columns
water_cf <- water_cf[, c("ISO3CD", "CF_GLOB_A_m", "Contains.outliers")]
# Rename column 'iso3cd' to 'iso3c'
colnames(water_cf)[colnames(water_cf) == "ISO3CD"] <- "iso3c"
# Rename column 'Contains outliers' to 'Outliers'
colnames(water_cf)[colnames(water_cf) == "Contains.outliers"] <- "Outliers"

# Preference for countries for countries with no outliers
water_cf <- water_cf %>%
  group_by(iso3c) %>%
  filter(Outliers == FALSE | !any(Outliers == FALSE)) %>%
  slice(1)  # Ensure only one row is kept if both TRUE and FALSE exist

# Remove Outliers column
water_cf <- water_cf[, c("iso3c", "CF_GLOB_A_m")]
# Rename column 'iso3cd' to 'iso3c'
colnames(water_cf)[colnames(water_cf) == "ISO3CD"] <- "iso3c"
# Convert CF values to numerics
water_cf["CF_GLOB_A_m"] <- lapply(water_cf["CF_GLOB_A_m"], as.numeric)


# PROCESSING FOOTRPINT MATRIX---------------------------------------------------
# Create unique list of commodity-country combos
item_country_combinations <- expand.grid(items$comm_code, regions$area_code)
# Combine the country and commodity codes to create row names for matrix
rownames(str_fp_water) <- paste(item_country_combinations$Var1,
                                           item_country_combinations$Var2,
                                           sep = "_")
# Now filter for only stim commodities and subset the matrix to keep only the relevant rows
# str_fp_water <- str_fp_water[grep(paste(items_stim, collapse = "|"),
#                                                         rownames(str_fp_water)), , drop = FALSE]
# Convert dgeMatrix to a data frame
str_fp_water <- as.data.frame(as.matrix(str_fp_water))


# MULTIPLY FOOTPRINT MATRIX WITH CF---------------------------------------------
# Extract the country code from row names
str_fp_water$area_code <- as.numeric(sub(".*_", "", rownames(str_fp_water)))
# Extract commodity name as well
str_fp_water$comm_code <- sub("_.*", "", rownames(str_fp_water))
# Add column for corresponding iso3 code
str_fp_water <- str_fp_water %>%
  left_join(regions[, c('area_code', 'iso3c')], by = "area_code")
# Join the land use dataframe with the characterization factors dataframe by country code
str_fp_water <- str_fp_water %>%
  left_join(water_cf, by = "iso3c")
# Multiply the footprint values by the characterization factors
# Multiply all columns except 'country_code', 'comm_code' and rownames contained in the CF
cols_to_multiply <- colnames(str_fp_water)[!colnames(str_fp_water) %in% c("area_code", "comm_code", colnames(water_cf))]
str_fp_water[cols_to_multiply] <- str_fp_water[cols_to_multiply] * str_fp_water$CF_GLOB_A_m
# Rename rows to correspond to comm, country, species group
rownames(str_fp_water) <- paste(str_fp_water$comm_code,
                                           str_fp_water$area_code,
                                           sep = "_")
# Drop unnecessary columns, create new bd dataframe
bd_fp_water <- str_fp_water[cols_to_multiply]



############### N, P USE BD FOOTPRINT BY COUNTRY ###############################
for (nutrient in c("N", "P")){
  # PROCESSING CF-----------------------------------------------------------------
  # Select CF
  nutrient_cf <- get(paste0(nutrient, "_cf"))
  # Reformat columns
  nutrient_cf <- nutrient_cf[, c(1:2, 19:26)]
  colnames(nutrient_cf)[3:ncol(nutrient_cf)] <- nutrient_cf[1, 3:ncol(nutrient_cf)]
  nutrient_cf <- nutrient_cf[-1, ]
  # Rename and drop unnecessary columns
  colnames(nutrient_cf)[colnames(nutrient_cf) == "ISO3"] <- "iso3c"
  colnames(nutrient_cf)[colnames(nutrient_cf) == "Average CF for diffuse source (PDF·year/kgN)"] <- "CF_avg_diff"
  nutrient_cf <- nutrient_cf[, c("iso3c", "CF_avg_diff")]

  # Convert CFs to numeric
  nutrient_cf["CF_avg_diff"] <- lapply(nutrient_cf["CF_avg_diff"], as.numeric)
  
  # PROCESSING FOOTRPINT MATRIx---------------------------------------------------
  # Select footprint matrix
  str_fp_nutrient <- get(paste0("str_fp_", nutrient))
  # Create unique list of commodity-country combos
  item_country_combinations <- expand.grid(items$comm_code, regions$area_code)
  # Combine the country and commodity codes to create row names for matrix
  rownames(str_fp_nutrient) <- paste(item_country_combinations$Var1,
                                           item_country_combinations$Var2,
                                           sep = "_")
  # Now filter for only stim commodities and subset the matrix to keep only the relevant rows
  # str_fp_nutrient <- str_fp_nutrient[grep(paste(items_stim, collapse = "|"),
  #                                                     rownames(str_fp_nutrient)), , drop = FALSE]
  # Convert dgeMatrix to a data frame
  str_fp_nutrient <- as.data.frame(as.matrix(str_fp_nutrient))
  
  # MULTIPLY FOOTPRINT MATRIX WITH CF---------------------------------------------
  # Extract the country code from row names
  str_fp_nutrient$area_code <- as.numeric(sub(".*_", "", rownames(str_fp_nutrient)))
  # Extract commodity name as well
  str_fp_nutrient$comm_code <- sub("_.*", "", rownames(str_fp_nutrient))
  # Add column for corresponding iso3 code
  str_fp_nutrient <- str_fp_nutrient %>%
    left_join(regions[, c('area_code', 'iso3c')], by = "area_code")
  # Join the land use dataframe with the characterization factors dataframe by country code
  str_fp_nutrient <- str_fp_nutrient %>%
    left_join(nutrient_cf, by = "iso3c")
  # Multiply the footprint values by the characterization factors
  # Multiply all columns except 'country_code', 'comm_code' and rownames contained in the CF
  cols_to_multiply <- colnames(str_fp_nutrient)[!colnames(str_fp_nutrient) %in% c("area_code", "comm_code", colnames(nutrient_cf))]
  str_fp_nutrient[cols_to_multiply] <- str_fp_nutrient[cols_to_multiply] * str_fp_nutrient$CF_avg_diff
  # Rename rows to correspond to comm, country, species group
  rownames(str_fp_nutrient) <- paste(str_fp_nutrient$comm_code,
                                           str_fp_nutrient$area_code,
                                           sep = "_")
  # Drop unnecessary columns, create new bd dataframe
  bd_fp_nutrient <- str_fp_nutrient[cols_to_multiply]
  # Assign as variable to env
  assign(paste0("bd_fp_", nutrient), bd_fp_nutrient, envir = .GlobalEnv)
}


################## PUTTING BD FOOTPRINT TOGETHER ###############################

# SUM THE MATRICES TOGETHER-----------------------------------------------------
rownames(bd_fp_landuse_grouped_by_sp) <- rownames(bd_fp_water)
# Simple sum of dataframes, first changing NA to 0 where necessary
bd_fp_landuse_grouped_by_sp[is.na(bd_fp_landuse_grouped_by_sp)] <- 0
bd_fp_water[is.na(bd_fp_water)] <- 0
bd_fp_water[is.na(bd_fp_water)] <- 0
bd_fp_P[is.na(bd_fp_P)] <- 0
bd_fp_total <-  bd_fp_landuse_grouped_by_sp +
                            bd_fp_water +
                            bd_fp_water +
                            bd_fp_P

# Choose from individual impact categories or the combined form
bd_fp_for_chosen_impact_category <- bd_fp_total


# CHOOSE ONE OF THE FOLLOWING: -------------------------------------------------
# SUM ACROSS CONSUMPTION CATEGORIES FOR EACH COUNTRY----------------------------
country_code <- regions$area_code
# Now sum across consumption categories for each country
bd_fp_total_by_country_filtered <- bd_fp_for_chosen_impact_category
# Initialize an empty matrix to store the results
bd_fp_total_by_country <- matrix(0, nrow = nrow(bd_fp_total_by_country_filtered),
                                                      ncol = length(unique(sub("_.*", "", colnames(bd_fp_total_by_country_filtered)))))
# Set the row and column names for the result matrix
rownames(bd_fp_total_by_country) <- rownames(bd_fp_water)
colnames(bd_fp_total_by_country) <- unique(sub("_.*", "", colnames(bd_fp_total_by_country_filtered)))
# Loop over each unique country code and sum across the consumption categories
for (country in colnames(bd_fp_total_by_country)) {
  # Identify the columns corresponding to the current country (consumption categories)
  country_columns <- grep(paste0("^", country, "_"), colnames(bd_fp_total_by_country_filtered))
  # Sum across the columns for this country
  bd_fp_total_by_country[, country] <- rowSums(bd_fp_total_by_country_filtered[, country_columns], na.rm = TRUE)
} 
# Convert to df
bd_fp_total_by_country <- as.data.frame(bd_fp_total_by_country)


# OR SUM ACROSS CONSUMPTION CATEGORIES FOR EACH COUNTRY EXCEPT 'OTHER'----------
# Filter out columns that contain 'other'
bd_fp_total_by_country_ex_other_filtered <- bd_fp_for_chosen_impact_category[, !grepl("other", colnames(bd_fp_for_chosen_impact_category))]
# Initialize an empty matrix to store the results
bd_fp_total_by_country_ex_other <- matrix(0, nrow = nrow(bd_fp_total_by_country_ex_other_filtered),
                                                      ncol = length(unique(sub("_.*", "", colnames(bd_fp_total_by_country_ex_other_filtered)))))
# Set the row and column names for the result matrix
rownames(bd_fp_total_by_country_ex_other) <- rownames(bd_fp_water)
colnames(bd_fp_total_by_country_ex_other) <- unique(sub("_.*", "", colnames(bd_fp_total_by_country_ex_other_filtered)))
# Loop over each unique country code and sum across the consumption categories
for (country in colnames(bd_fp_total_by_country_ex_other)) {
  # Identify the columns corresponding to the current country (consumption categories)
  country_columns <- grep(paste0("^", country, "_"), colnames(bd_fp_total_by_country_ex_other_filtered))
  # Sum across the columns for this country
  bd_fp_total_by_country_ex_other[, country] <- rowSums(bd_fp_total_by_country_ex_other_filtered[, country_columns], na.rm = TRUE)
} 
# Convert to df
bd_fp_total_by_country_ex_other <- as.data.frame(bd_fp_total_by_country_ex_other)


# OR JUST TAKE THE FOOD CONUMPTION CATEGORY-------------------------------------
country_code <- regions$area_code
bd_fp_food_by_country <- bd_fp_for_chosen_impact_category
# Extract country codes from column names
colnames(bd_fp_food_by_country) <- sub(".*_", "", colnames(bd_fp_food_by_country))
# Select only FOOD columns
bd_fp_food_by_country <- bd_fp_food_by_country[grepl("food", colnames(bd_fp_food_by_country))]
# Rename the columns with just the country code
colnames(bd_fp_food_by_country) <- unique(country_code)
# Restore rownames of the dataframe
rownames(bd_fp_food_by_country) <- rownames(bd_fp_water)


# CHOOSE CONSUMPTION CATEGORIES HERE:-------------------------------------------
bd_fp_for_chosen_cons_category <- bd_fp_total_by_country


# SORT ACCORDING TO COMMODITIES-------------------------------------------------
# Subset the bd_footprint df based on whether the commodity is in the list
#bd_fp_total_by_country_coff <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items$comm_code[items$item %in% items_coff], ]
#bd_fp_total_by_country_coco <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items$comm_code[items$item %in% items_coco], ]
#bd_fp_total_by_country_tea <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items$comm_code[items$item %in% items_tea], ]
#bd_fp_total_by_country_tobac <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items$comm_code[items$item %in% items_tobac], ]
bd_fp_total_by_country_all_stim <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items_stim, ]


# HERE WE CHOOSE COMMODITY TO PLOT:---------------------------------------------
commodities_to_assess <- bd_fp_total_by_country_all_stim


# Sum the footprints across all rows for each consuming country-----------------
top_10_consumers <- as.data.frame(colSums(commodities_to_assess, na.rm = TRUE))
# Rename the column for clarity
colnames(top_10_consumers) <- "total_footprint"
# Identify the top 10 consuming countries
top_10_consumers <- top_10_consumers %>%
  arrange(desc(total_footprint)) %>%
  head(25)
# Calculate the share of production countries and commodities for the top 10 consumers
top_10_consumers_shares <- commodities_to_assess[, colnames(commodities_to_assess) %in% rownames(top_10_consumers)]
# Convert to long format
top_10_consumers_shares <- top_10_consumers_shares %>%
  rownames_to_column("prod_commodity") %>%
  pivot_longer(cols = -prod_commodity, names_to = "consuming_country", values_to = "footprint")  
# Separate producer and commodity
top_10_consumers_shares <- top_10_consumers_shares %>%
  separate(prod_commodity, into = c("commodity", "producing_country"), sep = "_", remove = TRUE)
# Convert country codes to integers for later join
top_10_consumers_shares$consuming_country <- as.integer(top_10_consumers_shares$consuming_country)
top_10_consumers_shares$producing_country <- as.integer(top_10_consumers_shares$producing_country)
# Replace consuming country codes with names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = area) %>%
  dplyr::select(-c(consuming_country, iso3c, continent))
# Replace producing country codes with names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(regions, by = c("producing_country" = "area_code")) %>%
  rename(producer = area) %>%
  dplyr::select(-c(producing_country, iso3c, continent))
# Replace commodity codes with commodity names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(items, by = c("commodity" = "comm_code")) %>%
  rename(commodity_name = item) %>%
  dplyr::select(-c(commodity, item_code, unit, group, moisture, feedtype, comm_group))
# Arrange in descending order for visualization
top_10_consumers_shares <- top_10_consumers_shares %>%
  arrange(desc(footprint))
# Change names to iso3
top_10_consumers_shares <-  top_10_consumers_shares %>%
  left_join(regions, by = c("consumer" = "area")) %>%
  dplyr::select(-c(consumer, area_code, continent)) %>%
  rename(consumer = iso3c) %>%
  left_join(regions, by = c("producer" = "area")) %>%
  dplyr::select(-c(producer, area_code, continent)) %>%
  rename(producer = iso3c) #%>%
# Summarise the footprint contributions
top_10_consumers_shares <- top_10_consumers_shares %>%
  group_by(consumer, commodity_name, producer) %>%
  summarise(total_footprint = sum(footprint, na.rm = TRUE), .groups = 'drop')
# Identify top contributors and group the rest as 'Other'
top_10_consumers_shares <- top_10_consumers_shares %>%
  group_by(consumer) %>%
  mutate(rank = rank(-total_footprint, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    # Only keep top 3 contributors per consumer, and group others as "Other"
    prod_commodity_name = ifelse(rank <= 2, paste(producer, commodity_name, sep = "_"), "other")
  ) %>%
  group_by(consumer, prod_commodity_name) %>%
  summarise(total_footprint = sum(total_footprint, na.rm = TRUE), .groups = 'drop')
top_10_consumers_shares$prod_commodity_name <- sub("_", " ", top_10_consumers_shares$prod_commodity_name)

# Create a color palette for each commodity group
commodity_palettes <- list(
  "Coffee" = colorRampPalette(brewer.pal(9, "Reds")[2:9]),
  "Cocoa" = colorRampPalette(brewer.pal(9, "Greens")[2:9]),
  "Tea" = colorRampPalette(brewer.pal(9, "Blues")[2:9]),
  "Tobacco" = colorRampPalette(brewer.pal(7, "Purples")[3:7]),
  "other" = function(n) rep("#d3d3d3", n)  # Grey for "Other"
)

# Simplify commodity names
simplified_names <- c(
  "Unmanufactured tobacco" = "Tobacco",
  "Cocoa beans" = "Cocoa",
  "Coffee, green" = "Coffee",
  "Tea leaves" = "Tea"
)

# Simplify and group data
top_10_consumers_shares <- top_10_consumers_shares %>%
  mutate(
    producer = sub(" .*", "", prod_commodity_name),
    commodity_name = sub("^[A-Z]{3} ", "", prod_commodity_name),  # Extract only the commodity name
    simplified_name = recode(commodity_name, !!!simplified_names),  # Simplify names
    legend_label = paste(simplified_name, producer, sep = " "),         # Combine for legend
    legend_group = factor(simplified_name, levels = c("Coffee", "Cocoa", "Tea", "Tobacco", "other"))  # Order groups
  ) %>%
  arrange(legend_group) %>%
  mutate(legend_label = factor(legend_label, levels = unique(legend_label))) # Ensure correct order

# Change 'other other' to 'other'
top_10_consumers_shares <- top_10_consumers_shares %>%
  mutate(legend_label = recode(legend_label, "other other" = "other"))


# Generate colors for legend labels
prod_commodity_colors <- setNames(
  unlist(lapply(levels(top_10_consumers_shares$legend_group), function(group) {
    # Filter rows for the current group
    group_rows <- top_10_consumers_shares %>% filter(legend_group == group)
    n <- n_distinct(group_rows$legend_label)  # Count unique legend labels for this group
    commodity_palettes[[group]](n)  # Generate n shades for this group
  })),
  levels(top_10_consumers_shares$legend_label)  # Match colors to legend_label levels
)

# Create the bar plot
bar_plot <- ggplot(
  top_10_consumers_shares,
  aes(
    x = reorder(consumer, -total_footprint),
    y = total_footprint,
    fill = legend_label
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::scientific_format()) +
  scale_fill_manual(
    values = prod_commodity_colors,
    guide = guide_legend(title = "Commodity and Producer")
  ) +
  labs(
    #title = "Top Biodiversity Footprint Contributions by Consuming Country",
    x = "Consuming Country",
    y = "Biodiversity Footprint (PDF)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.7, "cm"),
    legend.position = c(0.95,0.95),
    legend.justification = c(1, 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print the plot
print(bar_plot)

ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar.png",
       plot = bar_plot,
       width = 180,
       height = 140,
       unit = 'mm',
       dpi = 300)


# Save the total impacts stacked bar chart as SVG
ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar.svg",
       plot = bar_plot,
       width = 180,
       height = 140,
       unit = 'mm')











# THE SAME BUT PER CAPITA-------------------------------------------------------
top_10_consumers_per_cap <- as.data.frame(colSums(commodities_to_assess, na.rm = TRUE))
colnames(top_10_consumers_per_cap) <- "total_footprint"
top_10_consumers_per_cap <- top_10_consumers_per_cap %>%
  mutate(consuming_country = as.integer(rownames(top_10_consumers_per_cap))) %>%
  # Convert area code to iso3c
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = iso3c) %>%
  # Join with world bank data for populations
  left_join(world_bank_pop[world_bank_pop$indicator == "SP.POP.TOTL", ][c("country", "2017")], 
            by = c("consumer" = "country")) %>%
  rename(pop_est = "2017") %>%
  mutate(total_footprint_per_cap = if_else(pop_est == 0, 0, total_footprint / pop_est)) %>%
  # Remove unnecessary rows
  dplyr::select(-c(total_footprint, consuming_country, continent, area)) %>%
  arrange(desc(total_footprint_per_cap)) %>%
  # Filter for countries with population greater than 1 million
  filter(pop_est >= 1000000)  %>%
  dplyr::select(-c(pop_est)) %>%
  # Take top 20? Consumers
  head(25) #%>%

# Calculate the share of production countries and commodities for the top 10 consumers
top_10_consumers_shares_per_cap <- commodities_to_assess[, colnames(commodities_to_assess) %in% regions$area_code[regions$iso3c %in% top_10_consumers_per_cap$consumer]]
# Convert to long format
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  rownames_to_column("prod_commodity") %>%
  pivot_longer(cols = -prod_commodity, names_to = "consuming_country", values_to = "footprint")  
# Separate producer and commodity
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  separate(prod_commodity, into = c("commodity", "producing_country"), sep = "_", remove = TRUE)
# Convert country codes to integers for later join
top_10_consumers_shares_per_cap$consuming_country <- as.integer(top_10_consumers_shares_per_cap$consuming_country)
top_10_consumers_shares_per_cap$producing_country <- as.integer(top_10_consumers_shares_per_cap$producing_country)
# Replace consuming country codes with names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = area) %>%
  dplyr::select(-c(consuming_country, iso3c, continent))
# Replace producing country codes with names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(regions, by = c("producing_country" = "area_code")) %>%
  rename(producer = area) %>%
  dplyr::select(-c(producing_country, iso3c, continent))
# Replace commodity codes with commodity names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(items, by = c("commodity" = "comm_code")) %>%
  rename(commodity_name = item) %>%
  dplyr::select(-c(commodity, item_code, unit, group, moisture, feedtype, comm_group))
# Arrange in descending order for visualization
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  arrange(desc(footprint))
# Change names to iso3
top_10_consumers_shares_per_cap <-  top_10_consumers_shares_per_cap %>% 
  left_join(regions, by = c("consumer" = "area")) %>%
  dplyr::select(-c(consumer, area_code, continent)) %>%
  rename(consumer = iso3c) %>%
  left_join(regions, by = c("producer" = "area")) %>%
  dplyr::select(-c(producer, area_code, continent)) %>%
  rename(producer = iso3c) #%>%
# Summarise the footprint contributions
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  group_by(consumer, commodity_name, producer) %>%
  summarise(total_footprint = sum(footprint, na.rm = TRUE), .groups = 'drop')
# Identify top contributors and group the rest as 'Other'
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  group_by(consumer) %>%
  mutate(rank = rank(-total_footprint, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    # Only keep top 3 contributors per consumer, and group others as "Other"
    prod_commodity_name = ifelse(rank <= 2, paste(producer, commodity_name, sep = "_"), "other")
  ) %>%
  group_by(consumer, prod_commodity_name) %>%
  summarise(total_footprint = sum(total_footprint, na.rm = TRUE), .groups = 'drop') %>%
  # Join with world bank data for populations
  left_join(world_bank_pop[world_bank_pop$indicator == "SP.POP.TOTL", ][c("country", "2017")], 
            by = c("consumer" = "country")) %>%
  rename(pop_est = "2017") %>%
  mutate(total_footprint_per_cap = if_else(pop_est == 0, 0, total_footprint / pop_est)) %>%
  dplyr::select(-c(total_footprint, pop_est)) %>%
  # Remove underscore from commodity-countries
  mutate(prod_commodity_name = as.character(prod_commodity_name)) %>%
  mutate(prod_commodity_name = str_replace_all(prod_commodity_name, "_", " "))


# Simplify and group data for legend simplicity
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  mutate(
    producer = sub(" .*", "", prod_commodity_name),
    commodity_name = sub("^[A-Z]{3} ", "", prod_commodity_name),  # Extract only the commodity name
    simplified_name = recode(commodity_name, !!!simplified_names),  # Simplify names
    legend_label = paste(simplified_name, producer, sep = " "),         # Combine for legend
    legend_group = factor(simplified_name, levels = c("Coffee", "Cocoa", "Tea", "Tobacco", "other"))  # Order groups
  ) %>%
  arrange(legend_group) %>%
  mutate(legend_label = factor(legend_label, levels = unique(legend_label))) # Ensure correct order

# Change 'other other' to 'other'
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  mutate(legend_label = recode(legend_label, "other other" = "other"))


# Generate colors for legend labels
prod_commodity_colors <- setNames(
  unlist(lapply(levels(top_10_consumers_shares_per_cap$legend_group), function(group) {
    # Filter rows for the current group
    group_rows <- top_10_consumers_shares_per_cap %>% filter(legend_group == group)
    n <- n_distinct(group_rows$legend_label)  # Count unique legend labels for this group
    commodity_palettes[[group]](n)  # Generate n shades for this group
  })),
  levels(top_10_consumers_shares_per_cap$legend_label)  # Match colors to legend_label levels
)

# Create the bar plot
bar_plot_per_cap <- ggplot(
  top_10_consumers_shares_per_cap,
  aes(
    x = reorder(consumer, -total_footprint_per_cap),
    y = total_footprint_per_cap,
    fill = legend_label
  )
) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::scientific_format()) +
  scale_fill_manual(
    values = prod_commodity_colors,
    guide = guide_legend(title = "Producer & Commodity")
  ) +
  labs(
    # title = "Top Biodiversity Footprint Contributions Per Capita by Consuming Country",
    x = "Consuming Country",
    y = "Biodiversity Footprint per Capita (PDF/cap)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.6, "cm"),
    legend.position = c(0.95,0.95),
    legend.justification = c(1, 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print the plot
print(bar_plot_per_cap)


ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar_per_cap.png",
       plot = bar_plot_per_cap,
       width = 180,
       height = 140,
       unit = 'mm',
       dpi = 300)


# Save the second stacked bar chart (per capita) as SVG
ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar_per_cap.svg",
       plot = bar_plot_per_cap,
       width = 180,
       height = 140,
       unit = 'mm')











############################################# PATTERNS??? ######################


top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  mutate(
    pattern = case_when(
      simplified_name == "Coffee" & producer == "BRA" ~ "stripe",
      simplified_name == "Cocoa" & producer == "BRA" ~ "stripe",
      simplified_name == "Coffee" & producer == "CIV" ~ "circle",
      simplified_name == "Cocoa" & producer == "CIV" ~ "circle",
      simplified_name == "Tea" & producer == "LKA" ~ "square",
      simplified_name == "Tobacco" & producer == "GTM" ~ "diamond",
      TRUE ~ "none"  # Default pattern for others
    )
  ) %>%
  mutate(pattern = ifelse(pattern == "none", NA, pattern))

bar_plot_per_cap <- ggplot(
  top_10_consumers_shares_per_cap,
  aes(
    x = reorder(consumer, -total_footprint_per_cap),
    y = total_footprint_per_cap,
    fill = legend_label,
    pattern = pattern  # Add pattern aesthetic
  )
) +
  ggpattern::geom_bar_pattern(
    stat = "identity", 
    position = "stack", 
    pattern_density = 0.1,  # Adjust the density of patterns
    pattern_fill = "white",  # Pattern color (you can change this)
    pattern_spacing = 0.02  # Adjust spacing between pattern elements
  ) +
  scale_y_continuous(labels = scales::scientific_format()) +
  scale_fill_manual(
    values = prod_commodity_colors,
    guide = guide_legend(title = "Producer & Commodity")
  ) +
  # scale_pattern_manual(
  #   values = prod_commodity_colors,  # Use the same color palette for patterns
  #   guide = guide_legend(title = "Producer & Commodity")  # Combine pattern into the same legend
  # ) +
  labs(
    title = "Top Biodiversity Footprint Contributions Per Capita by Consuming Country",
    x = "Consuming Country",
    y = "Biodiversity Footprint per Capita (PDF/cap)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),
    legend.position = c(1,1),
    legend.justification = c(1, 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Print the plot
print(bar_plot_per_cap)





##############################################################################

# Calculate the contribution of each matrix as a percentage of the total
contribution_land <- bd_fp_landuse_grouped_by_sp / bd_fp_total
contribution_water <- bd_fp_water / bd_fp_total
contribution_n <- bd_fp_water / bd_fp_total
contribution_p <- bd_fp_P / bd_fp_total

# Restore rownames of the dataframe
rownames(contribution_land) <- rownames(bd_fp_water)
rownames(contribution_water) <- rownames(bd_fp_water)
rownames(contribution_n) <- rownames(bd_fp_water)
rownames(contribution_p) <- rownames(bd_fp_water)

# Filter for only Stim commodites
# contribution_land <- contribution_land[sub("_.*", "", rownames(contribution_land)) %in% items_stim, ]
# contribution_water <- contribution_water[sub("_.*", "", rownames(contribution_water)) %in% items_stim, ]
# contribution_n <- contribution_n[sub("_.*", "", rownames(contribution_n)) %in% items_stim, ]
# contribution_p <- contribution_p[sub("_.*", "", rownames(contribution_p)) %in% items_stim, ]

# Sum across rows
contribution_land_sum <- data.frame(land = colSums(contribution_land, na.rm = TRUE))
contribution_water_sum <- data.frame(water = colSums(contribution_water, na.rm = TRUE))
contribution_n_sum <- data.frame(n = colSums(contribution_n, na.rm = TRUE))
contribution_p_sum <- data.frame(p = colSums(contribution_p, na.rm = TRUE))

# Combine into to single df
footprint_contributions_sums_combined <- as.data.frame(cbind(land = contribution_land_sum,
                                                    water = contribution_water_sum,
                                                    n = contribution_n_sum,
                                                    p = contribution_p_sum))
# Create another columns with totals
footprint_contributions_sums_combined$total <- rowSums(footprint_contributions_sums_combined)
# footprint_contributions_sums_combined$total <- footprint_contributions_sums_combined$land


# Arrange in descending order for visualization
footprint_contributions_top_25 <- footprint_contributions_sums_combined %>%
  arrange(desc(total))
footprint_contributions_top_25 <- head(footprint_contributions_top_25, 25)

# Reshape into long format
footprint_contributions_top_25 <- footprint_contributions_top_25 %>%
  rownames_to_column(var = "consuming_country") %>%  # Convert rownames into a column
  pivot_longer(cols = c(land, water, n, p), names_to = "footprint_type", values_to = "contribution")

footprint_contributions_top_25 <- footprint_contributions_top_25 %>%
  mutate(country = as.integer(sub("_.*", "", consuming_country)),
         consumption_category = sub(".*_", "", consuming_country)) %>%
  left_join(regions, by = c("country" = "area_code")) %>%
  dplyr::select(-c(consuming_country, country, area, continent))

# PLOT!
ggplot(footprint_contributions_top_25, aes(x = reorder(iso3c, -total), y = contribution, fill = footprint_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Footprint by Consuming Country",
       x = "Consuming Country",
       y = "Total Footprint",
       fill = "Footprint Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


###############################################################################
# species contributions. Eh.

bd_fp_landuse_stim <- bd_fp_landuse[sub("_.*", "", rownames(bd_fp_landuse)) %in% items_stim, ]

# Convert the row names to a column to extract species type
df_species <- as.data.frame(bd_fp_landuse_stim) %>%
  rownames_to_column("country_commodity_species")

# Separate the row name into country, commodity, and species
df_species <- df_species %>%
  separate(country_commodity_species, into = c("commodity", "country","species_type"), sep = "_")

# Sum the impacts by species type, selecting only numeric columns for summing
df_summary_species <- df_species %>%
  group_by(species_type) %>%
  summarise(total_impact = sum(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), na.rm = TRUE), .groups = 'drop')

# Create a pie chart of total impact by species type
ggplot(df_summary_species, aes(x = "", y = total_impact, fill = species_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Total Biodiversity Impact by Species Type") +
  theme_void() +  # Clean theme for pie chart
  theme(legend.title = element_blank())











