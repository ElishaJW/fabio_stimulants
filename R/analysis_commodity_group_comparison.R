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
str_fp_CO2 <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['CO2']]
str_fp_CH4 <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['CH4']]
str_fp_NO2 <- readRDS(paste0("./data/str_footprint_",year,".rds"))[['NO2']]


# World map with country info
world_sf <- ne_countries(scale = "medium", returnclass = "sf")



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
bd_fp_total


#################################################################################


# Reshape biodiversity impact matrix to long format
bd_long <- bd_fp_total %>%
  mutate(comm_code = rep(items$comm_code, each = length(unique(regions$area_code)))) %>% # Assign commodity codes
  mutate(producer = rep(regions$area_code, length.out = nrow(bd_fp_total))) %>% # Assign producer country codes
  pivot_longer(
    cols = -c(comm_code, producer),  # Keep commodity-country pairs intact
    names_to = "cons_category", 
    values_to = "impact") %>%
  # Extract consumer country codes from column names (before "_")
  mutate(consumer = as.numeric(sub("_.*", "", cons_category))) %>%
  # Merge with items to get commodity categories
  left_join(items[, c("comm_code", "comm_group")],
            by = "comm_code") %>%
  # Merge with regions to get continent info
  left_join(regions[, c("area_code", "continent", "iso3c")],
            by = c("consumer" = "area_code")) %>%
  # Merge with world data for pop estimates
  left_join(world_sf[, c("pop_est", "iso_a3")],
            by = c("iso3c" = "iso_a3")) %>%
  mutate(impact_per_cap = if_else(pop_est == 0, 0, impact / pop_est)) %>%
  dplyr::select(-c(geometry, pop_est, iso3c))

# Aggregate impacts by commodity category and consuming region
impact_summary <- bd_long %>%
  group_by(comm_group, continent) %>%
  summarise(total_impact = sum(impact_per_cap, na.rm = TRUE), .groups = "drop") %>%
  # Reshape for easier comparison across commodities
  pivot_wider(names_from = continent, values_from = total_impact) %>%
  filter(!comm_group %in% c("Animal fats", "Live animals", "Meat", "Hides, skins, wool", "Grazing", "Fish", "Milk", "Eggs", "Fodder crops", "Ethanol", "Oil cakes"))

# Convert impact_wide from wide to long format for ggplot
impact_summary <- impact_summary %>%
  pivot_longer(cols = -comm_group, names_to = "Region", values_to = "Impact")

# Stacked bar plot
ggplot(impact_summary, aes(x = Region, y = Impact, fill = comm_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Biodiversity Impact of Different Commodity Categories Across Regions",
    x = "Consuming Region",
    y = "Total Biodiversity Impact",
    fill = "Commodity Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








