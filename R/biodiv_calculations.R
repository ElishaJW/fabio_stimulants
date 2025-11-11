

library(dplyr)
library(tidyverse)
library(data.table)
library(openxlsx)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

# Load Data---------------------------------------------------------------------
year <- 2020
str_fp <- readRDS(paste0("./data/str_footprint_",year,".rds"))
items <- fread("inst/items_full.csv")
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" | items$comm_group == "Tobacco, rubber" & items$comm_code != 'c060']$comm_code
regions <- fread("inst/regions_fabio.csv", )
landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF_country.csv")
water_cf <- read.xlsx("X:/Eli/DATA/cf/water consumption/water.xlsx", sheet="Country_CF")
N_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/N/Country_CF_N.xlsx")
P_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/P/Country_CF_P.xlsx")
cc_cf <- read.xlsx("X:/Eli/DATA/cf/climate change/GLAM_template_EQ_Climate_Change_FW_TR_MA.xlsx", sheet="lciamethods_CF_GLAM")

#COMMODITY/COUNTRY GROUPS ------------------------------------------------------

# Create unique list of combos
combinations <- expand.grid(items$comm_code, regions$area_code)
# Combine the country and commodity codes to create row names
row_names <- paste(combinations$Var1, combinations$Var2, sep = "_")






############### BD FOOTPRINT BY COUNTRY and CONS CATEGORY ######################


#---------------------LAND USE BD FOOTPRINT BY COUNTRY--------------------------
# PROCESSING landuse CF-------------------------------------------------
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


# PROCESSING landuse FOOTRPINT MATRIX-------------------------------------------
str_fp_landuse <- str_fp[["landuse"]]
str_fp_landuse <- as.data.frame(as.matrix(str_fp_landuse))
rownames(str_fp_landuse) <- row_names
bd_fp_landuse <- str_fp_landuse

# MULTIPLY LANDUSE FOOTPRINT MATRIX WITH CF-------------------------------------
# Extract the country code from row names
bd_fp_landuse$area_code <- as.numeric(sub(".*_", "", rownames(bd_fp_landuse)))

# Extract commodity name as well
bd_fp_landuse$comm_code <- sub("_.*", "", rownames(bd_fp_landuse))

# Add column for corresponding iso3 code
bd_fp_landuse <- bd_fp_landuse %>%
  left_join(regions[, c('area_code', 'iso3c')], by = "area_code")

# Average landuse CF across species groups!!!!!
landuse_cf <- landuse_cf %>%
  group_by(iso3c) %>%
  summarise(across(CF_occ_avg_glo, mean, .groups = 'drop'))

# Join the land use dataframe with the characterization factors dataframe by country code
bd_fp_landuse <- bd_fp_landuse %>%
  left_join(landuse_cf, by = "iso3c")

# Multiply the footprint values by the characterization factors---------------
# Multiply all columns except 'country_code', 'comm_code' and rownames contained in the CF
cols_to_multiply <- colnames(bd_fp_landuse)[!colnames(bd_fp_landuse) %in% c("area_code", "comm_code", colnames(landuse_cf))]
bd_fp_landuse[cols_to_multiply] <- bd_fp_landuse[cols_to_multiply] * bd_fp_landuse$CF_occ_avg_glo

# Rename rows to correspond to comm, country, species group
rownames(bd_fp_landuse) <- paste(bd_fp_landuse$comm_code,
                                 bd_fp_landuse$area_code,
                                 #bd_fp_landuse$species_group,
                                 sep = "_")

# Drop unnecessary columns
bd_fp_landuse <- bd_fp_landuse[cols_to_multiply]




#-----------------------WATER USE BD FOOTPRINT BY COUNTRY-----------------------
# PROCESSING WATER USE CF------------------------------------------------------
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
str_fp_water <- str_fp[["blue"]]
rownames(str_fp_water) <- row_names
str_fp_water <- as.data.frame(as.matrix(str_fp_water)) 
str_fp_water <- str_fp_water

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



#--------------------N, P EMISSIONS BD FOOTPRINT BY COUNTRY---------------------

for (nutrient in c("N", "P")){
  
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
  
  #assign to variable
  assign(paste0(nutrient,"_cf"), nutrient_cf, envir = .GlobalEnv)

  
  # PROCESSING FOOTRPINT MATRIX-------------------------------------------------
  # Select footprint matrix
  if (nutrient == "N"){
    str_fp_nutrient <- str_fp[["n_application"]]
  } else if (nutrient == "P"){
    str_fp_nutrient <- str_fp[["p_application"]]
  }
  rownames(str_fp_nutrient) <- row_names
  str_fp_nutrient <- as.data.frame(as.matrix(str_fp_nutrient))
  str_fp_nutrient <- str_fp_nutrient
  
  # MULTIPLY FOOTPRINT MATRIX WITH CF-------------------------------------------
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


# ---------------------Climate Change Impacts by Country------------------------


cc_cf <- read.xlsx("X:/Eli/DATA/cf/climate change/GLAM_template_EQ_Climate_Change_FW_TR_MA.xlsx", sheet="lciamethods_CF_GLAM")

# Select only important columns
cc_cf <- cc_cf[, c("FLOW_name", "CF", "LCIAMethod_realm")]
cc_cf <- cc_cf[cc_cf$FLOW_name == "carbon dioxide" | cc_cf$FLOW_name == "methane" | cc_cf$FLOW_name == "nitrous oxide",]

cc_cf <- cc_cf %>%
  group_by(FLOW_name) %>%
  summarise(across(CF, mean, .groups = 'drop'))



# SUM THE MATRICES TOGETHER-----------------------------------------------------
# rownames(bd_fp_landuse_grouped_by_sp) <-   rownames(bd_fp_water)
# Simple sum of dataframes, first changing NA to 0 where necessary
bd_fp_landuse[is.na(bd_fp_landuse)] <- 0
bd_fp_water[is.na(bd_fp_water)] <- 0
bd_fp_water[is.na(bd_fp_water)] <- 0
bd_fp_P[is.na(bd_fp_P)] <- 0
bd_fp_total <-  bd_fp_landuse +
  bd_fp_water +
  bd_fp_water +
  bd_fp_P

rownames(bd_fp_total) <-   rownames(bd_fp_water)





############### BIODIVERSITY IMPACT MATRIX PER UNIT CONSUMPTION ################

# Essentially add bd footprint to E matrix--------------------------------------

E_2010 <- readRDS("./data/E_full_2010.rds") 
E_2011 <- readRDS("./data/E_full_2011.rds")   
E_2012 <- readRDS("./data/E_full_2012.rds")   
E_2013 <- readRDS("./data/E_full_2013.rds")   
E_2014 <- readRDS("./data/E_full_2014.rds")   
E_2015 <- readRDS("./data/E_full_2015.rds")   
E_2016 <- readRDS("./data/E_full_2016.rds")   
E_2017 <- readRDS("./data/E_full_2017.rds")   
E_2018 <- readRDS("./data/E_full_2018.rds")   
E_2019 <- readRDS("./data/E_full_2019.rds")   
E_2020 <- readRDS("./data/E_full_2020.rds")   

stressor_list <- read.csv("./data/stressor_list.csv")
stressor_list <- colnames(stressor_list)[2:ncol(stressor_list)]

#years <- 2010:2020
years <- 2010:2020

# Define a list of stressor names to sum contained in original stressor_list
stressors_to_sum <- c("CH4", "CO2", "N2O")

# Loop over each year in the list
for (year in years) {
  
  # Get the current stressor matrix
  E_name <- paste0("E_", year)
  E_year <- get(E_name)
  
  # Initialize an empty list to store summed rows
  summed_rows <- list()
  
  # Loop over each stressor name to sum rows
  for (stressor in stressors_to_sum) {
    # Identify the rows corresponding to the current stressor
    stressor_rows <- grep(stressor, rownames(E_year), ignore.case = TRUE)
    
    # Sum the rows corresponding to the current stressor
    stressor_sum <- colSums(E_year[stressor_rows, , drop = FALSE])
    
    # Store the summed rows in the list
    summed_rows[[stressor]] <- stressor_sum
  }
  
  # Remove the original detailed stressor rows from the matrix
  rows_to_remove <- unlist(lapply(stressors_to_sum, function(stressor) {
    grep(stressor, rownames(E_year), ignore.case = TRUE)
  }))
  E_new <- E_year[-rows_to_remove, , drop = FALSE]
  
  # Add the new summed rows to the matrix
  for (stressor in stressors_to_sum) {
    E_new <- rbind(E_new, summed_rows[[stressor]])
  }
  
  # Rename the rows to just "CH4", etc.
  rownames(E_new)[(nrow(E_new) - length(stressors_to_sum) + 1):nrow(E_new)] <- stressors_to_sum
  
  # Replace the original matrix in the list with the updated one
  assign(E_name, E_new)
}

# Get new stressor list
stressor_list <- rownames(E_2020)



# ---------------- ADD BIODIVERSITY IMPACTS TO E MATRICES --------------------

# Loop over each year
for (year in years) {
  
  E_name <- paste0("E_", year)
  E_year <- get(E_name)
  
  # Get column area codes (extract from column names like "1_c001")
  col_area <- as.numeric(sub("_.*", "", colnames(E_year)))
  col_map <- data.frame(area_code = col_area,
                        col_id = colnames(E_year),
                        stringsAsFactors = FALSE) %>%
    left_join(regions[, c('area_code', 'iso3c')], by = "area_code")
  
  # Create empty matrix for new BD rows
  bd_rows <- matrix(NA, nrow = 7, ncol = ncol(E_year))
  rownames(bd_rows) <- c("landuse_bd", "water_bd", "P_bd", "N_bd", "CH4_bd", "CO2_bd", "N2O_bd")
  colnames(bd_rows) <- colnames(E_year)
  
  # ---- Land use biodiversity impact
  landuse_row <- E_year["landuse", , drop = FALSE]
  landuse_cf_vec <- landuse_cf$CF_occ_avg_glo[match(col_map$iso3c, landuse_cf$iso3c)]
  bd_rows["landuse_bd", ] <- as.numeric(landuse_row) * landuse_cf_vec
  
  # ---- Water use biodiversity impact
  water_row <- E_year["blue", , drop = FALSE]
  water_cf_vec <- water_cf$CF_GLOB_A_m[match(col_map$iso3c, water_cf$iso3c)]
  bd_rows["water_bd", ] <- as.numeric(water_row) * water_cf_vec
  
  # ---- Phosphorus biodiversity impact
  P_row <- E_year["p_application", , drop = FALSE]
  P_cf_vec <- P_cf$CF_avg_diff[match(col_map$iso3c, P_cf$iso3c)]
  bd_rows["P_bd", ] <- as.numeric(P_row) * P_cf_vec
  
  # ---- Nitrogen biodiversity impact
  N_row <- E_year["n_application", , drop = FALSE]
  N_cf_vec <- N_cf$CF_avg_diff[match(col_map$iso3c, N_cf$iso3c)]
  bd_rows["N_bd", ] <- as.numeric(N_row) * N_cf_vec
  
  # ---- Climate change biodiversity impact
  bd_rows["CO2_bd", ] <- as.numeric(E_year["CH4", , drop = FALSE]) * cc_cf$CF[cc_cf$FLOW_name == "methane"]
  bd_rows["CH4_bd", ] <- as.numeric(E_year["CO2", , drop = FALSE]) * cc_cf$CF[cc_cf$FLOW_name == "carbon dioxide"]
  bd_rows["N2O_bd", ] <- as.numeric(E_year["N2O", , drop = FALSE]) * cc_cf$CF[cc_cf$FLOW_name == "nitrous oxide"]
  
  # ---- Drop NA to 0
  bd_rows[is.na(bd_rows)] <- 0
  
  # ---- Append to E_year
  E_new <- rbind(E_year, bd_rows)
  
  # Save back into environment
  assign(E_name, E_new)
  
  # Save
  saveRDS(E_name, file=paste0("./data/E_bd_", year, ".rds"))
}

# Update stressor list to include biodiversity impacts
stressor_list <- rownames(E_2020)


#------------------ Convert prod. to cons. with L ------------------------------
# Multiply updated E (S) matrix with with Leontief inverse to get full impacts through
#   supply chain

L_2010 <- readRDS("./data/2010_L_mass.rds")           
L_2011 <- readRDS("./data/2011_L_mass.rds")       
L_2012 <- readRDS("./data/2012_L_mass.rds")       
L_2013 <- readRDS("./data/2013_L_mass.rds")       
L_2014 <- readRDS("./data/2014_L_mass.rds")       
L_2015 <- readRDS("./data/2015_L_mass.rds")       
L_2016 <- readRDS("./data/2016_L_mass.rds")       
L_2017 <- readRDS("./data/2017_L_mass.rds")       
L_2018 <- readRDS("./data/2018_L_mass.rds")       
L_2019 <- readRDS("./data/2019_L_mass.rds")       
L_2020 <- readRDS("./data/2020_L_mass.rds") 


EL <- list()

for (i in 1:length(years)) {
  # Get the corresponding LY matrix for the current year
  year <- years[i]
  L_year <- get(paste0("L_", year))
  
  # Access the stressor matrix for the current year using get()
  E_year <- get(paste0("E_", year))
  
  # Track year
  print(paste0("For year ", year, "..."))
  
  # Multiply the diagonalized stressor matrix by the LY matrix
  EL_year <- E_year %*% L_year
  
  # Store the sublist in the main list with the year as the key
  EL[[as.character(year)]] <- EL_year
  
  rownames(EL[[as.character(year)]]) <- rownames(E_2020)
  colnames(EL[[as.character(year)]]) <- colnames(E_2020)
  
  # Save as RDS files
  print("Saving...")
  # saveRDS(EL[[as.character(year)]], file=paste0("./exporters_to_country_RAINFOREST/EL", year, ".rds"))
  write.csv(
    as.matrix(EL[[as.character(year)]]),
    paste0("./exporters_to_country_RAINFOREST/EL_",year,"_biodiversity.csv"),
    row.names = TRUE,
    col.names = TRUE
  )
}
