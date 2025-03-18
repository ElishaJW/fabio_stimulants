
# VISUALIZING MRIO DATA AND COMPARING WITH MASTER Z MATRIX
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# Load necessary libraries
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)


# Set Paths
# fabio_master_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_master"
# fabio_stimulant_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants"


# Load Data
Z <- readRDS("./data/Z_mass.rds")
X <- readRDS("./data/X.rds")
Y <- readRDS("./data/Y.rds")

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


#------------------IO Math & Stressor Footprinting------------------------------

years <- 2010:2020

# Starting with multiplying L and Y for each year to get total cons.

Ly <- list()

for (year in years) {
  
  # Access the L matrices for the current year
  L_year <- get(paste0("L_", year))
  
  # Extract the corresponding slice from the 3D Y matrix
  Y_year <- Y[[as.character(year)]]

  print(paste0("Calculating L*y for year ", year))
  
  # Multiply
  Ly_year <- L_year %*% Y_year
  
  # Store the result in the 3D array
  Ly[[as.character(year)]] <- Ly_year
  
}


# Need to sum the emissions stressors for each unique GHG (e.g., sum all CH4),
# to simplify footprinting later

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



# Now we multiply Ly for each year by a diagonalized stressor matrix to get the
# source of each stressor by country nd commodity for a given final demand. I.e.
# the stressor footprints! Characterizations come later.

footprint <- list()

for (i in 1:length(years)) {
  # Get the corresponding LY matrix for the current year
  year <- years[i]
  Ly_year <- Ly[[i]]
  
  # Access the stressor matrix for the current year using get()
  E_year <- get(paste0("E_", year))
  
  # Initialize a sublist for the current year to store results by stressor
  footprint_year <- list()
  
  # Track year
  print(paste0("For year ", year, "..."))
  
  # Inner loop over each stressor
  for (s in 1:nrow(E_year)) {
    # Track stressor
    print(paste("Calculating", stressor_list[s], "footprints"))
    
    # Get the stressor vector for the current stressor (row of E_year)
    e <- as.vector(E_year[s, ])
    
    # Diagonalize the stressor vector
    e_diag <- diag(e)
    
    # Multiply the diagonalized stressor matrix by the LY matrix
    footprint_e <- e_diag %*% Ly_year
    
    # Store the result in the sublist for the current year and stressor
    footprint_year[[stressor_list[s]]] <- footprint_e
  }
  
  # Store the sublist in the main list with the year as the key
  footprint[[as.character(year)]] <- footprint_year
  
  # Save as RDS files
  print("Saving as RDS file")
  saveRDS(footprint[[as.character(year)]], file=paste0("./data/str_footprint_", year, ".rds"))

}

# The 'footprint' list now contains a list for each year, with each sublist
# containing matrices for each stressor.

# Example: Access the footprint for the 1st stressor in 2010
#print(footprint[["2010"]][["landuse"]])



