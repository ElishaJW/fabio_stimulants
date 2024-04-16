
# VISUALIZING MRIO DATA AND COMPARING WITH MASTER Z MATRIX
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# Load necessary libraries
library(tidyverse)
library("data.table")
library(dplyr)
library(ggplot2)
source("R/01_tidy_functions.R")


# Set paths
fabio_master_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\FABIO Development\\fabio_master"
fabio_stimulant_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\FABIO Development\\fabio_stimulants"


# Load Data
z_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\Z_mass.rds"))
z_master <- readRDS(paste0(fabio_master_path, "\\data\\Z_mass.rds"))

x_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\X.rds"))
x_master <- readRDS(paste0(fabio_master_path, "\\data\\X.rds"))
                  
y_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\Y.rds"))
y_master <- readRDS(paste0(fabio_master_path, "\\data\\Y.rds"))                  

sup_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\sup_final.rds"))
mr_sup_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\mr_sup_mass.rds")) 

regions <- read.csv(paste0(fabio_stimulant_path, "\\inst\\regions_fabio.csv"))
items <- read.csv(paste0(fabio_stimulant_path, "\\inst\\items_full.csv"))




######################## TESTING ###########################
######## Asssessing differences between Z matrices #########

# # Specify the string to search for
filter_commodity <- "c045"
# Index 2805 for Brazil (?)
mr_sup_coffee <- mr_sup_stim[['2018']]#[comm_code == filter_commodity][area == 'Brazil']

# gx <- list(mr_sup_stim[["2018"]])
# gx2 <- as.data.frame(as.matrix(gx))

gx <- as(mr_sup_stim[["2018"]], "matrix")
gx2 <- as.data.frame(gx)
gx2_sum <- rowSums(gx2)



########### Production over time for a single country ##################
########### Comparison between old and new data ########################
########### Only works for Tea leaves, Cocoa beans, & Coffee, green ####


graph_stuff <- function(country, commodity){
  CountryToPlot <- country
  CommToPlotName <- commodity
  
  # Convert commodity name to number
  CommToPlot <- items$comm_code[items$item == CommToPlotName]
  
  # Subset the x vector for the specified area and item
  x_comm_stim <- x_stim[grep(CommToPlot, rownames(x_stim)), ]
  rownames(x_comm_stim) <- rownames(regions)
  x_comm_stim <- x_comm_stim[which(regions$area == CountryToPlot), ]
  
  # Subset the x vector for the specified area and item
  x_comm_master <- x_master[grep(CommToPlot, rownames(x_master)), ]
  rownames(x_comm_master) <- rownames(regions)
  x_comm_master <- x_comm_master[which(regions$area == CountryToPlot), ]
  
  # Subset the x vector for the specified area and item
  sup_stim_comm <- sup_stim[comm_code == CommToPlot][area == CountryToPlot]
  # sup_stim_comm <- sup_stim_comm[year == 2018]
  
  
  # Combine into single df with overlapping years
  # df for master data
  df_master <- data.frame(x_comm_master)
  df_master <- rownames_to_column(df_master, var = "year")
  # df for stim data
  df_stim <- data.frame(x_comm_stim)
  df_stim <- rownames_to_column(df_stim, var = "year")
  # merged...
  combined_df_2 <- merge(df_master, df_stim, by = "year", all = TRUE)
  # df for supply data
  df_sup <- data.frame(sup_stim_comm$year, sup_stim_comm$production)
  colnames(df_sup) <- c("year", "supply")
  # merged...
  combined_df <- merge(combined_df_2, df_sup, by = "year", all = TRUE)
  # Put 0 values where data unavailable
  # combined_df$x_comm_stim[combined_df$x_comm_stim == 0] <- NA
  combined_df$x_comm_master[is.na(combined_df$x_comm_master)] <- 0
  combined_df$supply[is.na(combined_df$supply)] <- 0
  # Rename columns
  colnames(combined_df)[colnames(combined_df) == "x_comm_master"] <- "master"
  colnames(combined_df)[colnames(combined_df) == "x_comm_stim"] <- "stim"
  
  if (CommToPlotName == 'Tea leaves') {
    combined_df <- combined_df[combined_df$year <= 2019, ]
  }
  
  ggplot(combined_df, aes(x = year, group=1)) +
    geom_line(aes(y = stim, # stim[stim != 0],
                  # x = year[stim != 0],
                  color = "Stimulants")) +
    geom_line(aes(y = master,
                  color = "Master")) +
    geom_line(aes(y = supply,
                  color = "Supply")) +
    labs(title = paste("Annual Production of", CommToPlotName, "in", CountryToPlot, "with New and Old Data"),
         x = "Year",
         y = "Production (tonnes)",
         caption = "Source: FAOSTAT",
         color = "Legend") +
    scale_color_manual(values = c("Master" = "red", "Stimulants" = "blue", "Supply" = "darkgreen"),
                       labels = c("Master", "Stimulants", "Supply (stim)")) +
    theme_minimal() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}


graph_stuff("Brazil", "Tea leaves")



##### ONLY FOR ADDED STIMULANT COMMODITIES ##################

graph_stuff_new_comms <- function(country, commodity){
  CountryToPlot <- country
  CommToPlotName <- commodity
  
  # Convert commodity name to number
  CommToPlot <- items$comm_code[items$item == CommToPlotName]
  
  # Subset the x vector for the specified area and item
  x_comm_stim <- x_stim[grep(CommToPlot, rownames(x_stim)), ]
  rownames(x_comm_stim) <- rownames(regions)
  x_comm_stim <- x_comm_stim[which(regions$area == CountryToPlot), ]
  
  # Subset the x vector for the specified area and item
  sup_stim_comm <- sup_stim[comm_code == CommToPlot][area == CountryToPlot]
  # sup_stim_comm <- sup_stim_comm[year == 2018]
  
  
  # Combine into single df with overlapping years
  # df for stim data
  df_stim <- data.frame(x_comm_stim)
  df_stim <- rownames_to_column(df_stim, var = "year")
  # df for supply data
  df_sup <- data.frame(sup_stim_comm$year, sup_stim_comm$production)
  colnames(df_sup) <- c("year", "supply")
  # merged...
  combined_df <- merge(df_stim, df_sup, by = "year", all = TRUE)
  # Put 0 values where data unavailable
  combined_df$supply[is.na(combined_df$supply)] <- 0
  # Rename columns
  colnames(combined_df)[colnames(combined_df) == "x_comm_stim"] <- "stim"
  # Cut off years at 2010
  combined_df <- combined_df[combined_df$year > 2009, ]
  
  if (CommToPlotName == 'Tea leaves') {
    combined_df <- combined_df[combined_df$year <= 2019, ]
  }
  
  ggplot(combined_df, aes(x = year, group=1)) +
    geom_line(aes(y = stim, # stim[stim != 0],
                  # x = year[stim != 0],
                  color = "Stimulants")) +
    geom_line(aes(y = supply,
                  color = "Supply")) +
    labs(title = paste("Annual Production of", CommToPlotName, "in", CountryToPlot, "with New and Old Data"),
         x = "Year",
         y = "Production (tonnes)",
         caption = "Source: FAOSTAT",
         color = "Legend") +
    scale_color_manual(values = c("Stimulants" = "blue", "Supply" = "darkgreen"),
                       labels = c("Stimulants", "Supply (stim)")) +
    theme_minimal() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}

graph_stuff_new_comms("Brazil", "Coffee, green")



##### ONLY FOR ADDED STIMULANT COMMODITIES ##################
##### COMPARING COUNTRIES #######


graph_top_countries <- function(number_countries, commodity_name){
  CommToPlotName <- commodity_name
  num_countries_to_plot <- number_countries
  
  # Convert commodity name to number
  CommToPlot <- items$comm_code[items$item == CommToPlotName]
  
  # Subset the x vector for the specified area and item
  x_comm_stim <- x_stim[grep(CommToPlot, rownames(x_stim)), ]
  rownames(x_comm_stim) <- regions$area
  
  
  #TOP 5 Countries That produced commodity in 2021 in stimulant x vector
  x_comm_stim_2021 <- data.frame(production_2021 = x_comm_stim[, '2021'])
  # x_stim_sorted <- sort(x_comm_stim_2021, decreasing = TRUE)
  x_stim_sorted <- x_comm_stim_2021[order(x_comm_stim_2021$production_2021, decreasing = TRUE), , drop = FALSE]
  top_countries <- rownames(x_stim_sorted)[1:num_countries_to_plot+1]
  x_comm_stim <- x_comm_stim[top_countries, ]
  
  
  # Subset the x vector for the specified area and item
  x_comm_master <- x_master[grep(CommToPlot, rownames(x_master)), ]
  rownames(x_comm_master) <- regions$area
  x_comm_master <- x_comm_master[top_countries, ]
  
  # Subset the supply data for the specified area and item NOT USEFUL HERE
  # sup_stim_comm <- sup_stim[comm_code == CommToPlot][area %in% top_countries,]
  # sup_stim_comm <- sup_stim_comm[year == 2021]
  # df_sup <- data.frame(sup_stim_comm$area, sup_stim$year, sup_stim_comm$production)
  
  # Combine into single df with overlapping years
  # df for master data
  df_master <- data.frame(x_comm_master)
  colnames(df_master) <- gsub("X", "", colnames(df_master))
  df_master <- rownames_to_column(df_master, var = "country")
  df_master_long <- pivot_longer(df_master, cols = -country, names_to = "year", values_to = "value")
  df_master_long$country[df_master_long$country == 'C\xf4te d\x92Ivoire'] <- 'Cote dIvoire'
  df_master_long$country <- paste(df_master_long$country,"(master)", sep = " ")
  
  
  # df for stim data
  df_stim <- data.frame(x_comm_stim)
  colnames(df_stim) <- gsub("X", "", colnames(df_stim))
  df_stim <- rownames_to_column(df_stim, var = "country")
  df_stim_long <- pivot_longer(df_stim, cols = -country, names_to = "year", values_to = "value")
  df_stim_long$country[df_stim_long$country == 'C\xf4te d\x92Ivoire'] <- 'Cote dIvoire'
  df_stim_long$country <- paste(df_stim_long$country,"(stim)", sep = " ")
  
  # Merge data
  combined_df <- merge(df_stim_long, df_master_long, by = c("year", "country", "value"), all = TRUE)
  # Put 0 values where data unavailable, then cut out
  combined_df[is.na(combined_df)] <- 0
  combined_df <- subset(combined_df, value != 0)

  print(top_countries)
  
  if (CommToPlotName == 'Tea leaves') {
    combined_df <- combined_df[combined_df$year <= 2019, ]
  }
  
  ggplot(combined_df, aes(x = year, y = value, group = country, color = country)) +
    geom_line(size = 1) +
    labs(title = "Production Over Time",
         x = "Year",
         y = "Production") +
    scale_color_manual(values = c("pink",
                                  'red',
                                  'lightblue',
                                  'blue',
                                  'yellow',
                                  'gold',
                                  'lightgreen',
                                  'green',
                                  'grey',
                                  'black'
    )) +
    theme_minimal() +
    theme(legend.position = "right") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
}


graph_top_countries(5, "Coffee, green")


