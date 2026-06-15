
# VISUALIZING MRIO DATA WITH MAPS!
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

install.packages(c("viridis", "scico", "RColorBrewer"))

# Load necessary libraries
library(tidyverse)
library("data.table")
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library("RColorBrewer")
library(paletteer)

source("R/01_tidy_functions.R")

'-------------------------------------------------------------------------------'

# Set paths
fabio_master_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_master"
fabio_stimulant_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants"


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

'------------------------------------------------------------------------------'

# Load Data
# z_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\Z_mass.rds"))
# z_master <- readRDS(paste0(fabio_master_path, "\\data\\Z_mass.rds"))

x_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\X.rds"))
x_master <- readRDS(paste0(fabio_master_path, "\\data\\X.rds"))

y_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\Y.rds"))
y_master <- readRDS(paste0(fabio_master_path, "\\data\\Y.rds"))                  

# sup_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\sup_final.rds"))
# mr_sup_stim <- readRDS(paste0(fabio_stimulant_path, "\\data\\mr_sup_mass.rds")) 

regions <- read.csv(paste0(fabio_stimulant_path, "\\inst\\regions_fabio.csv"))
items <- read.csv(paste0(fabio_stimulant_path, "\\inst\\items_full.csv"))

world_blank <- ne_countries(scale = "medium", returnclass = "sf")

'------------------------------------------------------------------------------'
################ GRAPHING CONSUMPTION BY COUNTRY ###############################

# Select commodity to assess
#filter_commodity <- "c131" # Coffee roasted 
#filter_commodity <- "c130" # Chocolate products nes
#filter_commodity <- "c047" # Tea
#filter_commodity <- "c059" # Tobacco

# Select Y matrix and year to assess for consumption
year <- '2020'
y <- y_stim[[year]]
num_countries_to_plot <- 50

for (filter_commodity in c("c131", "c130", "c047", "c059")){
  # Filter rows of y for commodity then sum for single stim commodity
  y_comm <- colSums(y[rownames(y) == filter_commodity, ])
  
  # If we want to sum by all commodity for one stimulant type:
  #y_comm <- colSums(y[rownames(y) %in% items$comm_code[items$item %in% items_coff], ])
  
  # Get the positional index of the top 10 values in y
  top_indices <- order(y_comm, decreasing = TRUE)
  top_values <- y_comm[top_indices]
  # Convert positional index to to column name
  top_indices <- colnames(y)[top_indices]
  # Remove consumption category to get just the country
  top_countries <- gsub("_.*", "", top_indices)
  # Get country name
  #top_countries <- regions$area[regions$area_code == as.integer(top_countries)]
  top_countries <- left_join(data.frame(area_code = as.integer(top_countries)),
                             regions,
                             by = "area_code")$area
  # Same, but to get consumption category
  top_countries_cons <- gsub("^[0-9]+_", "", top_indices)
  # Combine country name and production data into a df
  top_consumption <- data.frame(country=top_countries,
                               category=top_countries_cons,
                               consumption=top_values)
  if (filter_commodity == 'c059'){ #Unmanufactured tobacco
    top_consumption <- top_consumption[top_consumption$category == 'unspecified', ]
  } else if (filter_commodity == 'c138') { #Cigarettes
    top_consumption <- top_consumption[top_consumption$category == 'other', ]
  } else{
    top_consumption <- top_consumption[top_consumption$category == 'food', ]
  }
  top_consumption$category <- NULL
  # Take only top 20 consumers
  top_consumers <- top_consumption[1:num_countries_to_plot, "country"]
  top_consumption$consumption[!top_consumption$country %in% top_consumers] <- NA
  
  # Ensure country names match the map data
  top_consumption$country[top_consumption$country == 'China, mainland'] <- 'China'
  top_consumption$country[top_consumption$country == "Russian Federation"] <- "Russia"
  top_consumption$country[top_consumption$country == 'C\xf4te d\x92Ivoire'] <-  "Côte d'Ivoire"
  
  # Merge the consumption data with the world map data
  world <- world_blank %>%
    left_join(top_consumption, by = c("name" = "country"))
  
  # Get consumption per cap, just for fun
  world$consumption_per_cap <- world$consumption / world$pop_est
  #world <- world[order(-world$consumption_per_cap), ]
  
  # Plot the map
  cons_plot <- ggplot(data = world) +
    geom_sf(aes(fill = consumption), color = "darkgrey", size = 0.1) +
    scale_fill_distiller(palette = "Greens",
                         na.value = "grey",
                         direction = 1,
                         name = "Final Consumption (tonnes)") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6),
      panel.background = element_rect(fill = "white", color="white"),  # Set plot background to white
      plot.background = element_rect(fill = "white", color="white"),    # Set plot area background to white
      panel.grid.major = element_blank()                # Remove major gridlines
    ) +
    labs(title = paste("Top", num_countries_to_plot, "Consumers of",
                       items$item[items$comm_code == filter_commodity], "in", year), 
         #subtitle = "Production values for a specific commodity",
         #caption = "Source: FABIO"
         )
  
  ggsave(filename = paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\prod_cons_maps\\cons_maps\\top_cons_",
                           items$item[items$comm_code == filter_commodity], "_", year, ".png"),
         plot = cons_plot,
         width = 180,
         height = 80,
         unit = 'mm',
         dpi = 300)

}
  
'------------------------------------------------------------------------------'
################ GRAPHING PRODUCTION BY COUNTRY ###############################

# Select commodity to assess
#CommToPlot <- "c045" # Coffee
#CommToPlot <- "c046" # Chocolate Products
#CommToPlot <- "c047" # Tea
#CommToPlot <- "c059" # Tobacco

#Select x vector and year to assess for production
x <- x_stim
year <- '2020'
num_countries_to_plot <- 50

for (CommToPlot in c("c045", "c046", "c047", "c059")){
  # Subset the x vector for the specified area and item
  x_comm <- x[grep(CommToPlot, rownames(x)), ]
  rownames(x_comm) <- regions$area
  
  #TOP 5 Countries That produced commodity in 2021 in stimulant x vector
  x_comm_year <- data.frame(production = x_comm[, year])
  # Sort data by production
  x_comm_sorted <- x_comm_year[order(x_comm_year$production, decreasing = TRUE), , drop = FALSE]
  # Convert to Dataframe
  top_production <- data.frame(country=rownames(x_comm_sorted), production=x_comm_sorted)
  # Eliminate RoW
  top_production <- top_production[top_production$country != 'RoW', ]
  # Take only top 20 producers
  top_producers <- top_production[1:num_countries_to_plot, "country"]
  top_production$production[!top_production$country %in% top_producers] <- NA
  
  #Name Matching
  top_production$country[top_production$country == 'China, mainland'] <- 'China'
  top_production$country[top_production$country == "Russian Federation"] <- "Russia"
  top_production$country[top_production$country == 'C\xf4te d\x92Ivoire'] <-  "Côte d'Ivoire"
  
  # Merge the production data with the world map data
  world <- world_blank %>%
    left_join(top_production, by = c("name" = "country"))
  
  # Plot the map
  prod_plot <- ggplot(data = world) +
    geom_sf(aes(fill = production), color = "darkgrey", size = 0.1) +
    scale_fill_distiller(palette = "Greens",
                         na.value = "grey",
                         direction = 1,
                         name = "Production (tonnes)") +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 6),
      panel.background = element_rect(fill = "white", color="white"),  # Set plot background to white
      plot.background = element_rect(fill = "white", color="white"),    # Set plot area background to white
      panel.grid.major = element_blank()                # Remove major gridlines
    ) +
    labs(title = paste("Top", num_countries_to_plot, "Producers of",
                       items$item[items$comm_code == CommToPlot], "in", year),
         #subtitle = "Production values for a specific commodity",
         #caption = "Source: FABIO"
         )
  
  ggsave(filename = paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\prod_cons_maps\\prod_maps\\top_prod_",
                           items$item[items$comm_code == CommToPlot], "_", year, ".png"),
         plot = prod_plot,
         width = 180,
         height = 80,
         unit = 'mm',
         dpi = 300)
}


