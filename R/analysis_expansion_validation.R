
# VISUALIZING MRIO DATA AND COMPARING WITH MASTER Z MATRIX
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# Load necessary libraries
library(tidyverse)
library("data.table")
library(dplyr)
library(ggplot2)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

source("R/01_tidy_functions.R")


# Set paths
fabio_master_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_master"
fabio_stimulant_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants"


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
#regions <- regions[regions$area_code != 999, ]
items <- read.csv(paste0(fabio_stimulant_path, "\\inst\\items_full.csv"))


## 116 Total commodities without livestock and tobacco ##


######################## TESTING ###########################
######## Asssessing differences between Z matrices #########

examine_z <- function(filter_country, filter_commodity, z, x){
  # Create list of items based on x vector row names
  items_x <- unique(rownames(x))
  # Find positional index of commodity selected
  comm_index <- which(items_x == filter_commodity)

  # Find positional index of country selected
  country_index <- which(regions$area == filter_country)
  # Index Z matrix accordingly
  z_index <- length(items_x) * (country_index - 1) + comm_index
  
  # Get values of the top 10 uses of commodity in z
  top_values <- sort(z[z_index, ], decreasing = TRUE)[1:10]
  # Get the positional index of the top 10 values in z
  top_indices <- order(z[z_index, ], decreasing = TRUE)[1:10]
  # Find the countries receiving the comm based on indices
  top_countries_name_index <- top_indices %/% length(items_x) + 1
  # Find the comms receiving the comm based on indices
  top_countries_comm_index <- top_indices %% length(items_x)
  
  for (i in seq_along(top_countries_comm_index)){
    combined <- paste(regions$area[top_countries_name_index[i]],",",
                      items$item[items$comm_code == items_x[top_countries_comm_index[i]]],
                      ",", top_values[i], "tonnes")
    print(combined)
  }
  print(paste("Total intermediate cons:", sum(z[z_index, ]), "tonnes"))
}


examine_z <- function(filter_commodity, z, x){
  # Create list of items based on x vector row names
  items_x <- unique(rownames(x))
  # Find positional index of commodity selected
  comm_index <- which(items_x == filter_commodity)

  # Sum all rows for the selected commodity to get total global distribution
  comm_indices <- which(rownames(z == filter_commodity))
  z_comm <- colSums(z[comm_indices, , drop = FALSE])
  
  # Get values of the top 10 uses of commodity in z
  top_values <- sort(z_comm, decreasing = TRUE)[1:10]
  # Get the positional index of the top 10 values in z
  top_indices <- order(z_comm, decreasing = TRUE)[1:10]
  # Find the countries receiving the comm based on indices
  top_countries_name_index <- top_indices %/% length(items_x) + 1
  # Find the comms receiving the comm based on indices
  top_countries_comm_index <- top_indices %% length(items_x)
  
  for (i in seq_along(top_countries_comm_index)){
    combined <- paste(regions$area[top_countries_name_index[i]],",",
                      items$item[items$comm_code == items_x[top_countries_comm_index[i]]],
                      ",", top_values[i], "tonnes")
    print(combined)
  }
  print(paste("Total intermediate cons:", sum(z_comm), "tonnes"))
}



# Select year and branch (stim, master) to assess
z_branch_year <- z_master[['2013']]
x_branch <- x_master
# Select commodity and country to assess
filter_commodity <- "c046"
filter_country <- "C\xf4te d\x92Ivoire"

# Execute!
examine_z(filter_commodity, z=z_branch_year, x=x_branch)


#################### DOING THE SAME FOR Y MATRIX ############################

examine_y <- function(filter_country, filter_commodity, y, x){
  # Create list of items based on x vector index
  items_x <- unique(rownames(x))
  # Find positional index of commodity selected
  comm_index <- which(items_x == filter_commodity)
  # Find positional index of country selected
  country_index <- which(regions$area == filter_country)
  # Index y matrix accordingly
  y_index <- length(items_x) * (country_index - 1) + comm_index
  # Get values of the top 10 uses of commodity in y
  top_values <- sort(y[y_index, ], decreasing = TRUE)[1:10]
  # Get the positional index of the top 10 values in y
  top_indices <- order(y[y_index, ], decreasing = TRUE)[1:10]
  # Convert index to to column name
  top_indices <- colnames(y)[top_indices]
  # Remove consumption category to get just the country
  top_countries <- gsub("_.*", "", top_indices)
  # Same, but to get consumption category
  top_countries_cons <- gsub("^[0-9]+_", "", top_indices)
  
  # Print the country, cons category, and value of the top 10 consumers
  for (i in seq_along(top_indices)){
    combined <- paste(regions$area[regions$area_code == as.integer(top_countries[i])],
                      ",",
                      top_countries_cons[i],
                      ",",
                      top_values[i], "tonnes")
    print(combined)
  }
  print(paste("Total final consumption:", sum(y[y_index, ]), "tonnes"))
}

# Select year and branch (stim, master) to assess
y_branch_year <- y_stim[['2020']]
x_branch <- x_stim

# Select commodity and country to assess
filter_commodity <- "c046"
filter_country <- "Norway"

# Execute!
examine_y(filter_country, filter_commodity, y=y_branch_year, x=x_branch)








# # Identify the number of countries in the dataset
# num_countries <- length(unique(regions$area_code)) # Assuming 'regions' contains area codes
# 
# # Identify the number of commodities
# num_commodities <- length(y_no_cons) / num_countries  # Assuming equal distribution
# 
# # Generate country and commodity labels based on order
# countries <- rep(regions$area, each = num_commodities)
# commodities <- rep(unique(items$comm_code), times = num_countries)
# 
# # Create Data Frame
# df <- data.frame(
#   Country = countries,
#   Commodity = commodities,
#   Export_Value = y_no_cons
# )
# 
# # Filter to only food commodities
# food_commodities <- unique(df$Commodity)  # Modify if you have a predefined list
# df_food <- df %>% filter(Commodity %in% food_commodities)
# 
# # Rank exporters by commodity
# df_ranked <- df_food %>%
#   group_by(Commodity) %>%
#   arrange(desc(Export_Value), .by_group = TRUE) %>%
#   mutate(Rank = row_number())
# 
# # Display top exporters for each food commodity
# print(df_ranked)
# 
# df_ranked$Country <- enc2utf8(df_ranked$Country)
# 
# write.csv(df_ranked, "top_exporters_to_Norway.csv", row.names = FALSE)
# 
# 








######### Assessing total cons.of a commodity by country ######################

total_cons_y <- function(filter_commodity, y){
  # Exclude RoW from analysis
  #y <- y[, !grepl("999", names(y))]
  # Filter rows of y for commodity then sum
  y_comm <- colSums(y[rownames(y) == filter_commodity, ])
  # Get the positional index of the top 10 values in y
  top_indices <- order(y_comm, decreasing = TRUE)[1:20]
  top_values <- y_comm[top_indices]
  # Convert positional index to to column name
  top_indices <- colnames(y)[top_indices]
  # Remove consumption category to get just the country
  top_countries <- gsub("_.*", "", top_indices)
  # Same, but to get consumption category
  top_countries_cons <- gsub("^[0-9]+_", "", top_indices)
  
  # Dataframe for the country, cons category, and value of the top 20 consumers
  combined <- data.frame(area = character(),
                         category = character(),
                         value = numeric(),
                         stringsAsFactors = FALSE)
  
  for (i in seq_along(top_indices)){
    combined[i,]$area <- regions$area[regions$area_code == as.integer(top_countries[i])]
    combined[i,]$category <- top_countries_cons[i]
    combined[i,]$value <- top_values[i]
    
  }
  # Convert value column to numeric
  combined$value <- as.numeric(combined$value)
  # Plot the data using ggplot2
  print(ggplot(combined, aes(x = reorder(area, -value), y = value, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Largest Consumers of", items$item[items$comm_code == filter_commodity]),
         x = "Country",
         y = "Consumption (tonnes)",
         fill = "Cons. category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  
  
  assign("combined", combined, envir = .GlobalEnv)
  #print(paste("Total final consumption:", sum(y_comm), "tonnes"))
}


# Select commodity and country to assess
filter_commodity <- "c045"
#filter_commodity <- "c131" # Coffee roasted 
y <- y_stim[['2020']]

# Execute!
total_cons_y(filter_commodity=filter_commodity, y=y)


########### Production over time for a single country ##################
########### Comparison between old and new data ########################


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
  
  combined_df[combined_df == 0] <- NA
  
  data_comparison_plot <- ggplot(combined_df, aes(x = year, group=1)) +
    geom_line(aes(y = stim, color = "Stimulants")) +
    geom_line(aes(y = master, color = "Master")) +
    geom_line(aes(y = supply, color = "Supply")) +
    labs(title = paste("Annual Production of", CommToPlotName, "in", CountryToPlot),
         x = "Year",
         y = "Production (tonnes)",
         #caption = "Source: FAOSTAT",
         color = "Legend") +
    scale_color_manual(values = c("Master" = "red", "Stimulants" = "blue", "Supply" = "darkgreen"),
                       labels = c("Master", "Stimulants", "Supply (SUA)")) +
    scale_x_discrete(breaks = combined_df$year[seq(1, length(combined_df$year), by = 2)]) +  # Display every other year   
    #theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 60, hjust = 1))
  
  print(data_comparison_plot)
  
  ggsave(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\data_comparison\\country_comm_data_comparison\\data_comparison_",CommToPlotName,"_",CountryToPlot,".png"),
         plot = data_comparison_plot,
         width = 180,
         height = 140,
         unit = 'mm',
         dpi = 300)
  
}


graph_stuff("Sri Lanka", "Tea leaves")


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
  x_comm_stim_2021 <- data.frame(production_2021 = x_comm_stim[, '2017'])
  # x_stim_sorted <- sort(x_comm_stim_2021, decreasing = TRUE)
  x_stim_sorted <- x_comm_stim_2021[order(x_comm_stim_2021$production_2021, decreasing = TRUE), , drop = FALSE]
  top_countries <- rownames(x_stim_sorted)[0:num_countries_to_plot]
  x_comm_stim <- x_comm_stim[top_countries, ]
  
  
  # Subset the x vector for the specified area and item
  x_comm_master <- x_master[grep(CommToPlot, rownames(x_master)), ]
  rownames(x_comm_master) <- regions$area
  x_comm_master <- x_comm_master[top_countries, ]
  
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
  
  #return(combined_df)
  
  # top_producers_plot <- ggplot(combined_df, aes(x = year, y = value, group = country, color = country)) +
  #   geom_line(size = 0.5) +
  #   labs(title = paste("Production Over Time for", CommToPlotName, "in Top 5 Regions"),
  #        x = "Year",
  #        y = "Production (tonnes)") +
  #   scale_color_manual(values = c("red",
  #                                 'red',
  #                                 'blue',
  #                                 'blue',
  #                                 'gold',
  #                                 'gold',
  #                                 'lightgreen',
  #                                 'lightgreen',
  #                                 'black',
  #                                 'black'
  #   )) +
  #   theme_minimal() +
  #   theme(legend.position = "right") +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #   scale_y_continuous(breaks = seq(0, 10000000, by = 500000)) +
  #   scale_x_discrete(breaks = combined_df$year[seq(1, length(combined_df$year), by = 2)]) +  # Display every other year    
  #   theme_minimal() +
  #   theme(axis.text.y = element_text(size = 6, angle = 0))
  
  
  top_producers_plot <- ggplot(combined_df, aes(x = year, y = value, group = country, color = country)) +
    geom_line(size = 0.5) +
    labs(title = paste("Production Over Time for", CommToPlotName, "in Top 5 Regions"),
         x = "Year",
         y = "Production (tonnes)") +
    scale_color_manual(values = c("red",
                                  "red",
                                  "blue",
                                  "blue",
                                  "gold",
                                  "gold",
                                  "lightgreen",
                                  "lightgreen",
                                  "black",
                                  "black")) +
    scale_x_discrete(breaks = levels(factor(combined_df$year))[seq(1, length(levels(factor(combined_df$year))), by = 2)]) +  
    scale_y_continuous(breaks = seq(0, 10000000, by = 500000)) +  
    #theme_minimal() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 60, hjust = 1),
          axis.text.y = element_text(size = 6, angle = 0))
  
  print(top_producers_plot)
  
  ggsave(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\data_comparison\\top_producers\\",CommToPlotName,".png"),
         plot = top_producers_plot,
         width = 180,
         height = 140,
         unit = 'mm',
         dpi = 300)
  
}
 

graph_top_countries(5, "Cocoa beans")






















