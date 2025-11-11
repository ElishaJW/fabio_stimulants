
# install.packages("webshot")
# install.packages("htmlwidgets")
# webshot::install_phantomjs()

# Load necessary libraries
library(dplyr)
library(networkD3)
library(ggplot2)
library(ggsankey)
library(RColorBrewer)
library(webshot)
library(htmlwidgets)
library(tidyverse)
library(data.table)
library(openxlsx)
library(paletteer)
library(circlize)
library(scales)  # for alpha()

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

# Load Data
year <- 2020
str_fp <- readRDS(paste0("./data/str_footprint_",year,".rds"))
items <- fread("inst/items_full.csv")
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" | items$comm_group == "Tobacco, rubber" & items$comm_code != 'c060']$comm_code
regions <- fread("inst/regions_fabio.csv", )
landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF_country.csv")
water_cf <- read.xlsx("X:/Eli/DATA/cf/water consumption/water.xlsx", sheet="Country_CF")
N_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/N/Country_CF_N.xlsx")
P_cf <- read.xlsx("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/P/Country_CF_P.xlsx")

income_classifications <-  read.xlsx("X:/Eli/DATA/misc/Income_classifications_country.xlsx", sheet = 'List of economies')

# Identify all stimulant commodities
items_coffee <- c("Coffee, green",
                "Coffee, decaffeinated or roasted",
                "Coffee extracts",
                "Coffee substitutes")
items_tea <- c("Tea leaves",
               "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
               "Mate leaves")
items_cocoa <- c("Cocoa beans",
                "Cocoa butter, fat and oil",
                "Cocoa husks and shells",
                "Cocoa paste not defatted",
                "Cocoa powder and cake",
                "Chocolate products nes")
items_tobacco <- c("Unmanufactured tobacco",
                 "Cigars and cheroots",
                 'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
                 "Cigarettes")



############################# PREPPING CF DATA #################################

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


# PROCESSING EUTROPHICATION CFs-------------------------------------------------
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
}



######################## COMMODITY/COUNTRY GROUP CHOICES #######################

# Create unique list of combos
combinations <- expand.grid(items$comm_code, regions$area_code)

# Combine the country and commodity codes to create row names
row_names <- paste(combinations$Var1, combinations$Var2, sep = "_")

# Save figures?
save <- FALSE

# Group by?
continent <- TRUE
wb_income <- FALSE
wb_group <- FALSE

for (stim_item in c("coffee", "cocoa", "tea", "tobacco")){
#for (stim_item in c("tobacco")){

  # Now filter for only stim commodities
  if (stim_item == "coffee"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_coffee], collapse = "|"), row_names)
  } else if (stim_item == "cocoa"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_cocoa], collapse = "|"), row_names)
  } else if (stim_item == "tea"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tea], collapse = "|"), row_names)
  } else if (stim_item == "tobacco"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tobacco], collapse = "|"), row_names)
  } else if (stim_item == "all"){
    rows_to_keep <- grep(paste(items$comm_code[items$comm_code %in% items_stim], collapse = "|"), row_names)
  }
  
  regions <- fread("inst/regions_fabio.csv", )
  regions$continent[regions$continent == "LAM"] <- "SAM"
  regions$continent[regions$continent == "EU"] <- "EUR"
  
  
  
  ############### LAND USE BD FOOTPRINT BY COUNTRY##############################

  # PROCESSING FOOTRPINT MATRIX-------------------------------------------------
  str_fp_landuse <- str_fp[["landuse"]]
  str_fp_landuse <- as.data.frame(as.matrix(str_fp_landuse))
  rownames(str_fp_landuse) <- row_names
  str_fp_landuse <- str_fp_landuse[rows_to_keep, , drop = FALSE]
  bd_fp_landuse <- str_fp_landuse
  
  
  # MULTIPLY FOOTPRINT MATRIX WITH CF-------------------------------------------
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
  
  # 
  # # GROUPING THE LAND USE FOOTPRINT BY SPECIES GROUP - AVERAGE!-----------------
  # # Create a new row corresponding to the commodity_country combinations
  # comm_country <- sub("_[^_]+$", "", rownames(bd_fp_landuse))
  # bd_fp_landuse$comm_country <- comm_country
  # 
  # # Group by the product-country combination and calculate the mean across rows
  # bd_fp_landuse_grouped_by_sp <- bd_fp_landuse %>%
  #   group_by(comm_country) %>%
  #   summarise(across(everything(), mean, .groups = 'drop'))
  # 
  # # Create a factor to maintain original order and use it to sort the results
  # bd_fp_landuse_grouped_by_sp$comm_country <- factor(bd_fp_landuse_grouped_by_sp$comm_country,
  #                                                             levels = unique(comm_country))
  # 
  # # Arrange by the new factor levels to maintain the original order
  # bd_fp_landuse_grouped_by_sp <- bd_fp_landuse_grouped_by_sp %>% arrange(comm_country)
  # 
  # # Remove the comm_country column from the original and new matrices
  # bd_fp_landuse <- bd_fp_landuse %>%
  #   dplyr::select(-comm_country)
  # bd_fp_landuse_grouped_by_sp <- bd_fp_landuse_grouped_by_sp[-1]
  # 

  
  ############### WATER USE BD FOOTPRINT BY COUNTRY ############################
  
  # PROCESSING FOOTRPINT MATRIX-------------------------------------------------
  str_fp_water <- str_fp[["blue"]]
  rownames(str_fp_water) <- row_names
  str_fp_water <- str_fp_water[rows_to_keep, , drop = FALSE]
  str_fp_water <- as.data.frame(as.matrix(str_fp_water)) 
  str_fp_water <- str_fp_water
  
  # MULTIPLY FOOTPRINT MATRIX WITH CF-------------------------------------------
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
  
  
  
  ############### N, P EMISSIONS BD FOOTPRINT BY COUNTRY #######################
  
  for (nutrient in c("N", "P")){
    
    # PROCESSING FOOTRPINT MATRIX-----------------------------------------------
    # Select footprint matrix
    if (nutrient == "N"){
      str_fp_nutrient <- str_fp[["n_application"]]
    } else if (nutrient == "P"){
      str_fp_nutrient <- str_fp[["p_application"]]
    }
    rownames(str_fp_nutrient) <- row_names
    str_fp_nutrient <- str_fp_nutrient[rows_to_keep, , drop = FALSE]
    str_fp_nutrient <- as.data.frame(as.matrix(str_fp_nutrient))
    str_fp_nutrient <- str_fp_nutrient
    
    # MULTIPLY FOOTPRINT MATRIX WITH CF-----------------------------------------
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
  
  
  
  ###################### PUT BD FOOTPRINTS TOGETHER ############################
  
  # SUM THE MATRICES TOGETHER---------------------------------------------------
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
  
  
  ############# RESHAPE DATA AND CHOOSE COUNTRY GROUPINGS ######################
  
  # Reshape and Aggregate flows by commodity and consumption category-----------
  # DOES NOT INCLUDE STOCK_ADDITIONS
  aggregated_flows <- bd_fp_total %>%
    rownames_to_column(var = "Producer") %>%  # Convert row names to a column
    pivot_longer(
      cols = -Producer, 
      names_to = c("Country", "Category"), 
      names_sep = "_",  # Split column names at the underscore
      values_to = "value"
    ) %>%
    group_by(Producer, Country) %>%
    # All consumption categories?
    # summarise(value = sum(value), .groups = "drop") %>%
    # OR just food?
    filter(Category == "food") %>%
    
    # Extract commodity and producer country from the producer row names
    mutate(Commodity = sub("^(c[0-9]+)_([0-9]+)$", "\\1", Producer),
           Producer_Country = sub("^(c[0-9]+)_([0-9]+)$", "\\2", Producer)) %>%
    
    # Group by the producer country and consumer
    group_by(Producer_Country, Country) %>%
    summarise(flow = sum(value), .groups = "drop") %>%
    rename(producer = Producer_Country, consumer = Country) %>%
    
    # Remove RoW
    filter(!grepl("999", producer) & !grepl("999", consumer))
  aggregated_flows$flow[is.na(aggregated_flows$flow)] <- 0  # Replace NA with 0
  
  # Area code as character
  regions$area_code <- as.character(regions$area_code)
  
  # Group by Continent----------------------------------------------------------
  if (continent == TRUE){
    aggregated_flows <- aggregated_flows %>%
      rename(area_code=producer) %>%
      left_join(regions, by = "area_code") %>%
      group_by(continent, consumer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(producer=continent, area_code=consumer) %>%
      left_join(regions, by = "area_code") %>%
      group_by(continent, producer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(consumer=continent)
  
    # Remove the "_producer" and "_consumer" suffixes to unify country nodes
    aggregated_flows$producer <- gsub("_producer", "", aggregated_flows$producer)
    aggregated_flows$consumer <- gsub("_consumer", "", aggregated_flows$consumer)
  }
  
  # OR Group by Income Group----------------------------------------------------
  if (wb_income == TRUE){
    aggregated_flows <- aggregated_flows %>%
      left_join(regions, by = c("producer" = "area_code")) %>%
      left_join(income_classifications, by = c("iso3c" = "Code")) %>%
      group_by(Income.group, consumer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(producer=Income.group) %>%
      left_join(regions, by = c("consumer" = "area_code")) %>%
      left_join(income_classifications, by = c("iso3c" = "Code")) %>%
      group_by(Income.group, producer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(consumer=Income.group) %>%
      # filter out some unmatched NA values
      filter(!is.na(producer) & !is.na(consumer))
  }
  
  # OR Group by WB country Group------------------------------------------------
  if (wb_group == TRUE){
    aggregated_flows <- aggregated_flows %>%
      left_join(regions, by = c("producer" = "area_code")) %>%
      left_join(income_classifications, by = c("iso3c" = "Code")) %>%
      group_by(Region, consumer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(producer=Region) %>%
      left_join(regions, by = c("consumer" = "area_code")) %>%
      left_join(income_classifications, by = c("iso3c" = "Code")) %>%
      group_by(Region, producer) %>%
      summarise(flow = sum(flow), .groups = "drop") %>%
      rename(consumer=Region) %>%
      # filter out some unmatched NA values
      filter(!is.na(producer) & !is.na(consumer))
  }
  
  
  
  ########################### GRAPH SANKEYS ####################################
  
  # Graph Sankeys---------------------------------------------------------------
  # Create Unique color palette for sankey
  #country_names <- sort(unique(c(aggregated_flows$producer, aggregated_flows$consumer)))
  if (continent == TRUE){
    country_groups <- sort(unique(regions$continent[regions$continent != "ROW"]))
  } else if (wb_income == TRUE){
    country_groups <- sort(unique(income_classifications$Income.group[1:218]))
  } else if (wb_group == TRUE){
    country_groups <- sort(unique(income_classifications$Region[1:218]))
  }
  num_countries <- length(country_groups)
  
  # Create a mapping of country names to colors
  node_colors <- paletteer_d("ggthemes::excel_Organic")[0:num_countries]
  node_colors <- node_colors[order(node_colors)]  # Sort colors
  node_colors <- data.frame(color = node_colors, country_groups)
  
  # Rename the producers and consumers with suffixes
  aggregated_flows$producer <- paste0(aggregated_flows$producer, "_producer")
  aggregated_flows$consumer <- paste0(aggregated_flows$consumer, "_consumer")
  
  # Remove producers with zero values from the flow data
  aggregated_flows <- aggregated_flows %>%
    filter(flow > 0)
  
  # Create nodes with the renamed producer and consumer labels
  nodes <- data.frame(name = unique(c(aggregated_flows$producer, aggregated_flows$consumer)))
  
  # Assign colors to nodes based on the original country name (before suffix)
  # Extract the base country names from the nodes (remove "_producer" and "_consumer")
  nodes$base_name <- gsub("_(producer|consumer)$", "", nodes$name)
  nodes <- nodes[order(nodes$base_name), ]  # Sort node names
  nodes <- left_join(nodes, node_colors, by=c("base_name"="country_groups"))
  # print(nodes)
  
  # Create the links data frame, mapping the source and target to the new producer/consumer nodes
  links <- aggregated_flows %>%
    mutate(
      source = match(producer, nodes$name) - 1,  # Get the index for the producer nodes
      target = match(consumer, nodes$name) - 1   # Get the index for the consumer nodes
    ) %>%
    dplyr::select(source, target, flow)
  links$source_base_name <- nodes$base_name[match(aggregated_flows$producer, nodes$name)]
  links <- left_join(links, node_colors, by=c("source_base_name"="country_groups"))
  #links$color <- nodes$color[match(aggregated_flows$producer, nodes$name)]
  links <- as.data.frame(links)
  
  # # Identify producers with zero total flow
  # zero_flow_producers <- aggregated_flows %>%
  #   group_by(producer) %>%
  #   summarise(total_flow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  #   filter(total_flow == 0) %>%
  #   pull(producer)  # Extract the list of producers with zero flow
  # 
  # # Filter out rows in nodes corresponding to zero-flow producers
  # nodes <- nodes %>%
  #   filter(!name %in% zero_flow_producers)
  # 
  # # Same for links df
  # links <- links %>%
  #   filter(flow > 0)
  
  # print(links)
  # print(nodes$name)
  # print(nodes$color)
  # print(JS(paste0("d3.scaleOrdinal().range([", paste0("'", node_colors, "'", collapse = ","), "])")))
  print(stim_item)
  
  # Create the Sankey diagram with the new producer/consumer labels and color consistency
  sankey <- sankeyNetwork(
    Links = links,
    Nodes = nodes,
    Source = "source",
    Target = "target",
    Value = "flow",
    NodeID = "base_name",
    #units = "Flow Units", 
    fontSize = 20, 
    nodeWidth = 50, 
    #colourScale = JS(paste0("d3.scaleOrdinal().range([", paste0("'", node_colors, "'", collapse = ","), "])")),  # Node color scale
    colourScale = "d3.scaleOrdinal() .domain(['AFR_producer', 'ASI_producer','EUR_producer', 'NAM_producer', 'OCE_producer', 'SAM_producer']) .range(['#3C9770FF','#44709DFF','#83992AFF','#A23C33FF','#D97828FF','#DEB340FF'])",
    NodeGroup = "color",
    LinkGroup = "color"  # Use the source country color for the flows
  )
  
  print(sankey)
}
  
################################################################################

if (save ==TRUE) {
  
  if (continent == TRUE){
    # Have to first save as html
    saveWidget(sankey, paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_continents.html"),
               selfcontained = TRUE)
    # Save as PNG
    webshot(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\", stim_item, "\\sankey_continents.html"),
            file = paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\", stim_item, "\\sankey_continents.png"),
            vwidth = 1063, vheight = 1063, zoom = 2, delay = 2)
  } else if (wb_income == TRUE){
    # Have to first save as html
    saveWidget(sankey, paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_incomes.html"),
               selfcontained = TRUE)
    # Save as PNG
    webshot(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_incomes.html"),
            file = paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_incomes.png"),
            vwidth = 1063, vheight = 1063, zoom = 2, delay = 2)
  } else if (wb_group == TRUE){
    # Have to first save as html
    saveWidget(sankey, paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_wb_regions.html"),
               selfcontained = TRUE)
    # Save as PNG
    webshot(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_wb_regions.html"),
            file = paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\",stim_item,"\\sankey_wb_regions.png"),
            vwidth = 1063, vheight = 1063, zoom = 2, delay = 2)
  }
}


###########################################################################
############################ Chord! ####################################


aggregated_flows_wide <- aggregated_flows %>%
  pivot_wider(names_from = consumer, values_from = flow, values_fill = 0) %>%
  mutate(producer = gsub("_(producer|consumer)$", "", producer)) %>%
  column_to_rownames(var = "producer") %>%
  rename_with(~ gsub("_(producer|consumer)$", " ", .x)) %>%
  as.matrix()

# Set colors of flows
# producer_colors <- c("AFR" = "blue", "ASI" = "green", "EUR" = "red", "NAM" = "yellow", "OCE" = "orange", "SAM" = "purple")
# producer_colors <- setNames(paletteer_d("ggthemes::excel_Organic")[0:length(colnames(aggregated_flows_wide))], colnames(aggregated_flows_wide))
producer_colors <- setNames(rep(paletteer_d("ggthemes::excel_Organic")[0:length(country_groups)], length(country_groups)), country_groups)

# Define order to place producers at top and consumers at bottom
all_sectors <- c(rownames(aggregated_flows_wide), colnames(aggregated_flows_wide))  # order matters

# Safely compute total flow per sector
row_totals <- rowSums(aggregated_flows_wide)
col_totals <- colSums(aggregated_flows_wide)
all_names <- union(names(row_totals), names(col_totals))

sector_total <- setNames(
  replace_na(row_totals[all_names], 0) + replace_na(col_totals[all_names], 0),
  all_names
)

# Plot
circos.clear()
chordDiagram(
  x = aggregated_flows_wide,
  directional = 1,
  direction.type = c("arrows"),
  link.arr.type = "big.arrow",
  grid.col = c(producer_colors, setNames(rep("grey80", length(colnames(aggregated_flows_wide))), colnames(aggregated_flows_wide))),
  order = all_sectors,
  annotationTrack = "grid",
  transparency = 0.3
)

# Add central total value label
text(
  x = 0,
  y = -0.3,
  labels = paste0(format(sum(aggregated_flows_wide, na.rm = TRUE), digits = 3, scientific = FALSE), " PDF"),
  cex = 2,                     # font size
  font = 2,                    # bold
  col = alpha("black", 0.25)    # semi-transparent black
)

# Add labels with spacing and filtering
label_threshold <- 0.00001

circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    total <- sector_total[sector.name]
    if (!is.na(total) && total > label_threshold) {
      circos.text(
        x = mean(get.cell.meta.data("xlim")),
        y = get.cell.meta.data("ylim")[1] + mm_y(8),
        labels = sector.name,
        facing = "bending.inside",
        cex = 0.8,
        niceFacing = TRUE
      )
    }
  },
  bg.border = NA
)






###########################################################################
############################ GGSANKEY! ####################################

# Reshape and Aggregate flows by commodity and consumption category-------------
# DOES NOT INCLUDE STOCK_ADDITIONS
aggregated_flows <- bd_fp_landuse %>%
  rownames_to_column(var = "Producer") %>%  # Convert row names to a column
  pivot_longer(
    cols = -Producer, 
    names_to = c("Country", "Category"), 
    names_sep = "_",  # Split column names at the underscore
    values_to = "value"
  ) %>%
  group_by(Producer, Country) %>%
  summarise(Total_Flow = sum(value), .groups = "drop") %>%
  
  # Extract commodity and producer country from the producer row names
  mutate(Commodity = sub("^(c[0-9]+)_([0-9]+)$", "\\1", Producer),  # Extract commodity part
         Producer_Country = sub("^(c[0-9]+)_([0-9]+)$", "\\2", Producer)) %>%
  
  # Group by the producer country and consumer
  group_by(Producer_Country, Country) %>%
  summarise(Total_Flow = sum(Total_Flow), .groups = "drop") %>%
  rename(producer = Producer_Country, consumer = Country, flow = Total_Flow) %>%
  
  # Remove RoW
  filter(!grepl("999", producer) & !grepl("999", consumer))
aggregated_flows$flow[is.na(aggregated_flows$flow)] <- 0  # Replace NA with 0

# Area code as character
regions$area_code <- as.character(regions$area_code)

# Group by Continent----------------------------------------------------------
if (continent == TRUE){
  aggregated_flows <- aggregated_flows %>%
    rename(area_code=producer) %>%
    left_join(regions, by = "area_code") %>%
    group_by(continent, consumer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(producer=continent, area_code=consumer) %>%
    left_join(regions, by = "area_code") %>%
    group_by(continent, producer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(consumer=continent)
  
  # Remove the "_producer" and "_consumer" suffixes to unify country nodes
  aggregated_flows$producer <- gsub("_producer", "", aggregated_flows$producer)
  aggregated_flows$consumer <- gsub("_consumer", "", aggregated_flows$consumer)
}

# OR Group by Income Group----------------------------------------------------
if (wb_income == TRUE){
  aggregated_flows <- aggregated_flows %>%
    left_join(regions, by = c("producer" = "area_code")) %>%
    left_join(income_classifications, by = c("iso3c" = "Code")) %>%
    group_by(Income.group, consumer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(producer=Income.group) %>%
    left_join(regions, by = c("consumer" = "area_code")) %>%
    left_join(income_classifications, by = c("iso3c" = "Code")) %>%
    group_by(Income.group, producer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(consumer=Income.group) %>%
    # filter out some unmatched NA values
    filter(!is.na(producer) & !is.na(consumer))
}

# OR Group by WB country Group------------------------------------------------
if (wb_group == TRUE){
  aggregated_flows <- aggregated_flows %>%
    left_join(regions, by = c("producer" = "area_code")) %>%
    left_join(income_classifications, by = c("iso3c" = "Code")) %>%
    group_by(Region, consumer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(producer=Region) %>%
    left_join(regions, by = c("consumer" = "area_code")) %>%
    left_join(income_classifications, by = c("iso3c" = "Code")) %>%
    group_by(Region, producer) %>%
    summarise(flow = sum(flow), .groups = "drop") %>%
    rename(consumer=Region) %>%
    # filter out some unmatched NA values
    filter(!is.na(producer) & !is.na(consumer))
}

# Create Unique color palette for Sankey
if (continent == TRUE){
  country_groups <- sort(unique(regions$continent[regions$continent != "ROW"]))
} else if (wb_income == TRUE){
  country_groups <- sort(unique(income_classifications$Income.group[1:218]))
} else if (wb_group == TRUE){
  country_groups <- sort(unique(income_classifications$Region[1:218]))
}
num_countries <- length(country_groups)

# Create a mapping of country names to colors
node_colors <- paletteer_d("ggthemes::excel_Organic")[1:num_countries]
node_colors <- node_colors[order(node_colors)]  # Sort colors
node_colors <- data.frame(color = node_colors, country_groups)

# Rename the producers and consumers with suffixes
aggregated_flows$producer <- paste0(aggregated_flows$producer, "_producer")
aggregated_flows$consumer <- paste0(aggregated_flows$consumer, "_consumer")

# Remove producers with zero values from the flow data
aggregated_flows <- aggregated_flows %>%
  filter(flow > 0)

# Create nodes with the renamed producer and consumer labels
nodes <- data.frame(name = unique(c(aggregated_flows$producer, aggregated_flows$consumer)))

# Assign colors to nodes based on the original country name (before suffix)
nodes$base_name <- gsub("_(producer|consumer)$", "", nodes$name)
nodes <- nodes[order(nodes$base_name), ]  # Sort node names
nodes <- left_join(nodes, node_colors, by=c("base_name"="country_groups"))

# Create the links data frame, mapping the source and target to the new producer/consumer nodes
links <- aggregated_flows %>%
  mutate(
    source = match(producer, nodes$name) - 1,  # Get the index for the producer nodes
    target = match(consumer, nodes$name) - 1   # Get the index for the consumer nodes
  ) %>%
  dplyr::select(source, target, flow)
links$source_base_name <- nodes$base_name[match(aggregated_flows$producer, nodes$name)]
links <- left_join(links, node_colors, by=c("source_base_name"="country_groups"))

# Prepare data for ggsankey (long format)
sankey_data <- links %>%
  dplyr::mutate(
    node = factor(ifelse(source == target, source, NA), levels = unique(nodes$name)),
    x = as.numeric(source),
    next_x = as.numeric(target),
    flow = as.numeric(flow)
  )

# Create the Sankey diagram using ggsankey
ggplot(data = sankey_data, aes(x = x, xend = next_x, y = 0, yend = flow, fill = source_base_name)) +
  geom_sankey() +
  scale_fill_manual(values = setNames(nodes$color, nodes$base_name)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Sankey Diagram")

