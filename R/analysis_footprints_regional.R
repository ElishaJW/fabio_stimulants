
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

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

# Load Data
year <- 2020
str_fp <- readRDS(paste0("./data/str_footprint_",year,".rds"))
stressor <- "landuse"
items <- fread("inst/items_full.csv")
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" | items$comm_group == "Tobacco, rubber" & items$comm_code != 'c060']$comm_code
regions <- fread("inst/regions_fabio.csv", )
landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF_country.csv")
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


################################################################################

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


################################################################################

# Create indices in footprint matrix 
str_fp <- str_fp[[stressor]]

# Create unique list of combos
combinations <- expand.grid(items$comm_code, regions$area_code)

# Combine the country and commodity codes to create row names
row_names <- paste(combinations$Var1, combinations$Var2, sep = "_")

# Add row names back to matrix
rownames(str_fp) <- row_names

# Save figures?
save <- FALSE

# Group by?
continent <- TRUE
wb_income <- FALSE
wb_group <- FALSE

#for (stim_item in c("coffee", "cocoa", "tea", "tobacco")){
for (stim_item in c("tobacco")){

  # Now filter for only stim commodities
  if (stim_item == "coffee"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_coffee], collapse = "|"), rownames(str_fp))
  } else if (stim_item == "cocoa"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_cocoa], collapse = "|"), rownames(str_fp))
  } else if (stim_item == "tea"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tea], collapse = "|"), rownames(str_fp))
  } else if (stim_item == "tobacco"){
    rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tobacco], collapse = "|"), rownames(str_fp))
  } else if (stim_item == "all"){
    rows_to_keep <- grep(paste(items$comm_code[items$comm_code %in% items_stim], collapse = "|"), rownames(str_fp))
  }
  
  # Subset the matrix to keep only the relevant rows
  str_fp_stim <- str_fp[rows_to_keep, , drop = FALSE]
  
  # Convert dgeMatrix to a data frame
  # FOR STIM COMMODITIES ONLY:
  str_fp_stim <- as.data.frame(as.matrix(str_fp_stim))
  
  # FOR ALL FABIO PRODUCTS:
  #str_fp_stim <- as.data.frame(as.matrix(str_fp))
  
  
  regions <- fread("inst/regions_fabio.csv", )
  regions$continent[regions$continent == "LAM"] <- "SAM"
  regions$continent[regions$continent == "EU"] <- "EUR"
  # regions$continent[regions$continent == "EUR"] <- "EUR (NON-EU)"
  
  
  ################################################################################
  
  # MULTIPLY FOOTPRINT MATRIX WITH CF---------------------------------------------
  bd_fp_stim_df <- str_fp_stim
  
  # Extract the country code from row names
  bd_fp_stim_df$area_code <- as.numeric(sub(".*_", "", rownames(bd_fp_stim_df)))
  
  # Extract commodity name as well
  bd_fp_stim_df$comm_code <- sub("_.*", "", rownames(bd_fp_stim_df))
  
  # Add column for corresponding iso3 code
  bd_fp_stim_df <- bd_fp_stim_df %>%
    left_join(regions[, c('area_code', 'iso3c')], by = "area_code")
  
  # Join the land use dataframe with the characterization factors dataframe by country code
  bd_fp_stim_df <- bd_fp_stim_df %>%
    left_join(landuse_cf, by = "iso3c")
  
  
  # Multiply the footprint values by the characterization factors-----------------
  # Multiply all columns except 'country_code', 'comm_code' and rownames contained in the CF
  cols_to_multiply <- colnames(bd_fp_stim_df)[!colnames(bd_fp_stim_df) %in% c("area_code", "comm_code", colnames(landuse_cf))]
  bd_fp_stim_df[cols_to_multiply] <- bd_fp_stim_df[cols_to_multiply] * bd_fp_stim_df$CF_occ_avg_glo
  
  # Rename rows to correspond to comm, country, species group
  rownames(bd_fp_stim_df) <- paste(bd_fp_stim_df$comm_code,
                                             bd_fp_stim_df$area_code,
                                             bd_fp_stim_df$species_group,
                                             sep = "_")
  
  # Drop unnecessary columns, create new bd dataframe
  bd_fp_stim_df <- bd_fp_stim_df[cols_to_multiply]
  
  
  # GROUPING THE LAND USE FOOTPRINT BY SPECIES GROUP - AVERAGE!-------------------
  # Create a new row corresponding to the commodity_country combinations
  comm_country <- sub("_[^_]+$", "", rownames(bd_fp_stim_df))
  bd_fp_stim_df$comm_country <- comm_country
  
  # Group by the product-country combination and calculate the mean across rows
  bd_fp_stim_df_grouped_by_sp <- bd_fp_stim_df %>%
    group_by(comm_country) %>%
    summarise(across(everything(), mean, .groups = 'drop'))
  
  # Create a factor to maintain original order and use it to sort the results
  bd_fp_stim_df_grouped_by_sp$comm_country <- factor(bd_fp_stim_df_grouped_by_sp$comm_country,
                                                              levels = unique(comm_country))
  
  # Arrange by the new factor levels to maintain the original order
  bd_fp_stim_df_grouped_by_sp <- bd_fp_stim_df_grouped_by_sp %>% arrange(comm_country)
  
  # Remove the comm_country column from the original and new matrices
  bd_fp_stim_df <- bd_fp_stim_df %>%
    dplyr::select(-comm_country)
  bd_fp_stim_df_grouped_by_sp <- bd_fp_stim_df_grouped_by_sp[-1]
  
  # Add product-country names back as rownames
  rownames(bd_fp_stim_df_grouped_by_sp) <- rownames(str_fp_stim)
  
  
  ################################################################################
  
  # Reshape and Aggregate flows by commodity and consumption category-------------
  # DOES NOT INCLUDE STOCK_ADDITIONS
  aggregated_flows <- bd_fp_stim_df_grouped_by_sp %>%
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
  
  
  ################################################################################
  
  # Graph Sankeys-----------------------------------------------------------------
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
  print(nodes$name)
  print(nodes$color)
  # print(JS(paste0("d3.scaleOrdinal().range([", paste0("'", node_colors, "'", collapse = ","), "])")))
  
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
############################ GGSANKEY! ####################################



# Reshape and Aggregate flows by commodity and consumption category-------------
# DOES NOT INCLUDE STOCK_ADDITIONS
aggregated_flows <- bd_fp_stim_df_grouped_by_sp %>%
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

