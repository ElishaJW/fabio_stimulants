

# Load required libraries
library(dplyr)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

y_stim <- readRDS("./data/Y.rds")
regions <- fread("inst/regions_fabio.csv", )


for (region in regions$area_code){
  
  # Extract the final demand vector for Norway's food consumption
  y <- y_stim[['2020']][,paste0(region,"_food")]
  
  # Identify the number of countries in the dataset
  num_countries <- length(unique(regions$area_code)) # Assuming 'regions' contains area codes
  
  # Identify the number of commodities
  num_commodities <- length(y) / num_countries  # Assuming equal distribution
  
  # Generate country and commodity labels based on order
  countries <- rep(regions$area, each = num_commodities)
  commodities <- rep(unique(items$comm_code), times = num_countries)
  
  # Create Data Frame
  df <- data.frame(
    Country = countries,
    Commodity = commodities,
    Export_Value = y
  )
  
  # Convert country names to UTF-8 encoding
  df$Country <- enc2utf8(df$Country)
  #df_ranked$Country <- enc2utf8(df_ranked$Country)
  
  # Filter to only food commodities
  food_commodities <- unique(df$Commodity)  # Modify if you have a predefined list
  df_food <- df %>% filter(Commodity %in% food_commodities)
  
  # Rank exporters by commodity
  df_ranked <- df_food %>%
    group_by(Commodity) %>%
    arrange(desc(Export_Value), .by_group = TRUE) %>%
    mutate(Rank = row_number())
  
  # Display top exporters for each food commodity
  print(df_ranked)
  
  # Optionally, export results to CSV
  write.csv(
    df_ranked,
    paste0("./exporters_to_country_RAINFOREST/top_exporters_to_",
           regions$iso3c[regions$area_code == region],
           ".csv"),
    row.names = FALSE
  )  
}
