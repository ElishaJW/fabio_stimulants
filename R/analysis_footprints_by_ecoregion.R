
# PREP FOR VISUALIZING FOOTPRINT DATA
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# nolint start
#install.packages(c("sf", "raster", "ggplot2", "dplyr", "rgeos", "terra", "tmap"))

################################################################################

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

################################################################################

# Load the libraries------------------------------------------------------------
library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(terra)
library(tmap)
library(leaflet)
library(openxlsx)
library(viridis)

# Load FABIO inst data----------------------------------------------------------
items <- fread("inst/items_full.csv")
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" |
                    items$comm_group == "Tobacco, rubber" &
                    items$comm_code != 'c060'
                    ]$comm_code
regions <- fread("inst/regions_fabio.csv", )
EU <- regions$area[regions$continent == "EU"]
world <- regions$area[regions$area != "Belgium-Luxembourg" & regions$area != "Czechoslovakia"]

# Load shapefiles---------------------------------------------------------------
# COUNTRIES
countries_shp <- st_read("X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp")
#countries_EU28_shp <- st_read("X:/Eli/DATA/shapefiles/EU28/EU_countries.shp")

# ECOREGIONS
TEOW <- shapefile("X:/Eli/DATA/shapefiles/TEOW/wwf_terr_ecos.shp")
FEOW <- shapefile("X:/Eli/DATA/shapefiles/FEOW/FEOW.shp")

# BASINS
basins <- raster("X:/Eli/DATA/shapefiles/Basins/basins_5min_pcrglobwb_adjusted 1.tif")

# Load raster layers for the four commodities-----------------------------------
tea_tif <- raster("X:/Eli/PROJECTS/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TEA.tif")
tobac_tif <- raster("X:/Eli/PROJECTS/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TOBAC.tif")
coff_tif <- raster("X:/Eli/PROJECTS/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COFF.tif")
coco_tif <- raster("X:/Eli/PROJECTS/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COCO.tif")

# Load Characterization Factors-------------------------------------------------
landuse_cf <- fread("X:/Eli/DATA/cf/land use/CF.csv")
colnames(landuse_cf) <- toupper(colnames(landuse_cf))
water_cf <- read.xlsx("X:/Eli/DATA/cf/water consumption/water.xlsx", sheet="Basin_CF")
n_cf <- raster("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/N/ASCII_rasters/CF_average_direct.asc")
p_cf <- raster("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/P/ASCII_rasters/CF_average_direct.asc")

# Identify all stimulant commodities--------------------------------------------
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



################################################################################

# Function to identify top commodities and countries contributing to stimulant
# footprint of a given country and stressor
footprint_contributions <- function(country_list, footprint_data){
  # Identify country code
  country_code <- regions$area_code[regions$area %in% country_list]
  # Find country consumption category in matrix
  #country_str_fp <- footprint_data[, paste0(as.character(country_code),'_food')]
  
  # Sum country consumption categories in matrix
  # cols_to_sum <- grep(paste0("(^|[^0-9])", as.character(country_code), "([^0-9]|$)"),
  #                     colnames(footprint_data),
  #                     value = TRUE)
  
  cols_to_sum <- unlist(
    lapply(country_code, function(code) {
      grep(paste0("(^|[^0-9])", as.character(code), "([^0-9]|$)"),
           colnames(footprint_data),
           value = TRUE)
    })
  )
  
  #
  country_str_fp <- c(Sum = rowSums(footprint_data[, cols_to_sum]))
  
  # Get top indices of stimulant footprint
  top_indices <- order(country_str_fp, decreasing = TRUE)
  # Get the values themselves
  top_values <- country_str_fp[top_indices]
  # Convert positional index to to row name w country and commodity included
  top_indices <- rownames(footprint_data)[top_indices]
  # Remove country code to get just the commodity
  top_comms <- gsub("_.*", "", top_indices)
  # Same, but to get country
  top_countries <- gsub("*...._", "", top_indices)
  
  # Create a dataframe of all footprint information
  country_str_fp <- data.frame()
  for (i in seq_along(top_indices)){
    # Prints footprint information if we want:
    # print(paste(regions$area[regions$area_code == as.integer(top_countries[i])],
    #              ",",
    #              items$item[items$comm_code == top_comms[i]],
    #              ",",
    #              top_values[i], "hectares"))
    # Put footprint info into new dataframe
    country_str_fp[i,1] <- regions$area[regions$area_code == as.integer(top_countries[i])]
    country_str_fp[i,2] <- items$item[items$comm_code == top_comms[i]]
    country_str_fp[i,3] <- top_values[i]        
  }
  # Assign the top impacts to a new global env variable.
  colnames(country_str_fp) <- c('area', 'item', 'value')
  country_str_fp <- country_str_fp[country_str_fp$value != 0, ]
  assign("country_str_fp", country_str_fp, envir = .GlobalEnv)
}



###############################################################################

# Function to multiply country level footprint data with raster of production
# ratios on a 10x10 grid scale.
rasterize_country_level_footprint <- function(raster_layer, country, value, country_shapefile) {
  
  country_iso <- regions$iso3c[regions$area == country]
  # Need to cross check country names between FABIO list and shapefile
  if (country_iso %in% country_shapefile$ISO3) {
    # Filter the shapefile to get the specific country
    country_shape <- country_shapefile %>% filter(ISO3 == country_iso)
    # Crop the raster to the country's extent
    country_raster <- crop(raster_layer, extent(country_shape))
    # Mask the raster to the country's shape
    country_raster <- mask(country_raster, country_shape)
    # Multiply the relative contribution by the impact value
    impact_raster <- country_raster * value
    # Returns raster as variable
    return(impact_raster)
    # Otherwise, skip
  } else {
    print(paste(country, "in footprint data, but not in list of country shapefiles. Cannot match with production rasters."))
  }
}



################################################################################


# Function to create raster maps of stimulant footprints by stressor and country,
# then sum rasters to native scale (terrestrial ecoregion or basin)
stressor_footprint_country_to_ecoregion <- function(country_list,stressor,stim_item,year,save,figures){
  
  # Access footprint matrix for a given year------------------------------------
  str_fp <- readRDS(paste0("./data/str_footprint_", as.character(year),".rds")) 
  # Create indices in footprint matrix 
  str_fp <- str_fp[[stressor]]
  # Create unique list of combos
  combinations <- expand.grid(items$comm_code, regions$area_code)
  # Combine the country and commodity codes to create row names
  row_names <- paste(combinations$Var1, combinations$Var2, sep = "_")
  # Add row names back to matrix
  rownames(str_fp) <- row_names
  # Now filter for ONLY STIM COMMODITIES
  rows_to_keep <- grep(paste(items_stim, collapse = "|"), rownames(str_fp))
  # Subset the matrix to keep only the relevant rows
  str_fp_stim <- str_fp[rows_to_keep, , drop = FALSE]
  
  
  # Get stimulant footprint for top commodities---------------------------------
  print(paste0("Identifying ", stressor, " footprints of stimulants for ", country_list))
  # Calls function footprint_contributions created above
  footprint_contributions(country_list=country_list, footprint_data=str_fp_stim)
  # Change name of country list to world for saving data, if applicable
  #country_list <- "World"

  
  # DISAGGREGATING FOOTPRINTS TO 10X10 GRID SCALE FOR COFFEE COMMS--------------
  print(paste0("Disaggregating impacts to 10x10km grid scale"))
  # Do calculations based on stim_item selected. If coffee, do coffee.
  if (stim_item == 'coffee'){
    # Create empty list
    impact_stim_item_raster <- list()
    # Loop through items for the stim footprint calculated in previous section
    for (i in 1:nrow(country_str_fp)) {
      # Goes through footprint list, looking for stim_item. If yes, runs function above
      if (country_str_fp$item[i] %in% items_coff) {
        impact_raster <- rasterize_country_level_footprint(coff_tif, country_str_fp$area[i], country_str_fp$value[i], countries_shp)
        impact_stim_item_raster[[i]] <- impact_raster
      }
    }
    # Filter out any NULL or character elements from the list. Set 0 to NA
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.null)]
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.character)]
    
    # Mosaic all rasters of the same stim_item, summing their values where they overlap
    if (length(impact_stim_item_raster) > 1) {
      final_impact_raster_coff <- do.call(mosaic, c(impact_stim_item_raster, fun = sum))
      final_impact_raster_coff[final_impact_raster_coff == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_coff, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"/",country_list,"_",stressor,"_footprint_",year,"_coffee.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else if (length(impact_stim_item_raster) == 1){
      final_impact_raster_coff <- impact_stim_item_raster[[1]]
      final_impact_raster_coff[final_impact_raster_coff == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_coff,
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"/",country_list,"_",stressor,"_footprint_",year,"_coffee.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else {
      print("no impacts from coffee in top stimulant footprints")
    }
    
    
    # DIASAGGREGATING IMPACTS FOR COCOA COMMS-------------------------------------
  } else if (stim_item == 'cocoa'){
    # Create empty list
    impact_stim_item_raster <- list()
    # Loop through items for the stim footprint calculated in previous section
    for (i in 1:nrow(country_str_fp)) {
      # Goes through footprint list, looking for stim_item.
      # If in list of coco items, for example, runs function above
      if (country_str_fp$item[i] %in% items_coco) {
        impact_raster <- rasterize_country_level_footprint(coco_tif, country_str_fp$area[i], country_str_fp$value[i], countries_shp)
        impact_stim_item_raster[[i]] <- impact_raster
      }
    }
    # Filter out any NULL or character elements from the list. Set 0 to NA
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.null)]
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.character)]
    
    # Mosaic all rasters of the same stim_item, summing their values where they overlap
    if (length(impact_stim_item_raster) > 1) {
      final_impact_raster_coco <- do.call(mosaic, c(impact_stim_item_raster, fun = sum))
      final_impact_raster_coco[final_impact_raster_coco == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_coco, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_cocoa.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else if (length(impact_stim_item_raster) == 1){
      final_impact_raster_coco <- impact_stim_item_raster[[1]]
      final_impact_raster_coco[final_impact_raster_coco == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_coco, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_cocoa.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else {
      print("no impacts from cocoa in top stimulant footprints")
    }
    
    
    # DISAGGREGATING IMPACTS FOR TEA COMMS----------------------------------------
  } else if (stim_item == 'tea'){
    # Create empty list
    impact_stim_item_raster <- list()
    # Loop through items for the stim footprint calculated in previous section
    for (i in 1:nrow(country_str_fp)) {
      # Goes through footprint list, looking for stim_item. If yes, runs function above
      if (country_str_fp$item[i] %in% items_tea) {
        impact_raster <- rasterize_country_level_footprint(tea_tif, country_str_fp$area[i], country_str_fp$value[i], countries_shp)
        impact_stim_item_raster[[i]] <- impact_raster
      }
    }
    # Filter out any NULL or character elements from the list. Set 0 to NA
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.null)]
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.character)]
    
    # Mosaic all rasters of the same stim_item, summing their values where they overlap
    if (length(impact_stim_item_raster) > 1) {
      final_impact_raster_tea <- do.call(mosaic, c(impact_stim_item_raster, fun = sum))
      final_impact_raster_tea[final_impact_raster_tea == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_tea, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_tea.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else if (length(impact_stim_item_raster) == 1){
      final_impact_raster_tea <- impact_stim_item_raster[[1]]
      final_impact_raster_tea[final_impact_raster_tea == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_tea, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_tea.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else {
      print("no impacts from tea in top stimulant footprints")
    }
    
    
    # DISAGGREGATING IMPACTS FOR TOBACCO COMMS------------------------------------
  } else if (stim_item == 'tobacco'){
    # Create empty list
    impact_stim_item_raster <- list()
    # Loop through items for the stim footprint calculated in previous section
    for (i in 1:nrow(country_str_fp)) {
      # Goes through footprint list, looking for stim_item. If yes, runs function above
      if (country_str_fp$item[i] %in% items_tobac) {
        impact_raster <- rasterize_country_level_footprint(tobac_tif, country_str_fp$area[i], country_str_fp$value[i], countries_shp)
        impact_stim_item_raster[[i]] <- impact_raster
      }
    }
    # Filter out any NULL or character elements from the list. Set 0 to NA
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.null)]
    impact_stim_item_raster <- impact_stim_item_raster[!sapply(impact_stim_item_raster, is.character)]
    
    # Mosaic all rasters of the same stim_item, summing their values where they overlap
    if (length(impact_stim_item_raster) > 1) {
      final_impact_raster_tobac <- do.call(mosaic, c(impact_stim_item_raster, fun = sum))
      final_impact_raster_tobac[final_impact_raster_tobac == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_tobac, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_tobac.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
      
    } else if (length(impact_stim_item_raster) == 1){
      final_impact_raster_tobac <- impact_stim_item_raster[[1]]
      final_impact_raster_tobac[final_impact_raster_tobac == 0] <- NA
      if (save == TRUE) {
        writeRaster(final_impact_raster_tobac, 
                    filename = paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_rasters/",country_list,"_",stressor,"_footprint_",year,"_tobac.tif"),
                    format = "GTiff", 
                    overwrite = TRUE)}
    } else {
      print("no impacts from tobacco in top stimulant footprints")
    }
  } else {
    print("Commodity selected not in stimulant list")
  }
  
  
  # SELECT COMMODITY FOR RASTER EXTRACTION AND CF MULTIPLICATION--------------------
  # Probably an unnecessary step, but whatever
  if (stim_item == 'coffee'){
    final_impact_raster <- final_impact_raster_coff
  } else if (stim_item == 'cocoa'){
    final_impact_raster <- final_impact_raster_coco
  } else if (stim_item == 'tea'){
    final_impact_raster <- final_impact_raster_tea
  } else if (stim_item == 'tobacco'){
    final_impact_raster <- final_impact_raster_tobac
  } else {
    print("Commodity selected not in stimulant list")
  }
  assign("final_impact_raster", final_impact_raster, envir = .GlobalEnv)
  
  
  # FOR LANDUSE, CONVERT TO TERRESTRIAL ECOREGION LEVEL FROM 10x10 RASTER-----------
  if (stressor == 'landuse'){
    print(paste0("Summing impacts to ecoregion scale for chosen commodity"))
    # Extract raster values by ecoregion and sum them
    ecoregion_sums <- extract(final_impact_raster, TEOW, fun = sum, na.rm = TRUE)
    # The resulting dataframe will have the sums of raster values for each ecoregion
    TEOW$fp_sum <- ecoregion_sums
    # Better format
    TEOW_sf <- st_as_sf(TEOW)
    # Set NA to 0
    ## TEOW_sf$fp_sum[is.na(TEOW_sf$fp_sum)] <- 0
    TEOW_sf$fp_sum[TEOW_sf$fp_sum == 0] <- NA
    # Validate Geometries
    invalid_geometries <- TEOW_sf$ECO_NAME[!st_is_valid(TEOW_sf)]
    TEOW_sf <- TEOW_sf %>%
      mutate(geometry = st_make_valid(geometry))
    # Save as variable in environment
    assign("str_fp_by_ecoregion", TEOW_sf, envir = .GlobalEnv)
    
    # Save as shapefile
    if (save == TRUE) {
      st_write(TEOW_sf,
               paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_by_ecoregion/",country_list,"/",country_list,"_footprint_",stressor,"_",year,"_",stim_item,".shp"),
               delete_layer = TRUE)
    }
    
    # Graph on world map
    if (figures == TRUE) {
      pal <- colorNumeric(
        palette = c("#66c2a4", "#2ca25f", "#006d2c"),  # Green to blue gradient
        domain = TEOW_sf$fp_sum,
        na.color = "transparent"  # Transparent for NA values
      )
      
      leaflet(data = TEOW_sf) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # Use a simple grey background without reference data
        addPolygons(
          fillColor = ~ifelse(is.na(fp_sum) | fp_sum == 0, "transparent", pal(fp_sum)),  # Transparent for 0 & na values
          color = "lightgrey",  # Light grey border
          weight = 1,  # Border thickness
          opacity = 1,  # Border opacity
          fillOpacity = ~ifelse(fp_sum == 0, 0, 0.7),  # Transparent if 0, otherwise fill with opacity 0.7
          popup = ~paste("Ecoregion:", ECO_NAME, "<br>", "Summed Value:", fp_sum)  # Add popups for details
        ) %>%
        addLegend(
          pal = pal,#colorNumeric("YlOrRd", domain = TEOW_sf$fp_sum),
          values = TEOW_sf$fp_sum,
          title = "Summed Values",
          position = "bottomright"
        )
    }
    
    
    # FOR WATER USE, CONVERT TO BASIN LEVEL FROM 10x10 RASTER-----------------------
  } else if (stressor == 'blue') {
    print(paste0("Summing impacts to basin scale for chosen commodity"))
    
    # Resample MAPSPAM raster to basin raster resolution
    basin_sums <- resample(final_impact_raster, basins, method = "bilinear")
    # Zonal statistics: aggregate water use footprints by basin
    basin_ids <- unique(values(basins))
    basin_sums <- zonal(basin_sums, basins, fun = 'sum')
    # Convert to a polygon
    basin_polygons <- rasterToPolygons(basins, dissolve = TRUE)
    # Join water footprint data to the polygons
    basin_polygons@data <- left_join(basin_polygons@data,
                                     as.data.frame(basin_sums),
                                     by = c("basins_5min_pcrglobwb_adjusted.1" = "zone"))
    # Rename column pertaining to basin ID
    basin_polygons@data <- rename(basin_polygons@data,
                                  "ID" = "basins_5min_pcrglobwb_adjusted.1")
    # Save as variable in environment
    assign("str_fp_by_basin", basin_polygons, envir = .GlobalEnv)
    
    # Save as raster
    if (save == TRUE) {
      basin_fp_raster <- rasterize(basin_polygons, basins, field = "sum")
      writeRaster(basin_fp_raster,
                  paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_by_basin/",country_list,"/",country_list,"_footprint_",stressor,"_",year,"_",stim_item,".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
    # Graph on world map
    if (figures == TRUE) {
    }
    
    
    
    
  } else if (stressor == 'N') {
    print(paste0("Summing impacts to nitrogen raster scale for chosen commodity"))
    
    # Load n emissions raster (ensure correct file path)
    n_raster <- n_cf
    
    # Resample MAPSPAM raster to n raster resolution
    n_resampled <- resample(final_impact_raster, n_raster, method = "bilinear")
    
    # Aggregate impacts by n raster cells using zonal statistics
    n_ids <- unique(values(n_raster))
    n_sums <- zonal(n_resampled, n_raster, fun = 'sum')
    
    # Convert aggregated data into a usable format
    n_polygons <- rasterToPolygons(n_raster, dissolve = TRUE)
    n_polygons@data <- left_join(n_polygons@data,
                                        as.data.frame(n_sums),
                                        by = c("n_raster_ID" = "zone"))
    
    # Rename column pertaining to n IDs
    n_polygons@data <- rename(n_polygons@data, "ID" = "n_raster_ID")
    
    # Save as variable in environment
    assign("str_fp_by_n", n_polygons, envir = .GlobalEnv)
    
    # Save as raster (optional)
    if (save == TRUE) {
      n_fp_raster <- rasterize(n_polygons, n_raster, field = "sum")
      writeRaster(n_fp_raster,
                  paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_by_np_scale/", country_list, "_", stressor, "_", year, "_", stim_item, ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
    
  }else if (stressor == 'P') {
    print(paste0("Summing impacts to phosphorous raster scale for chosen commodity"))
    
    # Load n emissions raster (ensure correct file path)
    p_raster <- p_cf
    
    # Resample MAPSPAM raster to p raster resolution
    p_resampled <- resample(final_impact_raster, p_raster, method = "bilinear")
    
    # Aggregate impacts by p raster cells using zonal statistics
    p_ids <- unique(values(p_raster))
    p_sums <- zonal(p_resampled, p_raster, fun = 'sum')
    
    # Convert aggregated data into a usable format
    p_polygons <- rasterToPolygons(p_raster, dissolve = TRUE)
    p_polygons@data <- left_join(p_polygons@data,
                                 as.data.frame(p_sums),
                                 by = c("p_raster_ID" = "zone"))
    
    # Rename column pertaining to n IDs
    p_polygons@data <- rename(p_polygons@data, "ID" = "p_raster_ID")
    
    # Save as variable in environment
    assign("str_fp_by_p", p_polygons, envir = .GlobalEnv)
    
    # Save as raster (optional)
    if (save == TRUE) {
      p_fp_raster <- rasterize(p_polygons, p_raster, field = "sum")
      writeRaster(p_fp_raster,
                  paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_by_np_scale/", country_list, "_", stressor, "_", year, "_", stim_item, ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
  }
}



################################################################################

# Function to convert native scale footprints to native scale bd impacts using 
# cfs for land use, blue water use, n or p
stim_biodiversity_footprint <- function(run_all_functions,
                                        #str_fp_by_native_scale,
                                        stressor,
                                        country_list,
                                        stim_item,
                                        year,
                                        save,
                                        figures){
  
  if (run_all_functions == TRUE){
    stressor_footprint_country_to_ecoregion(country_list=country_list,
                                           stressor=stressor,
                                           stim_item=stim_item,
                                           year=year,
                                           save=TRUE,
                                           figures=figures)
  }
  
  #country_list <- "World"
  
  # Get stressor matrix
  str_fp_by_native_scale <- st_read(paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/stressor_footprint_by_ecoregion/",country_list,"/",country_list,"_footprint_",stressor,"_",year,"_",stim_item,".shp"))
  
  
  if (stressor == "landuse"){
    print(paste0("Multiplying commodity footprints with landuse CFs by ecoregion"))

    # Remove unnecessary data from footprint data by ecoregion, cleaning up too
    str_fp_by_ecoregion <- str_fp_by_native_scale %>%
      group_by(ECO_ID) %>%
      summarize(geometry = st_union(geometry), 
                fp_sum = sum(fp_sum, na.rm = TRUE))
    
    # Join fp data with CF data
    bd_fp <- str_fp_by_ecoregion %>%
      left_join(landuse_cf, by = "ECO_ID")
    
    # Remove lakes and ice sheets
    bd_fp <- bd_fp[bd_fp$ECO_ID >= 0, ]
    
    # Calculate global pdf for land occupation
    bd_fp <- bd_fp %>%
      mutate(pdf_occ_avg_glo = fp_sum * CF_OCC_AVG_GLO)

    # Drop unnecessary columns
    bd_fp <- bd_fp[ , !(colnames(bd_fp) %in% c(
      "CF_OCC_AVG_REG",
      "CF_OCC_AVG_GLO",
      "CF_OCC_MAR_REG",
      "CF_OCC_MAR_GLO",
      "CF_TRA_AVG_REG",
      "CF_TRA_AVG_GLO",
      "CF_TRA_MAR_REG",
      "CF_TRA_MAR_GLO",
      "QUALITY_REG",
      "QUALITY_GLO"))]
    
    # Filter rows where the 'HABITAT' column contains the string 'cropland'
    bd_fp <- bd_fp[grepl("Cropland", bd_fp$HABITAT), ]
    
    # Filter rows where the 'HABITAT' column contains the string 'intense'
    bd_fp <- bd_fp[grepl("Intense", bd_fp$HABITAT), ]
    
    # Ensure spatial data in bd_fp has valid geometries
    # print(TEOW_sf$ECO_NAME[!st_is_valid(TEOW_sf)])
    # bd_fp <- st_make_valid(bd_fp)
    # print(TEOW_sf$ECO_NAME[!st_is_valid(TEOW_sf)])
    
    # Now sum biodiversity impact across all species groups, grouping by ecoregion
    bd_fp_all_sp <- bd_fp %>%
      group_by(ECO_NAME, ECO_ID, geometry) %>%
      summarize(pdf_all_sp = mean(pdf_occ_avg_glo, na.rm = TRUE), .groups = 'drop')
    
    # Remove 0 values for visualizations
    bd_fp_all_sp <- bd_fp_all_sp[bd_fp_all_sp$pdf_all_sp > 0, ]
    
    if (figures == TRUE){
      pal <- colorNumeric(
        palette = inferno(5), # Get the 'inferno' colormap
        domain = bd_fp_all_sp$pdf_all_sp,
        na.color = "transparent"  # Transparent for NA values
      )
      
      leaflet(data = bd_fp_all_sp) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # Use a simple grey background without reference data
        addPolygons(
          fillColor = ~ifelse(is.na(pdf_all_sp) | pdf_all_sp == 0, "transparent", pal(pdf_all_sp)),  # Transparent for 0 & na values
          color = "lightgrey",  # Light grey border
          weight = 0.1,  # Border thickness
          opacity = 0.5,  # Border opacity
          fillOpacity = ~ifelse(pdf_all_sp == 0, 0, 0.7),  # Transparent if 0, otherwise fill with opacity 0.7
          #popup = ~paste("ECOREGION:", ECO_NAME, "<br>", "Summed PDF:", pdf_all_sp)  # Add popups for details
        ) %>%
        addLegend(
          pal = pal,
          values = bd_fp_all_sp$pdf_all_sp,
          title = "Summed Values",
          position = "bottomright"
        )
    }
    if (save == TRUE){
      st_write(bd_fp_all_sp,
               paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/",country_list,"/",country_list,"_bd_footprint_2020_",stim_item,".shp"),
               delete_layer = TRUE)
    }
    
  } else if (stressor == "blue"){
    # Join CFs to country_str_fp data at basin scale
    water_footprint <- merge(str_fp_by_native_scale@data, water_cf, by.x = "ID", by.y = "id_basin_pcrglob")
    # Multiply CFs with basin-level footprints
    water_footprint$bd_fp <- water_footprint$sum * as.numeric(water_footprint$CF_GLOB_A_m)
    # Convert back into raster
    bd_fp <- subs(basins, water_footprint, by = "ID", which = "bd_fp")
    # Replace NA values with 0
    bd_fp[is.na(bd_fp)] <- 0
    
    if (figures == TRUE){
    }
    
    if (save == TRUE){
      writeRaster(bd_fp,
                  paste0("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_basin/",country_list,"/",country_list,"_bd_footprint_2020_",stim_item,".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
    
  } else if (stressor == "N") {
    print(paste0("Multiplying commodity footprints with nitrogen CFs"))

    # Join CFs to nitrogen footprint data
    nitrogen_footprint <- merge(str_fp_by_native_scale@data, as.data.frame(n_cf), by.x = "ID", by.y = "nitrogen_raster_ID")
    
    # Multiply CFs with nitrogen-level footprints
    nitrogen_footprint$bd_fp <- nitrogen_footprint$sum * as.numeric(nitrogen_footprint$CF_value)
    
    # Convert back into raster
    bd_fp <- subs(nitrogen_raster, nitrogen_footprint, by = "ID", which = "bd_fp")
    
    # Replace NA values with 0
    bd_fp[is.na(bd_fp)] <- 0
    
    # Save biodiversity footprint raster
    if (save == TRUE) {
      writeRaster(bd_fp,
                  paste0("path/to/output/nitrogen_bd_fp_", country_list, "_", year, "_", stim_item, ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
    
  } else if (stressor == "P") {
    print(paste0("Multiplying commodity footprints with nitrogen CFs"))
    
    # Join CFs to nitrogen footprint data
    phosphorous_footprint <- merge(str_fp_by_native_scale@data, as.data.frame(p_cf), by.x = "ID", by.y = "phosphorous_raster_ID")
    
    # Multiply CFs with phosphorous-level footprints
    phosphorous_footprint$bd_fp <- phosphorous_footprint$sum * as.numeric(phosphorous_footprint$CF_value)
    
    # Convert back into raster
    bd_fp <- subs(phosphorous_raster, phosphorous_footprint, by = "ID", which = "bd_fp")
    
    # Replace NA values with 0
    bd_fp[is.na(bd_fp)] <- 0
    
    # Save biodiversity footprint raster
    if (save == TRUE) {
      writeRaster(bd_fp,
                  paste0("path/to/output/phosphorous_bd_fp_", country_list, "_", year, "_", stim_item, ".tif"),
                  format = "GTiff",
                  overwrite = TRUE)
    }
  }
}



################################################################################

# RUNNING TEST FOR... ----------------------------------------------------------

for (stim_item in c("coffee", "cocoa", "tea", "tobacco")){
  stressor_footprint_country_to_ecoregion(country_list="United States of America",
                                     stressor="landuse",
                                     stim_item=stim_item,
                                     year=2020,
                                     save=TRUE,
                                     figures=FALSE)
}


for (stim_item in c("coffee", "cocoa", "tea", "tobacco")){
  stim_biodiversity_footprint(run_all_functions=TRUE,
                              year=2020,
                              #str_fp_by_native_scale=str_fp_by_native_scale,
                              stressor="landuse",
                              country_list="United States of America",
                              stim_item=stim_item,
                              save=TRUE,
                              figures=FALSE)
}

################################################################################
# 
# # SUM SHAPEFILES FOR ALL STIMULANTS--------------------------------------------
biodiv_fp_world_coffee <- st_read("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/EU/EU_bd_footprint_2020_coffee.shp")
biodiv_fp_world_cocoa <- st_read("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/EU/EU_bd_footprint_2020_cocoa.shp")
biodiv_fp_world_tea <- st_read("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/EU/EU_bd_footprint_2020_tea.shp")
biodiv_fp_world_tobacco <- st_read("X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/EU/EU_bd_footprint_2020_tobacco.shp")

# Combine all shapefiles into one data frame
biodiv_fp_world <- bind_rows(biodiv_fp_world_coffee, biodiv_fp_world_cocoa, biodiv_fp_world_tea, biodiv_fp_world_tobacco)

# Group by ecoregion (ECO_ID) and sum the biodiversity impacts
# Using st_union to handle cases where multiple geometries for the same ecoregion exist
biodiv_fp_world <- biodiv_fp_world %>%
  group_by(ECO_ID, ECO_NAME) %>%
  summarize(
    geometry = st_union(geometry),
    pdf_all_sp = sum(pdf_all_sp, na.rm = TRUE),
    .groups = "drop"  # Optional, to ungroup after summarizing
  )

# Save the resulting shapefile with combined impacts
st_write(biodiv_fp_world,
         "X:/Eli/PROJECTS/fabio_stimulants/results_data/bd_footprint_by_ecoregion/EU/EU_bd_footprint_2020.shp",
         delete_layer = TRUE)
   
################################################################################























  
  