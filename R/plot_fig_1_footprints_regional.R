
# BIODIVERSITY FOOTPRINT CHORD DIAGRAMS — STIMULANT COMMODITIES
# V2.0 — uses E_bd_2020.rds (CFs already applied as per-tonne intensities),
#         skipping raw CF loading & multiplication done in analysis_footprints_regional_new.R.
#         Adds climate change (GHG) impacts (CH4, CO2, N2O) on top of land, water, N, P.
#
# Key change from old script:
#   Old: load str_footprint (physical) + load raw CFs + multiply → bd_fp per stressor
#   New: load E_bd (CFs already baked in as _bd rows) + compute L×Y once → bd_fp directly
#
#
# Eli Wilson - elisha.wilson@ntnu.no

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
library(scales)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')


# ── Load data ──────────────────────────────────────────────────────────────────
# E_bd has per-tonne biodiversity intensity rows (_bd suffix), CFs already applied.
# Path: data/E/E_bd_2020.rds  (change here if using a different file e.g. "E.rds")
eb      <- readRDS("data/E/E_bd_2020.rds")
items   <- fread("inst/items_full.csv")
regions <- fread("inst/regions_fabio.csv")
income_classifications <- read.xlsx(
  "X:/Eli/DATA/misc/Income_classifications_country.xlsx",
  sheet = "List of economies"
)

# NOTE: CH4_bd/CO2_bd label swap was fixed at the source in 17_biodiv_calculations.R.
# No correction needed here after regenerating E_bd files.

# ── Load IO tables ─────────────────────────────────────────────────────────────
year  <- 2020
Y_all <- readRDS("data/IO tables/Y.rds")
Y <- Y_all[[as.character(year)]]
L <- readRDS(paste0("data/IO tables/", year, "_L_mass.rds"))

# ── Compute L × Y ──────────────────────────────────────────────────────────────
# Ly[i, j] = total output of sector i needed to satisfy final demand j
cat("Computing L x Y for", year, "...\n")
Ly <- L %*% Y

# ── Build row names for the full 26112-row footprint matrix ───────────────────
# Row order matches E_bd columns: all combinations of comm_code × area_code
combinations <- expand.grid(items$comm_code, regions$area_code)
row_names    <- paste(combinations$Var1, combinations$Var2, sep = "_")

# ── Compute per-stressor biodiversity footprints ──────────────────────────────
# Each footprint matrix is 26112 rows (producer country-items) x 1344 cols (consumer country-demand).
# sweep(M, 1, v, "*") multiplies each row i of M by v[i].
cat("Computing stressor footprint matrices...\n")

Ly_mat <- as.matrix(Ly)

bd_fp_landuse <- sweep(Ly_mat, 1, as.numeric(eb["landuse_bd", ]), "*")
rownames(bd_fp_landuse) <- row_names

bd_fp_water <- sweep(Ly_mat, 1, as.numeric(eb["water_bd", ]), "*")
rownames(bd_fp_water) <- row_names

bd_fp_N <- sweep(Ly_mat, 1, as.numeric(eb["N_bd", ]), "*")
rownames(bd_fp_N) <- row_names

bd_fp_P <- sweep(Ly_mat, 1, as.numeric(eb["P_bd", ]), "*")
rownames(bd_fp_P) <- row_names

# Climate change: sum CH4, CO2, N2O biodiversity intensities
ghg_intensity <- as.numeric(eb["CH4_bd", ]) + as.numeric(eb["CO2_bd", ]) + as.numeric(eb["N2O_bd", ])
bd_fp_GHG <- sweep(Ly_mat, 1, ghg_intensity, "*")
rownames(bd_fp_GHG) <- row_names


################## SUM BD FOOTPRINT ###############################

# Replace NA with 0 before summing
bd_fp_landuse[is.na(bd_fp_landuse)] <- 0
bd_fp_water[is.na(bd_fp_water)]     <- 0
bd_fp_N[is.na(bd_fp_N)]             <- 0
bd_fp_P[is.na(bd_fp_P)]             <- 0
bd_fp_GHG[is.na(bd_fp_GHG)]         <- 0

bd_fp_fw          <- bd_fp_water +
                     bd_fp_N +
                     bd_fp_P

bd_fp_terrestrial <- bd_fp_landuse +
                     bd_fp_GHG

bd_fp_all         <- (bd_fp_fw + bd_fp_terrestrial) / 2


rownames(bd_fp_all)  <- row_names
colnames(bd_fp_all)  <- colnames(Ly)




# ── Stimulant commodity definitions ───────────────────────────────────────────
items_stim <- items[items$comm_group == "Coffee, tea, cocoa" |
                    items$comm_group == "Tobacco, rubber" & items$comm_code != "c060"]$comm_code

items_coffee  <- c("Coffee, green",
                   "Coffee, decaffeinated or roasted",
                   "Coffee extracts",
                   "Coffee substitutes")
items_tea     <- c("Tea leaves",
                   "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
                   "Mate leaves")
items_cocoa   <- c("Cocoa beans",
                   "Cocoa butter, fat and oil",
                   "Cocoa husks and shells",
                   "Cocoa paste not defatted",
                   "Cocoa powder and cake",
                   "Chocolate products nes")
items_tobacco <- c("Unmanufactured tobacco",
                   "Cigars and cheroots",
                   'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
                   "Cigarettes")

save <- TRUE


# ── Loop over country classification and commodity ─────────────────────────────
for (country_classification in c(1,2)) {

  if (country_classification == 1) {
    continent <- TRUE;  wb_income <- FALSE; wb_group <- FALSE
  } else if (country_classification == 2) {
    continent <- FALSE; wb_income <- TRUE;  wb_group <- FALSE
  } else if (country_classification == 3) {
    continent <- FALSE; wb_income <- FALSE; wb_group <- TRUE
  }

  for (stim_item in c("coffee", "cocoa", "tea", "tobacco")) {

    # Identify the rows of bd_fp_all belonging to this commodity group
    if (stim_item == "coffee") {
      rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_coffee],  collapse = "|"), row_names)
    } else if (stim_item == "cocoa") {
      rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_cocoa],   collapse = "|"), row_names)
    } else if (stim_item == "tea") {
      rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tea],     collapse = "|"), row_names)
    } else if (stim_item == "tobacco") {
      rows_to_keep <- grep(paste(items$comm_code[items$item %in% items_tobacco], collapse = "|"), row_names)
    }

    regions <- fread("inst/regions_fabio.csv")
    regions$continent[regions$continent == "LAM"] <- "SAM"
    regions$continent[regions$continent == "EU"]  <- "EUR"

    # Filter the full footprint matrix to this commodity group's rows only
    bd_fp_total <- bd_fp_all[rows_to_keep, , drop = FALSE]


    ############# RESHAPE DATA AND CHOOSE COUNTRY GROUPINGS ######################

    # Reshape and Aggregate flows by commodity and consumption category-----------
    # DOES NOT INCLUDE STOCK_ADDITIONS
    if (stim_item == "tobacco") { # all cons categories for tobacco, because it's not a food product
      aggregated_flows <- bd_fp_total %>%
        as.data.frame() %>%
        rownames_to_column(var = "Producer") %>%  # Convert row names to a column
        pivot_longer(
          cols = -Producer,
          names_to = c("Country", "Category"),
          names_sep = "_",  # Split column names at the underscore
          values_to = "value"
        ) %>%
        group_by(Producer, Country) %>%

        # All consumption categories for tobacco!
        summarise(value = sum(value), .groups = "drop") %>%

        # Extract commodity and producer country from the producer row names
        mutate(Commodity = sub("^(c[0-9]+)_([0-9]+)$", "\\1", Producer),
               Producer_Country = sub("^(c[0-9]+)_([0-9]+)$", "\\2", Producer)) %>%

        # Group by the producer country and consumer
        group_by(Producer_Country, Country) %>%
        summarise(flow = sum(value), .groups = "drop") %>%
        rename(producer = Producer_Country, consumer = Country) %>%

        # Remove RoW
        filter(!grepl("999", producer) & !grepl("999", consumer))
    } else {
      aggregated_flows <- bd_fp_total %>%
        as.data.frame() %>%
        rownames_to_column(var = "Producer") %>%  # Convert row names to a column
        pivot_longer(
          cols = -Producer,
          names_to = c("Country", "Category"),
          names_sep = "_",  # Split column names at the underscore
          values_to = "value"
        ) %>%
        group_by(Producer, Country) %>%

        # Just FOOD for other stim products
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
    }
    aggregated_flows$flow[is.na(aggregated_flows$flow)] <- 0  # Replace NA with 0

    # Area code as character
    regions$area_code <- as.character(regions$area_code)

    # Group by Continent----------------------------------------------------------
    if (continent == TRUE) {
      aggregated_flows <- aggregated_flows %>%
        rename(area_code = producer) %>%
        left_join(regions, by = "area_code") %>%
        group_by(continent, consumer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(producer = continent, area_code = consumer) %>%
        left_join(regions, by = "area_code") %>%
        group_by(continent, producer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(consumer = continent)

      # Remove the "_producer" and "_consumer" suffixes to unify country nodes
      aggregated_flows$producer <- gsub("_producer", "", aggregated_flows$producer)
      aggregated_flows$consumer <- gsub("_consumer", "", aggregated_flows$consumer)
    }

    # OR Group by Income Group----------------------------------------------------
    if (wb_income == TRUE) {
      aggregated_flows <- aggregated_flows %>%
        left_join(regions, by = c("producer" = "area_code")) %>%
        left_join(income_classifications, by = c("iso3c" = "Code")) %>%
        group_by(Income.group, consumer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(producer = Income.group) %>%
        left_join(regions, by = c("consumer" = "area_code")) %>%
        left_join(income_classifications, by = c("iso3c" = "Code")) %>%
        group_by(Income.group, producer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(consumer = Income.group) %>%
        # filter out some unmatched NA values
        filter(!is.na(producer) & !is.na(consumer))
    }

    # OR Group by WB country Group------------------------------------------------
    if (wb_group == TRUE) {
      aggregated_flows <- aggregated_flows %>%
        left_join(regions, by = c("producer" = "area_code")) %>%
        left_join(income_classifications, by = c("iso3c" = "Code")) %>%
        group_by(Region, consumer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(producer = Region) %>%
        left_join(regions, by = c("consumer" = "area_code")) %>%
        left_join(income_classifications, by = c("iso3c" = "Code")) %>%
        group_by(Region, producer) %>%
        summarise(flow = sum(flow), .groups = "drop") %>%
        rename(consumer = Region) %>%
        # filter out some unmatched NA values
        filter(!is.na(producer) & !is.na(consumer))
    }


    ########################### GRAPH SANKEYS ####################################

    # Graph Sankeys---------------------------------------------------------------
    if (continent == TRUE) {
      country_groups <- sort(unique(regions$continent[regions$continent != "ROW"]))
    } else if (wb_income == TRUE) {
      country_groups <- sort(unique(income_classifications$Income.group[1:218]))
    } else if (wb_group == TRUE) {
      country_groups <- sort(unique(income_classifications$Region[1:218]))
    }
    num_countries <- length(country_groups)

    # Change names of income groups to abbreviations
    if (wb_income == TRUE) {
      aggregated_flows <- aggregated_flows %>%
        mutate(producer = gsub(pattern = "High income",        replacement = "HI",  producer)) %>%
        mutate(consumer = gsub(pattern = "High income",        replacement = "HI",  consumer)) %>%
        mutate(producer = gsub(pattern = "Low income",         replacement = "LI",  producer)) %>%
        mutate(consumer = gsub(pattern = "Low income",         replacement = "LI",  consumer)) %>%
        mutate(producer = gsub(pattern = "Upper middle income", replacement = "UMI", producer)) %>%
        mutate(consumer = gsub(pattern = "Upper middle income", replacement = "UMI", consumer)) %>%
        mutate(producer = gsub(pattern = "Lower middle income", replacement = "LMI", producer)) %>%
        mutate(consumer = gsub(pattern = "Lower middle income", replacement = "LMI", consumer))
    }

    # Rename the producers and consumers with suffixes
    aggregated_flows$producer <- paste0(aggregated_flows$producer, "_producer")
    aggregated_flows$consumer <- paste0(aggregated_flows$consumer, "_consumer")

    # Remove producers with zero values from the flow data
    aggregated_flows <- aggregated_flows %>%
      filter(flow > 0)

    aggregated_flows_wide <- aggregated_flows %>%
      pivot_wider(names_from = consumer, values_from = flow, values_fill = 0) %>%
      mutate(producer = gsub("_(producer|consumer)$", "", producer)) %>%
      column_to_rownames(var = "producer") %>%
      rename_with(~ gsub("_(producer|consumer)$", " ", .x)) %>%
      as.matrix()

    # Set colors of flows and grids manually, based on group names
    if (continent == TRUE) {
      producer_colors <- c("AFR" = paletteer_d("ggthemes::excel_Organic")[1],
                           "ASI" = paletteer_d("ggthemes::excel_Organic")[2],
                           "EUR" = paletteer_d("ggthemes::excel_Organic")[3],
                           "NAM" = paletteer_d("ggthemes::excel_Organic")[4],
                           "OCE" = paletteer_d("ggthemes::excel_Organic")[5],
                           "SAM" = paletteer_d("ggthemes::excel_Organic")[6])
      consumer_colors <- c("AFR " = paletteer_d("ggthemes::excel_Organic")[1],
                           "ASI " = paletteer_d("ggthemes::excel_Organic")[2],
                           "EUR " = paletteer_d("ggthemes::excel_Organic")[3],
                           "NAM " = paletteer_d("ggthemes::excel_Organic")[4],
                           "OCE " = paletteer_d("ggthemes::excel_Organic")[5],
                           "SAM " = paletteer_d("ggthemes::excel_Organic")[6])

    } else if (wb_income == TRUE) {
      producer_colors <- c("LI"  = paletteer_c("grDevices::SunsetDark", 4)[4],
                           "LMI" = paletteer_c("grDevices::SunsetDark", 4)[3],
                           "UMI" = paletteer_c("grDevices::SunsetDark", 4)[2],
                           "HI"  = paletteer_c("grDevices::SunsetDark", 4)[1])
      consumer_colors <- c("LI "  = paletteer_c("grDevices::SunsetDark", 4)[4],
                           "LMI " = paletteer_c("grDevices::SunsetDark", 4)[3],
                           "UMI " = paletteer_c("grDevices::SunsetDark", 4)[2],
                           "HI "  = paletteer_c("grDevices::SunsetDark", 4)[1])

    } else if (wb_group == TRUE) {
      producer_colors <- c("Sub-Saharan Africa"       = paletteer_d("ggthemes::excel_Organic")[1],
                           "South Asia"               = paletteer_d("ggthemes::excel_Organic")[2],
                           "Europe & Central Asia"    = paletteer_d("ggthemes::excel_Organic")[3],
                           "North America"            = paletteer_d("ggthemes::excel_Organic")[4],
                           "East Asia & Pacific"      = paletteer_d("ggthemes::excel_Organic")[5],
                           "Latin America & Caribbean" = paletteer_d("ggthemes::excel_Organic")[6],
                           "Middle East & North Africa" = paletteer_d("ggthemes::excel_Organic")[7])
      consumer_colors <- c("Sub-Saharan Africa "       = paletteer_d("ggthemes::excel_Organic")[1],
                           "South Asia "               = paletteer_d("ggthemes::excel_Organic")[2],
                           "Europe & Central Asia "    = paletteer_d("ggthemes::excel_Organic")[3],
                           "North America "            = paletteer_d("ggthemes::excel_Organic")[4],
                           "East Asia & Pacific "      = paletteer_d("ggthemes::excel_Organic")[5],
                           "Latin America & Caribbean " = paletteer_d("ggthemes::excel_Organic")[6],
                           "Middle East & North Africa " = paletteer_d("ggthemes::excel_Organic")[7])
    }

    # Define order to place producers at top and consumers at bottom
    all_sectors <- c(rownames(aggregated_flows_wide), colnames(aggregated_flows_wide))

    # Safely compute total flow per sector
    row_totals <- rowSums(aggregated_flows_wide)
    col_totals <- colSums(aggregated_flows_wide)
    all_names  <- union(names(row_totals), names(col_totals))

    sector_total <- setNames(
      replace_na(row_totals[all_names], 0) + replace_na(col_totals[all_names], 0),
      all_names
    )

    if (save == TRUE) {
      if (continent == TRUE) {
        png(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\", stim_item, "\\chord_diagram_continents.png"),
            width = 2000, height = 2000, res = 300)
      } else if (wb_income == TRUE) {
        png(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\", stim_item, "\\chord_diagram_wb_incomes.png"),
            width = 2000, height = 2000, res = 300)
      } else if (wb_group == TRUE) {
        png(paste0("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\sankeys\\", stim_item, "\\chord_diagram_wb_regions.png"),
            width = 2000, height = 2000, res = 300)
      }
    }

    # Total flow across entire matrix
    total_flow <- sum(aggregated_flows_wide, na.rm = TRUE)

    # Percent share of total flow per sector
    sector_percent <- sector_total / total_flow * 100

    # Plot
    circos.clear()
    circos.par(canvas.xlim = c(-1.2, 1.2), canvas.ylim = c(-1.2, 1.2))
    chordDiagram(
      x              = aggregated_flows_wide,
      directional    = 1,
      direction.type = c("arrows"),
      link.arr.type  = "big.arrow",
      grid.col       = c(producer_colors, consumer_colors),
      order          = all_sectors,
      annotationTrack = c("grid"),
      transparency   = 0.3
    )

    # Add total value label
    text(
      x      = 0.9,
      y      = -1.2,
      labels = "",
      cex    = 0,
      col    = NA
    )

    # Add labels with spacing and filtering
    label_threshold_pct <- 1.3   # hide labels for sectors < 1% of total flow

    circos.trackPlotRegion(
      track.index = 1,
      panel.fun = function(x, y) {
        sector.name <- get.cell.meta.data("sector.index")
        xlim        <- get.cell.meta.data("xlim")
        ylim        <- get.cell.meta.data("ylim")
        total       <- sector_total[sector.name]
        percent     <- sector_percent[sector.name]

        if (!is.na(percent) && percent > label_threshold_pct) {
          # Sector name label
          circos.text(
            x    = mean(xlim),
            y    = ylim[2] + mm_y(10),
            labels = sector.name,
            niceFacing = TRUE,
            cex  = 1
          )

          # Axis ticks
          circos.axis(
            h                = "top",
            major.at         = NULL,
            labels           = FALSE,
            labels.cex       = 0.4,
            major.tick.length = mm_y(1.5),
            sector.index     = sector.name,
            track.index      = 1,
            lwd              = 0.8,
            col              = "grey10"
          )

          # Centered percentage label above arc
          circos.text(
            x    = mean(xlim),
            y    = ylim[2] + mm_y(4.5),
            labels = paste0(round(percent, 1), "%"),
            sector.index = sector.name,
            track.index  = 1,
            niceFacing   = TRUE,
            cex          = 0.8,
            col          = "grey30"
          )
        }
      },
      bg.border = NA
    )

    dev.off()
  }
}



