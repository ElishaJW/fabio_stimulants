
# BIODIVERSITY FOOTPRINT BY ECOREGION / BASIN / GRID CELL
# V3.0 — Single-function rewrite
# Eli Wilson - elisha.wilson@ntnu.no
#
# Usage:
#   result <- bd_footprint("coffee", "United States of America", 2020)
#
# The function runs all four stressors end-to-end, combines them into a
# single realm-averaged biodiversity footprint map, and returns all outputs
# as a named list. No multi-step calling order, no global-env side effects.
#
# Stressors:
#   landuse       → BD by terrestrial ecoregion (TEOW)  [PDF·yr/ecoregion]
#   blue          → BD by drainage basin                 [PDF·yr/basin]
#   n_application → BD raster (N eutrophication)        [PDF·yr/cell]
#   p_application → BD raster (P eutrophication)        [PDF·yr/cell]
#
# Combination method (realm average):
#   Both realms present : combined_bd = (landuse_bd + fw_total) / 2
#   Freshwater only     : combined_bd = fw_total
#   Landuse only        : combined_bd = landuse_bd
#   where fw_total = water_bd + n_bd_basin + p_bd_basin

# nolint start

################################################################################
# SETUP ------------------------------------------------------------------------
################################################################################

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

library(tidyverse)
library(data.table)
library(sf)
library(raster)
library(terra)
library(leaflet)
library(openxlsx)
library(viridis)

# FABIO lookup tables
items   <- fread("inst/items_full.csv")
regions <- fread("inst/regions_fabio.csv")
EU      <- regions$area[regions$continent == "EU"]
world   <- regions$area[regions$area != "Belgium-Luxembourg" & regions$area != "Czechoslovakia"]

items_stim <- items[comm_group == "Coffee, tea, cocoa" |
                    (comm_group == "Tobacco, rubber" & comm_code != "c060")]$comm_code

# Shapefiles
countries_shp <- st_read("X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp")
TEOW          <- shapefile("X:/Eli/DATA/shapefiles/TEOW/wwf_terr_ecos.shp")
#TEOW          <- st_read("X:/Eli/DATA/shapefiles/TEOW/wwf_terr_ecos.shp")
basins_sf     <- st_read("X:/Eli/data/cf/water consumption/1-s2.0-S0048969722058016-mmc3/basins_5min_pcrglobwb.gpkg",
                         layer = "basins2")

# Production rasters (10×10 km, normalized by country)
prod_rasters <- list(
  coffee  = raster("X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COFF.tif"),
  cocoa   = raster("X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_COCO.tif"),
  tea     = raster("X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TEA.tif"),
  tobacco = raster("X:/Eli/projects/fabio_stimulants/results_data/rel_production_by_country_rasters/rel_prod_normalized_TOBAC.tif")
)

# Commodity item lists (FABIO item names per stimulant crop)
item_groups <- list(
  coffee  = c("Coffee, green", "Coffee, decaffeinated or roasted",
               "Coffee extracts", "Coffee substitutes"),
  cocoa   = c("Cocoa beans", "Cocoa butter, fat and oil", "Cocoa husks and shells",
               "Cocoa paste not defatted", "Cocoa powder and cake", "Chocolate products nes"),
  tea     = c("Tea leaves",
               "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
               "Mate leaves"),
  tobacco = c("Unmanufactured tobacco", "Cigars and cheroots",
               'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
               "Cigarettes")
)

# Characterization factors
landuse_cf <- fread("X:/Eli/data/cf/land use/CF_domain.csv")
colnames(landuse_cf) <- toupper(colnames(landuse_cf))

water_cf <- read.xlsx("X:/Eli/DATA/cf/water consumption/water.xlsx", sheet = "Basin_CF")
water_cf$CF_GLOB_A_m       <- as.numeric(water_cf$CF_GLOB_A_m)
water_cf$id_basin_pcrglob  <- as.integer(water_cf$id_basin_pcrglob)

# ── WATER CF OUTLIER CORRECTION ──────────────────────────────────────────────
# Basin 21611 (Rift Valley, Kenya) has CF_GLOB_A_m = 6.23e-6, ~8 orders of
# magnitude above its geographic neighbours (~E-14 range). This is almost
# certainly a data artefact from a small/unrepresentative endemic species sample
# in the source CF dataset. Replace with the mean CF of all spatially adjacent
# basins (same East African montane context).
cat("Correcting outlier water CF for basin 21611...\n")
b21611_row <- which(as.integer(basins_sf$id_basin_pcrglob) == 21611L)
if (length(b21611_row) > 0) {
  old_s2_corr    <- sf_use_s2(); sf_use_s2(FALSE)
  neighbour_rows <- st_touches(basins_sf[b21611_row, ], basins_sf)[[1]]
  sf_use_s2(old_s2_corr)
  neighbour_ids  <- as.integer(basins_sf$id_basin_pcrglob[neighbour_rows])
  neighbour_cfs  <- water_cf$CF_GLOB_A_m[water_cf$id_basin_pcrglob %in% neighbour_ids]
  neighbour_cfs  <- neighbour_cfs[!is.na(neighbour_cfs)]
  replacement_cf <- if (length(neighbour_cfs) > 0) mean(neighbour_cfs) else 0
  orig_cf        <- water_cf$CF_GLOB_A_m[water_cf$id_basin_pcrglob == 21611L]
  water_cf$CF_GLOB_A_m[water_cf$id_basin_pcrglob == 21611L] <- replacement_cf
  cat(sprintf("  Basin 21611 CF: %.4e  ->  %.4e  (mean of %d neighbours)\n",
              orig_cf, replacement_cf, length(neighbour_cfs)))
} else {
  cat("  Basin 21611 not found in basins_sf — skipping correction.\n")
}

n_cf <- raster("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/N/ASCII_rasters/CF_average_diffuse.asc")
p_cf <- raster("X:/Eli/DATA/cf/freshwater eutrophication/CFs_freshwater_eutrophication/global_species_loss/P/ASCII_rasters/CF_average_diffuse.asc")
if (is.na(crs(n_cf))) crs(n_cf) <- CRS("+proj=longlat +datum=WGS84")
if (is.na(crs(p_cf))) crs(p_cf) <- CRS("+proj=longlat +datum=WGS84")


################################################################################
# HELPER 1: get_country_footprints --------------------------------------------
# Extract country-level stressor footprint values for the consuming country/
# region from the FABIO stressor matrix. Returns a data.frame with columns
# area, item, value — sorted descending by value, zeros removed.
################################################################################

get_country_footprints <- function(country_list, str_fp_matrix) {
  country_codes <- regions$area_code[regions$area %in% country_list]

  # Find columns belonging to the consuming country/region
  col_idx <- unlist(lapply(country_codes, function(code) {
    grep(paste0("(^|[^0-9])", code, "([^0-9]|$)"), colnames(str_fp_matrix))
  }))

  row_sums <- rowSums(str_fp_matrix[, col_idx, drop = FALSE])

  # Attach row names: commodity_country format
  combinations <- expand.grid(items$comm_code, regions$area_code)
  rownames(str_fp_matrix) <- paste(combinations$Var1, combinations$Var2, sep = "_")

  # Filter to stimulant rows
  stim_rows  <- grep(paste(items_stim, collapse = "|"), rownames(str_fp_matrix))
  row_sums   <- rowSums(str_fp_matrix[stim_rows, col_idx, drop = FALSE])
  row_names  <- rownames(str_fp_matrix)[stim_rows]

  ord        <- order(row_sums, decreasing = TRUE)
  comm_codes <- gsub("_.*", "", row_names[ord])
  reg_codes  <- gsub(".*_", "", row_names[ord])

  contrib <- data.frame(
    area  = regions$area[match(as.integer(reg_codes), regions$area_code)],
    item  = items$item[match(comm_codes, items$comm_code)],
    value = row_sums[ord],
    stringsAsFactors = FALSE
  )
  contrib[!is.na(contrib$value) & contrib$value != 0, ]
}


################################################################################
# HELPER 2: disaggregate_to_raster --------------------------------------------
# Multiply the production raster for `country` by `value` (country-level
# stressor footprint) to spatially distribute the footprint. Returns a
# RasterLayer cropped and masked to the country boundary, or NULL if the
# country is not in the shapefile.
################################################################################

disaggregate_to_raster <- function(prod_raster, country, value, countries_shp) {
  iso3 <- regions$iso3c[regions$area == country]
  if (length(iso3) == 0 || !iso3 %in% countries_shp$ISO3) {
    message("  Skipping ", country, ": not found in country shapefile.")
    return(NULL)
  }
  shp     <- countries_shp[countries_shp$ISO3 == iso3, ]
  cropped <- crop(prod_raster, extent(shp))
  masked  <- mask(cropped, shp)
  masked * value
}


################################################################################
# HELPER 3: build_fp_raster ---------------------------------------------------
# For a given stressor and contributing country data.frame, disaggregate the
# country-level footprints to a 10×10 km production raster and mosaic all
# country tiles into a single global raster.
################################################################################

build_fp_raster <- function(contrib, stim_item) {
  prod_r     <- prod_rasters[[stim_item]]
  item_list  <- item_groups[[stim_item]]
  if (is.null(prod_r)) stop("Unknown stim_item: ", stim_item)

  tiles <- lapply(seq_len(nrow(contrib)), function(i) {
    if (!contrib$item[i] %in% item_list) return(NULL)
    disaggregate_to_raster(prod_r, contrib$area[i], contrib$value[i], countries_shp)
  })
  tiles <- Filter(Negate(is.null), tiles)

  if (length(tiles) == 0) {
    warning("No valid country rasters for stim_item=", stim_item, ". Returning NULL.")
    return(NULL)
  }
  r <- if (length(tiles) == 1) tiles[[1]] else do.call(mosaic, c(tiles, fun = sum))
  r[r == 0] <- NA
  r
}


################################################################################
# HELPER 4: make_leaflet_pal --------------------------------------------------
# Build a colorNumeric palette for a numeric vector, filtering out NAs so the
# domain is always finite. Uses inferno(100) for a smooth, perceptually uniform
# colour ramp.
################################################################################

make_pal <- function(x) {
  colorNumeric(palette = inferno(100), domain = x[!is.na(x)], na.color = "transparent")
}


################################################################################
# MAIN FUNCTION: bd_footprint -------------------------------------------------
#
# Arguments:
#   stim_item    : "coffee", "cocoa", "tea", or "tobacco"
#   country_list : character vector of consuming country/region names
#                  (use EU, world, or a custom vector)
#   year         : integer, e.g. 2020
#   figures      : logical — print leaflet maps for each stressor + combined?
#   save         : logical — write output files to X:/Eli/... directories?
#
# Returns a named list:
#   $landuse   sf with col landuse_bd  (PDF·yr per ecoregion)
#   $water     sf with col water_bd    (PDF·yr per basin)
#   $n_raster  RasterLayer             (PDF·yr per cell, N eutrophication)
#   $p_raster  RasterLayer             (PDF·yr per cell, P eutrophication)
#   $combined  sf with col combined_bd (PDF·yr, realm-averaged)
################################################################################

bd_footprint <- function(stim_item, country_list, year,
                         figures = TRUE, save = FALSE,
                         combine_scale = "ecoregion") {  # "basin" or "ecoregion"

  # Consumer key: short label used in all output file/folder names.
  # paste(country_list, collapse="_") fails when country_list is the full world
  # vector (~190 entries) — the joined string is thousands of chars and exceeds
  # OS path/filename length limits.
  ckey <- if (setequal(country_list, world)) {
    "world"
  } else if (setequal(country_list, EU)) {
    "EU"
  } else if (length(country_list) > 5) {
    paste0(length(country_list), "_countries")
  } else {
    paste(country_list, collapse = "_")
  }

  cat("\n======================================================\n")
  cat("  BD footprint:", stim_item, "|", ckey, "|", year, "\n")
  cat("======================================================\n\n")

  stressors <- c("landuse", "blue", "n_application", "p_application")

  # STEP 1: Load FABIO stressor matrices ----------------------------------------
  cat("[1/6] Loading FABIO stressor footprint matrices...\n")
  str_fp_all <- readRDS(paste0("./data/stressor footprints/str_footprint_", year, ".rds"))

  # Build rownames (comm_code_area_code) for all matrices
  combinations <- expand.grid(items$comm_code, regions$area_code)
  rn           <- paste(combinations$Var1, combinations$Var2, sep = "_")
  for (s in stressors) rownames(str_fp_all[[s]]) <- rn


  # STEPS 2–4: Per-stressor disaggregation + CF multiplication ------------------
  cat("[2/6] Processing each stressor...\n")

  # ── LANDUSE ──────────────────────────────────────────────────────────────────
  cat("  [landuse] Identifying contributing countries...\n")
  contrib_lu   <- get_country_footprints(country_list, str_fp_all[["landuse"]])

  cat("  [landuse] Building production footprint raster...\n")
  fp_lu        <- build_fp_raster(contrib_lu, stim_item)

  cat("  [landuse] Aggregating to ecoregion scale and applying CF...\n")
  eco_sums     <- extract(fp_lu, TEOW, fun = sum, na.rm = TRUE)
  TEOW$fp_sum  <- eco_sums
  TEOW_sf      <- st_as_sf(TEOW)
  TEOW_sf$fp_sum[TEOW_sf$fp_sum == 0] <- NA
  TEOW_sf      <- st_make_valid(TEOW_sf)

  bd_lu <- TEOW_sf %>%
    group_by(ECO_ID) %>%
    summarize(geometry = st_union(geometry), fp_sum = sum(fp_sum, na.rm = TRUE)) %>%
    left_join(landuse_cf, by = "ECO_ID") %>%
    filter(ECO_ID >= 0) %>%
    # Select the intense cropland CF for each ecoregion.
    # CF_domain.csv may use different HABITAT naming conventions for intense cropland:
    #   "Cropland, Intense use"        (Chaudhary 2015 format)
    #   "Annual crops, Intense use"    (alternate format)
    #   "Permanent crops, Intense use" (perennial crops — coffee, cocoa, tea, tobacco)
    # Accept any row matching "Intense" + any cropland-like term; if multiple rows
    # survive per ECO_ID (e.g., annual + permanent), keep the one with the highest CF
    # (conservative, worst-case assignment — avoids silently dropping ecoregions).
    filter(grepl("ntense", HABITAT, ignore.case = TRUE) &
           grepl("Crop|Annual|Permanent", HABITAT, ignore.case = TRUE)) %>%
    group_by(ECO_ID) %>%
    slice_max(CF_OCC_AVG_GLO, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(landuse_bd = fp_sum * CF_OCC_AVG_GLO) %>%
    dplyr::select(-any_of(c("CF_OCC_AVG_REG", "CF_OCC_AVG_GLO",
                     "CF_OCC_MAR_REG", "CF_OCC_MAR_GLO",
                     "CF_TRA_AVG_REG", "CF_TRA_AVG_GLO",
                     "CF_TRA_MAR_REG", "CF_TRA_MAR_GLO",
                     "QUALITY_REG",    "QUALITY_GLO"))) %>%
    filter(!is.na(landuse_bd) & landuse_bd > 0)

  # Sanitize ECO_NAME encoding: TEOW .shp attributes use Latin-1/Windows-1252 on
  # Windows; leaflet's HTML serializer calls gsub() expecting strict UTF-8 and
  # crashes on raw non-ASCII bytes. iconv() converts in-place; un-convertible bytes
  # are replaced with their hex representation (sub="byte") rather than silently dropped.
  bd_lu$ECO_NAME <- iconv(as.character(bd_lu$ECO_NAME), from = "", to = "UTF-8", sub = "byte")

  cat("  [landuse] Ecoregions with impact:", nrow(bd_lu), "\n")

  if (save) {
    # Use .gpkg not .shp — .shp truncates/mangles column names (10-char .dbf limit
    # means "landuse_bd" and "ECO_NAME" come back with garbled names when re-read).
    # GPKG (SQLite) preserves column names exactly, same as bd_water output.
    out_lu <- paste0("X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_by_ecoregion/",
                     ckey, "/", ckey, "_bd_footprint_", year, "_", stim_item, ".gpkg")
    dir.create(dirname(out_lu), recursive = TRUE, showWarnings = FALSE)
    st_write(bd_lu, out_lu, delete_layer = TRUE)
  }


  # ── BLUE WATER ───────────────────────────────────────────────────────────────
  cat("  [blue] Identifying contributing countries...\n")
  contrib_blue <- get_country_footprints(country_list, str_fp_all[["blue"]])

  cat("  [blue] Building production footprint raster...\n")
  fp_blue      <- build_fp_raster(contrib_blue, stim_item)

  if (is.null(fp_blue)) {
    cat("  [blue] No blue water data for", stim_item, "— skipping.\n")
    # Create a zero-row sf with the correct columns so downstream left_join
    # produces water_bd = NA for all basins (treated as 0 in fw_total sum).
    bd_water <- st_sf(
      id_basin_pcrglob = integer(0),
      water_bd         = numeric(0),
      geometry         = st_sfc(crs = st_crs(basins_sf))
    )
  } else {
    cat("  [blue] Aggregating to basin scale and applying CF...\n")
    basin_sums   <- terra::extract(rast(fp_blue), vect(basins_sf), fun = "sum", na.rm = TRUE)
    basins_fp    <- basins_sf
    basins_fp$fp_sum              <- basin_sums[, 2]
    basins_fp$fp_sum[is.na(basins_fp$fp_sum) | basins_fp$fp_sum == 0] <- NA
    basins_fp$id_basin_pcrglob    <- as.integer(basins_fp$id_basin_pcrglob)
    bd_water <- basins_fp %>%
      left_join(water_cf, by = "id_basin_pcrglob") %>%
      mutate(water_bd = fp_sum * CF_GLOB_A_m) %>%
      filter(!is.na(water_bd) & water_bd > 0)
  }

  cat("  [blue] Basins with impact:", nrow(bd_water), "\n")

  if (save && nrow(bd_water) > 0) {
    out_water <- paste0("X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_by_basin/",
                        ckey, "/", ckey, "_bd_footprint_", year, "_", stim_item, ".gpkg")
    dir.create(dirname(out_water), recursive = TRUE, showWarnings = FALSE)
    st_write(bd_water, out_water, delete_layer = TRUE)
  }


  # ── NITROGEN ─────────────────────────────────────────────────────────────────
  cat("  [N] Identifying contributing countries...\n")
  contrib_n    <- get_country_footprints(country_list, str_fp_all[["n_application"]])

  cat("  [N] Building production footprint raster and applying CF...\n")
  fp_n         <- build_fp_raster(contrib_n, stim_item)

  if (is.null(fp_n)) {
    cat("  [N] No N application data for", stim_item, "— skipping.\n")
    n_bd_raster <- NULL
  } else {
    # Disaggregate N CF to production raster resolution (nearest-neighbour):
    # each production pixel inherits the CF of the CF-pixel it falls within.
    # Then multiply pixel-wise so eutrophication impact is confined to pixels
    # where production actually occurs — identical logic to the water workflow.
    # This prevents coarse-pixel aggregation from bleeding N impacts into
    # ecoregions with no crop production.
    # Using raster::resample (method="ngb") avoids terra in-memory conversion
    # issues that cause "[*] raster has no values" errors.
    n_cf_disagg  <- raster::resample(n_cf, fp_n, method = "ngb")
    n_bd_raster  <- fp_n * n_cf_disagg
    n_bd_raster[n_bd_raster <= 0] <- NA
    if (is.na(crs(n_bd_raster))) crs(n_bd_raster) <- CRS("+proj=longlat +datum=WGS84")
  }

  if (save && !is.null(n_bd_raster)) {
    writeRaster(n_bd_raster,
                paste0("X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_by_np_scale/",
                       ckey, "_bd_footprint_n_", year, "_", stim_item, ".tif"),
                format = "GTiff", overwrite = TRUE)
  }


  # ── PHOSPHORUS ───────────────────────────────────────────────────────────────
  cat("  [P] Identifying contributing countries...\n")
  contrib_p    <- get_country_footprints(country_list, str_fp_all[["p_application"]])

  cat("  [P] Building production footprint raster and applying CF...\n")
  fp_p         <- build_fp_raster(contrib_p, stim_item)

  if (is.null(fp_p)) {
    cat("  [P] No P application data for", stim_item, "— skipping.\n")
    p_bd_raster <- NULL
  } else {
    # Same disaggregation approach as for N: resample CF to production resolution
    # using nearest-neighbour, then multiply pixel-wise.
    p_cf_disagg  <- raster::resample(p_cf, fp_p, method = "ngb")
    p_bd_raster  <- fp_p * p_cf_disagg
    p_bd_raster[p_bd_raster <= 0] <- NA
    if (is.na(crs(p_bd_raster))) crs(p_bd_raster) <- CRS("+proj=longlat +datum=WGS84")
  }

  if (save && !is.null(p_bd_raster)) {
    writeRaster(p_bd_raster,
                paste0("X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_by_np_scale/",
                       ckey, "_bd_footprint_p_", year, "_", stim_item, ".tif"),
                format = "GTiff", overwrite = TRUE)
  }


  # STEP 5: Combine stressors (realm average) -----------------------------------
  cat("[5/6] Combining stressors into single biodiversity impact map...\n")

  # Switch to GEOS flat-geometry engine for this step.
  # s2 (spherical geometry, sf's default) enforces strict validity rules that
  # reject minor self-intersections common in global polygon datasets. GEOS is
  # more permissive and matches the behaviour of older sf versions.
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2), add = TRUE)

  # Aggregate N and P rasters to basin scale (guard against NULL if stressor absent)
  basins_sv  <- vect(basins_sf)

  # Build basin-level freshwater totals
  basins_fw               <- basins_sf
  basins_fw$id_basin_pcrglob <- as.integer(basins_fw$id_basin_pcrglob)
  basins_fw$n_bd <- if (!is.null(n_bd_raster)) {
    terra::extract(rast(n_bd_raster), basins_sv, fun = "sum", na.rm = TRUE)[, 2]
  } else {
    rep(0, nrow(basins_sf))
  }
  basins_fw$p_bd <- if (!is.null(p_bd_raster)) {
    terra::extract(rast(p_bd_raster), basins_sv, fun = "sum", na.rm = TRUE)[, 2]
  } else {
    rep(0, nrow(basins_sf))
  }
  basins_fw <- left_join(
    basins_fw,
    st_drop_geometry(bd_water[, c("id_basin_pcrglob", "water_bd")]),
    by = "id_basin_pcrglob"
  )
  basins_fw$fw_total <- rowSums(cbind(
    ifelse(is.na(basins_fw$water_bd), 0, basins_fw$water_bd),
    ifelse(is.na(basins_fw$n_bd),     0, basins_fw$n_bd),
    ifelse(is.na(basins_fw$p_bd),     0, basins_fw$p_bd)
  ))
  basins_fw <- basins_fw[basins_fw$fw_total > 0, ]
  basins_fw <- st_make_valid(basins_fw)
  # Normalize geometry column name: GPKG files commonly use "geom" not "geometry".
  # Any explicit column selection that includes "geometry" will fail if the actual
  # column has a different name (sf falls through to [.data.frame → undefined columns).
  geom_col <- attr(basins_fw, "sf_column")
  if (geom_col != "geometry") {
    names(basins_fw)[names(basins_fw) == geom_col] <- "geometry"
    attr(basins_fw, "sf_column") <- "geometry"
  }
  bd_lu_v   <- st_make_valid(bd_lu)

  cat("  Basins with freshwater impact:", nrow(basins_fw), "\n")
  cat("  Ecoregions with landuse impact:", nrow(bd_lu_v), "\n")

  # ── BASIN-SCALE combine ───────────────────────────────────────────────────────
  # Spatial unit: drainage basin polygon. FW impacts stay at basin scale;
  # land-use BD is attached by spatially joining the overlapping ecoregion.
  # Basins with no cropland ecoregion overlap are excluded entirely (fw-only
  # zones are not meaningful without a land-use anchor).
  # Ecoregions not overlapping any basin appear as lu-only, halved by /2.
  if (combine_scale == "basin") {

    # Step A: attach ecoregion landuse_bd to each basin via spatial join
    basin_lu_join <- st_join(
      basins_fw[, c("id_basin_pcrglob", "fw_total")],
      bd_lu_v[,  "landuse_bd"],
      join = st_intersects, left = TRUE
    ) %>%
      st_drop_geometry() %>%
      group_by(id_basin_pcrglob, fw_total) %>%
      summarize(landuse_bd_mean = mean(landuse_bd, na.rm = TRUE), .groups = "drop")

    # Step B: keep only basins that overlap ≥1 cropland ecoregion, compute combined_bd
    basins_combined <- basins_fw[, c("id_basin_pcrglob", "fw_total")] %>%
      left_join(basin_lu_join, by = c("id_basin_pcrglob", "fw_total")) %>%
      filter(!is.na(landuse_bd_mean) & !is.nan(landuse_bd_mean))
    basins_combined$combined_bd <- (basins_combined$fw_total + basins_combined$landuse_bd_mean) / 2

    # Step C: ecoregions with no overlapping basin → lu-only; halved (/2 = one-realm average)
    has_basin_idx   <- lengths(st_intersects(bd_lu_v, basins_fw)) > 0
    lu_only         <- bd_lu_v[!has_basin_idx, c("landuse_bd", "geometry")]
    lu_only$combined_bd <- lu_only$landuse_bd / 2

    cat("  Basins overlapping ≥1 ecoregion:", nrow(basins_combined), "\n")
    cat("  Ecoregions with no basin overlap (lu-only):", sum(!has_basin_idx), "\n")

    # Step D: cast to consistent MULTIPOLYGON and bind
    basins_mp  <- st_cast(st_make_valid(basins_combined[, c("combined_bd", "geometry")]), "MULTIPOLYGON")
    lu_only_mp <- st_cast(st_make_valid(lu_only[,         c("combined_bd", "geometry")]), "MULTIPOLYGON")
    combined_sf <- bind_rows(basins_mp, lu_only_mp)
    combined_sf <- combined_sf[!is.na(combined_sf$combined_bd) & combined_sf$combined_bd > 0, ]
    cat("  Combined BD map:", nrow(combined_sf), "polygons\n")

  # ── ECOREGION-SCALE combine ───────────────────────────────────────────────────
  # Spatial unit: terrestrial ecoregion polygon (TEOW, cropland-intense only).
  # All FW stressors (water, N, P) are aggregated up from basin scale to the
  # ecoregion by summing fw_total of all basins that intersect each ecoregion.
  # combined_bd = (landuse_bd + fw_total_eco) / 2 in all cases — a strict
  # two-realm average. When fw_total_eco = 0 (no FW impact) this gives
  # landuse_bd / 2 automatically, implementing the one-realm-average rule.
  } else if (combine_scale == "ecoregion") {

    # Step A: freshwater impacts aggregated to ecoregion scale.
    #
    # The naive st_join(..., join=st_intersects) approach assigns the FULL fw_total
    # of each basin to every ecoregion it touches, regardless of how much overlap
    # there actually is — causing severe double-counting when large basins span many
    # ecoregion boundaries. Fix: use spatially correct methods for each sub-stressor.
    #
    # N and P: n_bd_raster / p_bd_raster are gridded rasters already in memory.
    #   terra::extract(..., fun="sum") assigns each raster cell to exactly one ecoregion
    #   polygon, identical to how the land-use step works — no double-counting possible.
    #
    # Blue water: bd_water is at basin resolution (no raster equivalent available).
    #   st_intersection() computes actual overlap polygons; we then apportion each
    #   basin's water_bd proportionally by (intersection_area / basin_total_area),
    #   so the total water_bd is conserved across ecoregions.

    cat("  [ecoregion FW] Extracting N raster to ecoregions...\n")
    eco_n_bd <- if (!is.null(n_bd_raster)) {
      terra::extract(rast(n_bd_raster), vect(bd_lu_v), fun = "sum", na.rm = TRUE)[, 2]
    } else {
      rep(0, nrow(bd_lu_v))
    }

    cat("  [ecoregion FW] Extracting P raster to ecoregions...\n")
    eco_p_bd <- if (!is.null(p_bd_raster)) {
      terra::extract(rast(p_bd_raster), vect(bd_lu_v), fun = "sum", na.rm = TRUE)[, 2]
    } else {
      rep(0, nrow(bd_lu_v))
    }

    # Pixel-level water BD raster — mirrors the N and P workflow exactly.
    #
    # The previous area-apportionment approach (st_intersection + int_area/basin_area)
    # assumed water consumption is spatially uniform across each basin. In practice,
    # production is concentrated in a few raster pixels; using basin area as a proxy
    # routes water BD to whatever ecoregion covers most of the basin's land area,
    # which may be completely different from where the crop actually grows. When a
    # small production footprint lands in a high-CF basin whose area is mostly
    # montane forest, the result is an extreme and spurious BD spike in that ecoregion.
    #
    # Fix: rasterize basin CFs to the production raster's resolution, multiply
    # pixel-wise by the blue water footprint raster, then extract to ecoregions with
    # terra::extract(fun="sum"). Each pixel's water BD is attributed to the ecoregion
    # where production occurs — identical in logic to the N and P stressors.
    cat("  [ecoregion FW] Building pixel-level water BD raster...\n")
    if (!is.null(fp_blue)) {
      # Join CF to every basin polygon, then rasterize to fp_blue resolution.
      # Basins without a matching CF get 0 (no impact, not NA) so the raster
      # multiplication doesn't produce spurious NAs in productive pixels.
      basins_with_cf <- basins_sf %>%
        mutate(id_basin_pcrglob = as.integer(id_basin_pcrglob)) %>%
        left_join(
          water_cf[, c("id_basin_pcrglob", "CF_GLOB_A_m")],
          by = "id_basin_pcrglob"
        ) %>%
        mutate(CF_GLOB_A_m = ifelse(is.na(CF_GLOB_A_m) | CF_GLOB_A_m <= 0,
                                    0, CF_GLOB_A_m))

      basin_cf_rast <- terra::rasterize(
        terra::vect(basins_with_cf),
        terra::rast(fp_blue),
        field = "CF_GLOB_A_m",
        fun   = "mean"          # basins don't overlap so "mean" == "first" == "last"
      )

      # Pixel water BD = blue water footprint (m3/pixel) x basin CF (PDF.yr/m3)
      water_bd_rast <- terra::rast(fp_blue) * basin_cf_rast
      water_bd_rast[water_bd_rast <= 0] <- NA

      eco_water_bd <- terra::extract(
        water_bd_rast, terra::vect(bd_lu_v), fun = "sum", na.rm = TRUE
      )[, 2]
      eco_water_bd[is.na(eco_water_bd)] <- 0
      cat("  [ecoregion FW] Ecoregions with pixel-level water BD:",
          sum(eco_water_bd > 0, na.rm = TRUE), "\n")
    } else {
      eco_water_bd <- rep(0, nrow(bd_lu_v))
    }

    # Combined FW total per ecoregion (row-order matches bd_lu_v)
    eco_fw_total <- eco_n_bd + eco_p_bd + eco_water_bd
    cat("  [ecoregion FW] Ecoregions with FW impact:",
        sum(eco_fw_total > 0, na.rm = TRUE), "\n")

    # Step B: attach per-ecoregion FW totals and compute combined_bd
    combined_sf <- bd_lu_v %>%
      mutate(fw_total_eco = ifelse(is.na(eco_fw_total), 0, eco_fw_total),
             combined_bd  = (landuse_bd + fw_total_eco) / 2)
    combined_sf <- combined_sf[combined_sf$combined_bd > 0, ]
    combined_sf <- st_cast(
      st_make_valid(combined_sf[, c("ECO_ID", "ECO_NAME",
                                    "landuse_bd", "fw_total_eco",
                                    "combined_bd", "geometry")]),
      "MULTIPOLYGON"
    )

    cat("  Ecoregions with FW impact:   ",
        sum(combined_sf$fw_total_eco > 0, na.rm = TRUE), "\n")
    cat("  Ecoregions lu-only (fw=0):   ",
        sum(combined_sf$fw_total_eco == 0, na.rm = TRUE), "\n")
    cat("  Combined BD map:", nrow(combined_sf), "ecoregion polygons\n")

  } else {
    stop("combine_scale must be 'basin' or 'ecoregion', got: '", combine_scale, "'")
  }

  if (save) {
    # Cast to MULTIPOLYGON before writing: ensures a single consistent geometry type
    # in the GPKG (avoids "Unknown (any)" which ArcGIS cannot open).
    combined_out <- st_cast(st_make_valid(combined_sf), "MULTIPOLYGON")
    out_comb <- paste0("X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_combined/",
                       ckey, "/", ckey, "_bd_footprint_combined_", year, "_", stim_item, ".gpkg")
    dir.create(dirname(out_comb), recursive = TRUE, showWarnings = FALSE)
    st_write(combined_out, out_comb, delete_layer = TRUE)
  }


  # STEP 6: Figures -------------------------------------------------------------
  if (figures) {
    cat("[6/6] Generating maps...\n")

    # — Figure 1: Landuse BD —
    if (nrow(bd_lu) > 0) {
      pal <- make_pal(bd_lu$landuse_bd)
      print(
        leaflet(data = bd_lu) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addPolygons(
            fillColor   = ~ifelse(is.na(landuse_bd) | landuse_bd == 0, "transparent", pal(landuse_bd)),
            color       = "lightgrey", weight = 0.1, opacity = 0.4,
            fillOpacity = ~ifelse(is.na(landuse_bd) | landuse_bd == 0, 0, 0.7),
            popup       = ~paste0("Ecoregion: ", ECO_NAME, "<br>BD impact: ",
                                   formatC(landuse_bd, format = "e", digits = 2), " PDF\u00b7yr")
          ) %>%
          addLegend(pal = pal, values = bd_lu$landuse_bd,
                    title = paste0("Land use BD (PDF\u00b7yr)<br>", stim_item),
                    position = "bottomright")
      )
    }

    # — Figure 2: Water BD —
    if (nrow(bd_water) > 0) {
      pal <- make_pal(bd_water$water_bd)
      print(
        leaflet(data = bd_water) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addPolygons(
            fillColor   = ~ifelse(is.na(water_bd) | water_bd == 0, "transparent", pal(water_bd)),
            color       = "lightgrey", weight = 0.1, opacity = 0.4,
            fillOpacity = ~ifelse(is.na(water_bd) | water_bd == 0, 0, 0.7),
            popup       = ~paste0("Basin: ", id_basin_pcrglob, "<br>BD impact: ",
                                   formatC(water_bd, format = "e", digits = 2), " PDF\u00b7yr")
          ) %>%
          addLegend(pal = pal, values = bd_water$water_bd,
                    title = paste0("Water BD (PDF\u00b7yr)<br>", stim_item),
                    position = "bottomright")
      )
    }

    # — Figure 3: N BD raster —
    if (!is.null(n_bd_raster)) {
      n_wgs84 <- if (!compareCRS(n_bd_raster, CRS("+proj=longlat +datum=WGS84"))) {
        projectRaster(n_bd_raster, crs = CRS("+proj=longlat +datum=WGS84"))
      } else { n_bd_raster }
      pal <- make_pal(values(n_wgs84))
      print(
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addRasterImage(n_wgs84, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = values(n_wgs84),
                    title = paste0("N BD (PDF\u00b7yr/cell)<br>", stim_item),
                    position = "bottomright")
      )
    } else {
      cat("  [Figure 3] No N raster for", stim_item, "— skipping.\n")
    }

    # — Figure 4: P BD raster —
    if (!is.null(p_bd_raster)) {
      p_wgs84 <- if (!compareCRS(p_bd_raster, CRS("+proj=longlat +datum=WGS84"))) {
        projectRaster(p_bd_raster, crs = CRS("+proj=longlat +datum=WGS84"))
      } else { p_bd_raster }
      pal <- make_pal(values(p_wgs84))
      print(
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addRasterImage(p_wgs84, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = values(p_wgs84),
                    title = paste0("P BD (PDF\u00b7yr/cell)<br>", stim_item),
                    position = "bottomright")
      )
    } else {
      cat("  [Figure 4] No P raster for", stim_item, "— skipping.\n")
    }

    # — Figure 5: Combined BD —
    if (nrow(combined_sf) > 0) {

      if (combine_scale == "basin") {
        # Interactive leaflet map for basin-scale output
        pal <- make_pal(combined_sf$combined_bd)
        print(
          leaflet(data = combined_sf) %>%
            addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
            addPolygons(
              fillColor   = ~pal(combined_bd),
              color       = "lightgrey", weight = 0.1, opacity = 0.3,
              fillOpacity = 0.75,
              popup       = ~paste0("Combined BD impact: ",
                                     formatC(combined_bd, format = "e", digits = 2), " PDF\u00b7yr")
            ) %>%
            addLegend(pal = pal, values = combined_sf$combined_bd,
                      title = paste0("Combined BD (PDF\u00b7yr)<br>", stim_item),
                      position = "bottomright")
        )

      } else {
        # Publication-quality static map for ecoregion-scale output
        world_outline <- st_as_sf(countries_shp)

        fig_combined <- ggplot() +
          geom_sf(data  = world_outline,
                  fill  = "grey92", colour = "white", linewidth = 0.15) +
          geom_sf(data  = combined_sf,
                  aes(fill = combined_bd), colour = NA) +
          scale_fill_viridis_c(
            name   = expression(paste("BD impact (PDF", "\u00b7", "yr)")),
            trans  = "log10",
            labels = scales::label_scientific(digits = 1),
            option = "inferno",
            direction = -1,
            guide  = guide_colorbar(
              barwidth       = 12,
              barheight      = 0.5,
              title.position = "top",
              title.hjust    = 0.5
            )
          ) +
          coord_sf(crs = "+proj=robin", datum = NA) +
          labs(
            title    = paste("Combined biodiversity footprint \u2014", stim_item),
            subtitle = paste0("Consumer: ", ckey, "  \u2022  Year: ", year,
                              "  \u2022  Terrestrial ecoregion scale"),
            caption  = paste0(
              "Two-realm average of land-use and freshwater (water, N, P) BD impacts.\n",
              "Ecoregions without freshwater overlap: combined BD = land-use BD \u00f7 2."
            )
          ) +
          theme_void(base_size = 11) +
          theme(
            plot.title         = element_text(face = "bold", size = 13,
                                              margin = margin(b = 4)),
            plot.subtitle      = element_text(colour = "grey40", size = 9),
            plot.caption       = element_text(colour = "grey50", size = 7,
                                              hjust = 0),
            legend.position    = "bottom",
            legend.title.align = 0.5,
            plot.margin        = margin(8, 8, 8, 8)
          )

        print(fig_combined)

        if (save) {
          fig_path <- paste0(
            "X:/Eli/projects/fabio_stimulants/results_data/bd_footprint_combined/",
            ckey, "/", ckey, "_bd_footprint_combined_", year, "_", stim_item, ".png"
          )
          ggsave(fig_path, fig_combined,
                 width = 18, height = 10, units = "cm",
                 dpi = 300, bg = "white")
          cat("  Figure saved:", fig_path, "\n")
        }
      }

    } else {
      warning("Combined BD map has 0 polygons — check stressor outputs above.")
    }
  }

  cat("\nDone.\n")

  # Return all outputs as a named list (no global env side effects)
  invisible(list(
    landuse  = bd_lu,
    water    = bd_water,
    n_raster = n_bd_raster,
    p_raster = p_bd_raster,
    combined = combined_sf
  ))
}


################################################################################
# EXECUTION --------------------------------------------------------------------
################################################################################

# Run all stressors for coffee consumed by the United States, year 2020.
# Five leaflet maps are printed; all outputs returned as a named list.
for (stim_item in c("cocoa", "coffee", "tea", "tobacco")){
  raster::removeTmpFiles(h = 0)   # clear stale raster temp files from previous iteration
  result <- bd_footprint(
    stim_item    = stim_item,
    country_list = world,
    year         = 2020,
    figures      = TRUE,
    save         = TRUE,
    combine_scale="ecoregion"
  )
}

# Access individual results:
#   result$landuse    — sf, landuse BD by ecoregion   (col: landuse_bd)
#   result$water      — sf, water BD by basin          (col: water_bd)
#   result$n_raster   — RasterLayer, N BD              (PDF·yr/cell)
#   result$p_raster   — RasterLayer, P BD              (PDF·yr/cell)
#   result$combined   — sf, all stressors combined     (col: combined_bd)

# Other examples:
# result_eu   <- bd_footprint("coffee", EU,    2020)
# result_coco <- bd_footprint("cocoa",  world, 2020)
# result_save <- bd_footprint("tea", "United Kingdom", 2020, save = TRUE)
