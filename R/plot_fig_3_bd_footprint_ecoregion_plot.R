################################################################################
# plot_bd_footprint_ecoregion.R
#
# Standalone figure script for bd_footprint() ecoregion-scale results.
# Reads the .gpkg and .tif files saved by bd_footprint(..., save = TRUE) and
# reproduces all five stressor maps without re-running the analysis pipeline.
#
# Usage: set the four variables in the CONFIG section, then source the script
# (or run it interactively section by section).
#
# Output files saved to the same folder as the combined .gpkg, suffixed _fig*.png
################################################################################


# ── CONFIG ────────────────────────────────────────────────────────────────────
# Adjust these to match the run you want to plot.

stim_item     <- "coffee"          # "coffee", "cocoa", "tea", "tobacco"
ckey          <- "world"           # consumer key used when saving: "world", "EU",
                                   # "47_countries", or a single country name
year          <- 2020
combine_scale <- "ecoregion"       # "ecoregion" → ggplot2 static map (Fig 5)
                                   # "basin"     → leaflet interactive map (Fig 5)

save_figs     <- TRUE              # write .png files alongside the .gpkg?
show_inset    <- FALSE              # add top-right zoom inset of most-impacted
                                   # ecoregion? Set FALSE for a clean full map.

# ── PACKAGES ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(raster)
  library(leaflet)
  library(viridis)
  library(ggplot2)
  library(scales)
})


# ── FILE PATHS ────────────────────────────────────────────────────────────────
base_results <- "X:/Eli/projects/fabio_stimulants/results_data"

for (stim_item in c("cocoa","coffee","tea","tobacco")){
#for (stim_item in c("tea")){  
  path_lu   <- file.path(base_results, "bd_footprint_by_ecoregion", ckey,
                         paste0(ckey, "_bd_footprint_", year, "_", stim_item, ".gpkg"))
  
  path_water <- file.path(base_results, "bd_footprint_by_basin", ckey,
                          paste0(ckey, "_bd_footprint_", year, "_", stim_item, ".gpkg"))
  
  path_n    <- file.path(base_results, "bd_footprint_by_np_scale",
                         paste0(ckey, "_bd_footprint_n_", year, "_", stim_item, ".tif"))
  
  path_p    <- file.path(base_results, "bd_footprint_by_np_scale",
                         paste0(ckey, "_bd_footprint_p_", year, "_", stim_item, ".tif"))
  
  path_comb <- file.path(base_results, "bd_footprint_combined", ckey,
                         paste0(ckey, "_bd_footprint_combined_", year, "_", stim_item, ".gpkg"))
  
  fig_dir   <- file.path(base_results, "bd_footprint_combined", ckey)
  
  
  # ── LOAD DATA ─────────────────────────────────────────────────────────────────
  cat("Loading saved outputs for:", stim_item, "/", ckey, "/", year, "\n")
  
  # Land use (always present)
  if (!file.exists(path_lu))
    stop("Land-use .gpkg not found:\n  ", path_lu)
  bd_lu <- st_read(path_lu, quiet = TRUE)
  cat("  bd_lu:      ", nrow(bd_lu), "ecoregion polygons\n")
  
  # Blue water (may be absent for tea / tobacco)
  if (file.exists(path_water)) {
    bd_water <- st_read(path_water, quiet = TRUE)
    cat("  bd_water:   ", nrow(bd_water), "basin polygons\n")
  } else {
    bd_water <- NULL
    cat("  bd_water:    not found (no blue water for this commodity) — skipping Fig 2.\n")
  }
  
  # N raster (may be absent)
  if (file.exists(path_n)) {
    n_bd_raster <- raster(path_n)
    cat("  n_bd_raster: loaded\n")
  } else {
    n_bd_raster <- NULL
    cat("  n_bd_raster: not found — skipping Fig 3.\n")
  }
  
  # P raster (may be absent)
  if (file.exists(path_p)) {
    p_bd_raster <- raster(path_p)
    cat("  p_bd_raster: loaded\n")
  } else {
    p_bd_raster <- NULL
    cat("  p_bd_raster: not found — skipping Fig 4.\n")
  }
  
  # Combined map
  if (!file.exists(path_comb))
    stop("Combined .gpkg not found:\n  ", path_comb)
  combined_sf <- st_read(path_comb, quiet = TRUE)
  cat("  combined_sf:", nrow(combined_sf), "polygons\n\n")
  
  # World outline (needed for the ggplot2 ecoregion static map only)
  countries_shp <- st_read("X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp",
                            quiet = TRUE)
  
  
  # ── HELPER: log-scale colour palette (leaflet) ────────────────────────────────
  make_pal <- function(x) {
    colorNumeric(palette = inferno(100), domain = x[!is.na(x)], na.color = "transparent")
  }
  
  
  # ── FIGURE 1: Land-use BD (leaflet) ───────────────────────────────────────────
  cat("Figure 1: Land-use BD...\n")
  if (nrow(bd_lu) > 0) {
    pal <- make_pal(bd_lu$landuse_bd)
    print(
      leaflet(data = bd_lu) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          fillColor   = ~ifelse(is.na(landuse_bd) | landuse_bd == 0,
                                "transparent", pal(landuse_bd)),
          color       = "lightgrey", weight = 0.1, opacity = 0.4,
          fillOpacity = ~ifelse(is.na(landuse_bd) | landuse_bd == 0, 0, 0.7),
          popup       = ~paste0("Ecoregion: ", ECO_NAME, "<br>BD impact: ",
                                 formatC(landuse_bd, format = "e", digits = 2),
                                 " PDF\u00b7yr")
        ) %>%
        addLegend(pal = pal, values = bd_lu$landuse_bd,
                  title    = paste0("Land use BD (PDF\u00b7yr)<br>", stim_item),
                  position = "bottomright")
    )
  } else {
    cat("  No land-use polygons — skipping.\n")
  }
  
  
  # ── FIGURE 2: Blue water BD (leaflet) ─────────────────────────────────────────
  cat("Figure 2: Blue water BD...\n")
  if (!is.null(bd_water) && nrow(bd_water) > 0) {
    pal <- make_pal(bd_water$water_bd)
    print(
      leaflet(data = bd_water) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          fillColor   = ~ifelse(is.na(water_bd) | water_bd == 0,
                                "transparent", pal(water_bd)),
          color       = "lightgrey", weight = 0.1, opacity = 0.4,
          fillOpacity = ~ifelse(is.na(water_bd) | water_bd == 0, 0, 0.7),
          popup       = ~paste0("Basin: ", id_basin_pcrglob, "<br>BD impact: ",
                                 formatC(water_bd, format = "e", digits = 2),
                                 " PDF\u00b7yr")
        ) %>%
        addLegend(pal = pal, values = bd_water$water_bd,
                  title    = paste0("Water BD (PDF\u00b7yr)<br>", stim_item),
                  position = "bottomright")
    )
  } else {
    cat("  No blue water data — skipping.\n")
  }
  
  
  # ── FIGURE 3: N BD raster (leaflet) ───────────────────────────────────────────
  cat("Figure 3: N BD raster...\n")
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
                  title    = paste0("N BD (PDF\u00b7yr/cell)<br>", stim_item),
                  position = "bottomright")
    )
  } else {
    cat("  No N raster — skipping.\n")
  }
  
  
  # ── FIGURE 4: P BD raster (leaflet) ───────────────────────────────────────────
  cat("Figure 4: P BD raster...\n")
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
                  title    = paste0("P BD (PDF\u00b7yr/cell)<br>", stim_item),
                  position = "bottomright")
    )
  } else {
    cat("  No P raster — skipping.\n")
  }
  
  
  # ── FIGURE 5: Combined BD ─────────────────────────────────────────────────────
  cat("Figure 5: Combined BD...\n")
  if (nrow(combined_sf) == 0) {
    warning("Combined BD map has 0 polygons — nothing to plot.")
  
  } else if (combine_scale == "basin") {
    # Interactive leaflet map (basin-scale output)
    pal <- make_pal(combined_sf$combined_bd)
    print(
      leaflet(data = combined_sf) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(
          fillColor   = ~pal(combined_bd),
          color       = "lightgrey", weight = 0.1, opacity = 0.3,
          fillOpacity = 0.75,
          popup       = ~paste0("Combined BD impact: ",
                                 formatC(combined_bd, format = "e", digits = 2),
                                 " PDF\u00b7yr")
        ) %>%
        addLegend(pal = pal, values = combined_sf$combined_bd,
                  title    = paste0("Combined BD (PDF\u00b7yr)<br>", stim_item),
                  position = "bottomright")
    )
  
  } else {
    # Publication-quality static ggplot2 map (ecoregion-scale output)
    world_outline <- countries_shp
    robin_crs     <- "+proj=robin"

    # ── Shared colour scale limits (both maps must use identical limits) ─────
    bd_range <- range(combined_sf$combined_bd[combined_sf$combined_bd > 0],
                      na.rm = TRUE)

    # ── Inset bbox: most-impacted ecoregion + tight 0.2× padding ────────────
    top_eco  <- combined_sf[which.max(combined_sf$combined_bd), ]
    top_bbox <- st_bbox(st_transform(top_eco, crs = robin_crs))
    pad_x    <- (top_bbox["xmax"] - top_bbox["xmin"]) * 0.2
    pad_y    <- (top_bbox["ymax"] - top_bbox["ymin"]) * 0.2
    inset_xlim <- c(top_bbox["xmin"] - pad_x, top_bbox["xmax"] + pad_x)
    inset_ylim <- c(top_bbox["ymin"] - pad_y, top_bbox["ymax"] + pad_y)

    # ── Main map ─────────────────────────────────────────────────────────────
    fig_combined <- ggplot() +
      # Globe border: densified WGS84 rectangle so edges curve in Robinson projection
      geom_sf(data      = st_segmentize(
                            st_as_sfc("POLYGON((-179.9 -89.9, -179.9 89.9, 179.9 89.9,
                                                179.9 -89.9, -179.9 -89.9))", crs = 4326),
                            dfMaxLength = 1),
              fill      = NA, colour = "grey80", linewidth = 0.4) +
      geom_sf(data      = world_outline,
              fill      = "grey92", colour = "white", linewidth = 0.15) +
      geom_sf(data      = combined_sf,
              aes(fill  = combined_bd), colour = NA) +
      # Light box marking the inset location — only drawn when inset is shown
      { if (show_inset)
          annotate("rect",
                   xmin = inset_xlim[1], xmax = inset_xlim[2],
                   ymin = inset_ylim[1], ymax = inset_ylim[2],
                   fill = NA, colour = "grey65", linewidth = 0.45)
        else
          NULL } +
      scale_fill_distiller(
        name      = expression(paste("Biodiversity impact (PDF", "\u00b7", "yr)")),
        palette   = "YlOrBr",
        direction = 1,
        trans     = "log10",
        limits    = bd_range,
        labels    = scales::label_scientific(digits = 1),
        na.value  = "transparent",
        guide     = guide_colorbar(
          barwidth       = 12,
          barheight      = 0.5,
          title.position = "top",
          title.hjust    = 0.5
        )
      ) +
      coord_sf(crs = robin_crs, datum = NA) +
      labs(
        subtitle = paste0(stim_item, " \u2022 ", "consumer: ", ckey, " \u2022 ", year)
      ) +
      theme_void(base_size = 11) +
      theme(
        plot.subtitle      = element_text(colour = "grey40", size = 9, hjust = 0.5),
        legend.position    = "bottom",
        legend.title.align = 0.5,
        plot.margin        = margin(8, 8, 8, 8)
      )

    # ── Inset map + composition (only when show_inset = TRUE) ───────────────
    if (show_inset) {

      inset_plot <- ggplot() +
        geom_sf(data      = world_outline,
                fill      = "grey92", colour = "white", linewidth = 0.2) +
        geom_sf(data      = combined_sf,
                aes(fill  = combined_bd), colour = NA) +
        scale_fill_distiller(
          palette   = "YlOrBr",
          direction = 1,
          trans     = "log10",
          limits    = bd_range,
          na.value  = "transparent",
          guide     = "none"
        ) +
        coord_sf(crs    = robin_crs,
                 xlim   = inset_xlim,
                 ylim   = inset_ylim,
                 datum  = NA) +
        theme_void() +
        theme(
          panel.border    = element_rect(colour = "grey65", fill = NA,
                                         linewidth = 0.45),
          plot.background = element_rect(fill = "white", colour = NA)
        )

    }

    # ── Draw helper: main map + optional inset via grid viewport ─────────────
    # grid::viewport uses npc (normalized parent coordinates): (0,0) = bottom-
    # left, (1,1) = top-right of the ENTIRE figure output including all margins.
    # This is the only reliable way to anchor the inset to the literal corner
    # of the PNG without any overlap with the map panel.
    draw_map <- function() {
      grid::grid.newpage()
      grid::grid.draw(ggplotGrob(fig_combined))
      if (show_inset) {
        grid::pushViewport(grid::viewport(
          x      = grid::unit(0.99, "npc"),
          y      = grid::unit(0.03, "npc"),
          width  = grid::unit(0.25, "npc"),   # ~4.5 cm on an 18 cm figure
          height = grid::unit(0.34, "npc"),   # ~3.4 cm on a 10 cm figure
          just   = c("right", "bottom")
        ))
        grid::grid.draw(ggplotGrob(inset_plot))
        grid::popViewport()
      }
    }

    draw_map()

    if (save_figs) {
      dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
      if (show_inset){
      fig_path <- file.path(paste0("X:/Eli/projects/fabio_stimulants/results_figures/combined_impacts/",
                                   ckey, "_bd_footprint_combined_", year, "_",
                                   stim_item, "_inset.png"))
      } else {
      fig_path <- file.path(paste0("X:/Eli/projects/fabio_stimulants/results_figures/combined_impacts/",
                                   ckey, "_bd_footprint_combined_", year, "_",
                                   stim_item, ".png"))        
      }
      png(fig_path, width = 18, height = 10, units = "cm", res = 300, bg = "white")
      draw_map()
      dev.off()
      cat("  Figure saved:", fig_path, "\n")
    }
  }
  
  cat("\nDone.\n")
}


################################################################################
# FIGURE 6: All stimulants combined ──────────────────────────────────────────
# Sums combined_bd across all four commodities per ecoregion and renders a
# single "total stimulant sector" map with the same aesthetic as Figure 5.
################################################################################

cat("\n── Figure 6: All-stimulants combined map ──\n")

all_stims  <- c("coffee", "cocoa", "tea", "tobacco")
stim_dfs   <- list()
geom_base  <- NULL     # geometry carrier — taken from first successful load

for (stim in all_stims) {
  p <- file.path(base_results, "bd_footprint_combined", ckey,
                 paste0(ckey, "_bd_footprint_combined_", year, "_", stim, ".gpkg"))
  if (!file.exists(p)) {
    warning("All-stimulants map: ", stim, " .gpkg not found — skipped.")
    next
  }
  sf_obj <- st_read(p, quiet = TRUE)
  cat("  Loaded:", stim, "(", nrow(sf_obj), "polygons )\n")
  if (is.null(geom_base)) geom_base <- sf_obj[, "ECO_ID"]   # keep geometry once
  df <- st_drop_geometry(sf_obj)[, c("ECO_ID", "combined_bd")]
  names(df)[2] <- paste0(stim, "_bd")
  stim_dfs[[stim]] <- df
}

print(length(stim_dfs))

if (length(stim_dfs) == 0) {
  warning("No combined .gpkg files found — Figure 6 skipped.")
} else {

  # ── Sum across commodities ──────────────────────────────────────────────────
  all_df  <- Reduce(function(a, b) merge(a, b, by = "ECO_ID", all = TRUE),
                    stim_dfs)
  bd_cols <- intersect(paste0(all_stims, "_bd"), names(all_df))
  all_df$all_stimulants_bd <- rowSums(all_df[, bd_cols], na.rm = TRUE)

  allstim_sf <- merge(geom_base,
                      all_df[, c("ECO_ID", "all_stimulants_bd")],
                      by = "ECO_ID", all.x = TRUE)
  allstim_sf <- allstim_sf[!is.na(allstim_sf$all_stimulants_bd) &
                              allstim_sf$all_stimulants_bd > 0, ]
  cat("  allstim_sf:", nrow(allstim_sf), "polygons with non-zero impact\n")

  # ── Shared objects reused from main loop ────────────────────────────────────
  # countries_shp is still in scope from the last loop iteration.
  # If the loop ran 0 iterations, reload it here:
  if (!exists("countries_shp")) {
    countries_shp <- st_read(
      "X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp",
      quiet = TRUE)
  }
  world_outline <- countries_shp
  robin_crs     <- "+proj=robin"

  # ── Colour scale limits ─────────────────────────────────────────────────────
  bd_range_all <- range(allstim_sf$all_stimulants_bd, na.rm = TRUE)

  # ── Main map (no inset for the all-stimulants aggregate) ────────────────────
  fig_all <- ggplot() +
    geom_sf(data  = st_segmentize(
                      st_as_sfc("POLYGON((-179.9 -89.9, -179.9 89.9, 179.9 89.9,
                                          179.9 -89.9, -179.9 -89.9))", crs = 4326),
                      dfMaxLength = 1),
            fill  = NA, colour = "grey80", linewidth = 0.4) +
    geom_sf(data  = world_outline,
            fill  = "grey92", colour = "white", linewidth = 0.15) +
    geom_sf(data  = allstim_sf,
            aes(fill = all_stimulants_bd), colour = NA) +
    scale_fill_distiller(
      name      = expression(paste("BD impact (PDF", "\u00b7", "yr)")),
      palette   = "YlOrBr",
      direction = 1,
      trans     = "log10",
      limits    = bd_range_all,
      labels    = scales::label_scientific(digits = 1),
      na.value  = "transparent",
      guide     = guide_colorbar(
        barwidth       = 12,
        barheight      = 0.5,
        title.position = "top",
        title.hjust    = 0.5
      )
    ) +
    coord_sf(crs = robin_crs, datum = NA) +
    labs(subtitle = paste0("All stimulants \u2022 consumer: ", ckey,
                           " \u2022 ", year)) +
    theme_void(base_size = 11) +
    theme(
      plot.subtitle      = element_text(colour = "grey40", size = 9, hjust = 0.5),
      legend.position    = "bottom",
      legend.title.align = 0.5,
      plot.margin        = margin(8, 8, 8, 8)
    )

  # ── Draw helper (no inset for the aggregate map) ────────────────────────────
  draw_all <- function() {
    grid::grid.newpage()
    grid::grid.draw(ggplotGrob(fig_all))
  }

  draw_all()

  # ── Save ─────────────────────────────────────────────────────────────────────
  if (save_figs) {
    fig_path_all <- paste0(
      "X:/Eli/projects/fabio_stimulants/results_figures/combined_impacts/",
      ckey, "_bd_footprint_combined_", year, "_all_stimulants.png")
    dir.create(dirname(fig_path_all), recursive = TRUE, showWarnings = FALSE)
    png(fig_path_all, width = 18, height = 10, units = "cm",
        res = 300, bg = "white")
    draw_all()
    dev.off()
    cat("  Figure saved:", fig_path_all, "\n")
  }

  cat("\nFigure 6 done.\n")
}
