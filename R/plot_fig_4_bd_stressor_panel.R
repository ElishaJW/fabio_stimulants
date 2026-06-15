################################################################################
# plot_fig_4_bd_stressor_panel.R
#
# 4 × 4 panel map figure:
#   Columns = commodities  (coffee, cocoa, tea, tobacco)
#   Rows    = stressors    (land use, water, N deposition, P deposition)
#
# Each row shares a log10 colour scale across all 4 commodities so intensities
# are directly comparable within a stressor. Palette: viridis "turbo"
# (blue → cyan → yellow → orange → red) — heatmap-style, perceptually uniform.
#
# Vector layers (land use, water): read as sf, plotted with geom_sf.
# Raster layers (N, P): projected to Robinson with terra, plotted with
# geom_raster. Missing files (e.g. no water data for tea/tobacco) render as
# a labelled empty panel rather than an error.
################################################################################


# ── CONFIG ────────────────────────────────────────────────────────────────────
ckey     <- "world"   # consumer key used when saving — "world", "EU", etc.
year     <- 2020
save_fig <- TRUE


# ── PACKAGES ──────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(ggplot2)
  library(scales)
  library(gridExtra)   # grid.arrange — part of most R installations
  library(grid)        # grid.draw, textGrob — always available (base R)
})


# ── CONSTANTS ─────────────────────────────────────────────────────────────────
base_results     <- "X:/Eli/projects/fabio_stimulants/results_data"
robin_crs        <- "+proj=robin"
commodities      <- c("coffee",  "cocoa",  "tea",   "tobacco")
commodity_labels <- c("Coffee",  "Cocoa",  "Tea",   "Tobacco")

# Stressor metadata list — type "vector" reads .gpkg, type "raster" reads .tif
stressors <- list(
  landuse = list(
    label   = "Land use BD",
    type    = "vector",
    col     = "landuse_bd",
    path_fn = function(stim)
      file.path(base_results, "bd_footprint_by_ecoregion", ckey,
                paste0(ckey, "_bd_footprint_", year, "_", stim, ".gpkg"))
  ),
  water = list(
    label   = "Water BD",
    type    = "vector",
    col     = "water_bd",
    path_fn = function(stim)
      file.path(base_results, "bd_footprint_by_basin", ckey,
                paste0(ckey, "_bd_footprint_", year, "_", stim, ".gpkg"))
  ),
  n = list(
    label   = "N deposition BD",
    type    = "raster",
    col     = NULL,
    path_fn = function(stim)
      file.path(base_results, "bd_footprint_by_np_scale",
                paste0(ckey, "_bd_footprint_n_", year, "_", stim, ".tif"))
  ),
  p = list(
    label   = "P deposition BD",
    type    = "raster",
    col     = NULL,
    path_fn = function(stim)
      file.path(base_results, "bd_footprint_by_np_scale",
                paste0(ckey, "_bd_footprint_p_", year, "_", stim, ".tif"))
  )
)


# ── BASE LAYERS (loaded once, shared by all panels) ───────────────────────────
cat("Loading base layers...\n")

world_outline <- st_read(
  "X:/Eli/DATA/shapefiles/TM_WorldBorders/TM_WORLD_BORDERS-0.3.shp",
  quiet = TRUE)

globe_border <- st_segmentize(
  st_as_sfc("POLYGON((-179.9 -89.9, -179.9 89.9, 179.9 89.9,
                       179.9 -89.9, -179.9 -89.9))", crs = 4326),
  dfMaxLength = 1)


# ── FIRST PASS: per-stressor colour ranges ────────────────────────────────────
# Collect all positive values across 4 commodities for each stressor row,
# then store range as shared limits for scale_fill_viridis_c.
cat("Computing per-stressor colour ranges...\n")
stressor_ranges <- setNames(vector("list", length(stressors)), names(stressors))

for (str_key in names(stressors)) {
  si       <- stressors[[str_key]]
  all_vals <- numeric(0)

  for (stim in commodities) {
    path <- si$path_fn(stim)
    if (!file.exists(path)) next

    vals <- if (si$type == "vector") {
      dat <- st_read(path, quiet = TRUE)
      dat[[si$col]]
    } else {
      terra::values(terra::rast(path), mat = FALSE)
    }
    all_vals <- c(all_vals, vals[is.finite(vals) & vals > 0])
  }

  stressor_ranges[[str_key]] <- if (length(all_vals)) range(all_vals) else c(NA_real_, NA_real_)
  cat(sprintf("  %-12s range: %s\n", str_key,
              paste(format(stressor_ranges[[str_key]], scientific = TRUE), collapse = " – ")))
}


# ── HELPER: build one subplot ─────────────────────────────────────────────────
make_panel <- function(str_key, stim, row_i, col_i) {

  si       <- stressors[[str_key]]
  path     <- si$path_fn(stim)
  bd_range <- stressor_ranges[[str_key]]

  # Flags controlling per-panel decoration
  show_legend  <- (col_i == length(commodities))   # legend on rightmost column only
  show_col_lbl <- (row_i == 1L)                    # commodity title on top row only
  show_row_lbl <- (col_i == 1L)                    # stressor tag on left column only

  # ── Globe border + world background ──────────────────────────────────────
  p <- ggplot() +
    geom_sf(data      = globe_border,
            fill      = NA, colour = "grey78", linewidth = 0.25) +
    geom_sf(data      = world_outline,
            fill      = "grey92", colour = "white", linewidth = 0.08)

  # ── Data layer ───────────────────────────────────────────────────────────
  has_data <- file.exists(path) && !any(is.na(bd_range))

  if (!has_data) {
    # Empty panel — centred "No data" label in Robinson space
    p <- p +
      annotate("text", x = 0, y = 0, label = "No data",
               colour = "grey55", size = 2, fontface = "italic")

  } else if (si$type == "vector") {
    dat <- st_read(path, quiet = TRUE)
    p <- p +
      geom_sf(data = dat, aes(fill = .data[[si$col]]), colour = NA)

  } else {
    # Raster: project to Robinson then convert to data frame for geom_raster.
    # Cells are equally spaced in the projected CRS so geom_raster is valid.
    r <- terra::rast(path)
    if (is.na(terra::crs(r))) terra::crs(r) <- "EPSG:4326"
    r_robin <- terra::project(r, "ESRI:54030")          # Robinson (ESRI:54030)
    df      <- as.data.frame(r_robin, xy = TRUE)
    colnames(df)[3] <- "value"
    df <- df[is.finite(df$value) & df$value > 0, , drop = FALSE]

    if (nrow(df) > 0)
      p <- p + geom_raster(data = df, aes(x = x, y = y, fill = value))
  }

  # ── Colour scale ─────────────────────────────────────────────────────────
  p <- p +
    scale_fill_viridis_c(
      option   = "turbo",
      trans    = "log10",
      limits   = bd_range,
      labels   = label_scientific(digits = 1),
      na.value = "transparent",
      name     = "BD (PDF\u00b7yr)",
      guide    = if (show_legend)
                   guide_colorbar(barwidth       = 5,
                                  barheight      = 0.35,
                                  title.position = "top",
                                  title.hjust    = 0.5)
                 else "none"
    ) +
    coord_sf(crs = robin_crs, datum = NA) +
    theme_void(base_size = 7) +
    theme(
      plot.margin      = margin(3, 3, 3, 12),   # extra left margin for row tag
      legend.position  = if (show_legend) "bottom" else "none",
      legend.title     = element_text(size = 5.5),
      legend.text      = element_text(size = 4.5),
      # Column label (commodity) — bold, centred above map
      plot.title       = element_text(size = 8, hjust = 0.5, face = "bold",
                                      margin = margin(b = 2)),
      # Row label (stressor) — rotated, on the left side
      plot.tag         = element_text(size = 6.5, face = "bold", colour = "grey20",
                                      angle = 90, hjust = 0.5, vjust = 0.5),
      plot.tag.position = "left"
    )

  # ── Panel labels ─────────────────────────────────────────────────────────
  if (show_col_lbl) p <- p + labs(title = commodity_labels[col_i])
  if (show_row_lbl) p <- p + labs(tag   = si$label)

  p
}


# ── SECOND PASS: build all 16 panels ─────────────────────────────────────────
cat("Building panels...\n")
stressor_keys <- names(stressors)
plot_list     <- vector("list", length(stressors) * length(commodities))

for (row_i in seq_along(stressor_keys)) {
  for (col_i in seq_along(commodities)) {
    idx <- (row_i - 1L) * length(commodities) + col_i
    cat(sprintf("  [%2d] %-14s / %s\n", idx, stressor_keys[row_i], commodities[col_i]))
    plot_list[[idx]] <- make_panel(stressor_keys[row_i], commodities[col_i],
                                   row_i = row_i, col_i = col_i)
  }
}


# ── ASSEMBLE ──────────────────────────────────────────────────────────────────
cat("Assembling 4\u00d74 panel...\n")

subtitle_grob <- grid::textGrob(
  paste0("Consumer: ", ckey, "  \u2022  Year: ", year,
         "  \u2022  Stressors at native spatial scale"),
  gp = grid::gpar(fontsize = 7, col = "grey40"))

fig_panel <- gridExtra::arrangeGrob(
  grobs = plot_list,
  ncol  = 4,
  nrow  = 4,
  top   = subtitle_grob)

grid::grid.newpage()
grid::grid.draw(fig_panel)


# ── SAVE ──────────────────────────────────────────────────────────────────────
if (save_fig) {
  fig_path <- paste0(
    "X:/Eli/projects/fabio_stimulants/results_figures/combined_impacts/",
    ckey, "_bd_stressor_panel_", year, ".png")
  dir.create(dirname(fig_path), recursive = TRUE, showWarnings = FALSE)
  png(fig_path, width = 30, height = 22, units = "cm", res = 300, bg = "white")
  grid::grid.draw(fig_panel)
  dev.off()
  cat("Figure saved:", fig_path, "\n")
}
