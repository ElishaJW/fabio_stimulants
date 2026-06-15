
# BD IMPACT NUMBER-LINE — STIMULANTS VS COMMON FOOD COMMODITIES
# Computes total global biodiversity footprint for 4 stimulant groups and
# 4 food comparators, then plots them on a single log-scaled number line.
#
# Stimulant groups: Coffee, Cocoa, Tea, Tobacco
# Food comparators: Wheat, Wine, Sugar cane, Beef
#
# Methodology:
#   Uses a demand-side (column-wise) footprint — the full supply chain
#   biodiversity impact attributable to global final demand for each commodity.
#   This correctly captures upstream impacts (e.g. feed crops for beef,
#   grapes for wine) that a production-row approach would miss.
#
#   For commodity X:
#     1. Build demand vector y_X: rowSums(Y), zeroed except for X rows
#     2. Propagate through supply chain: output_X = L %*% y_X
#     3. Apply per-sector BD intensities and aggregate:
#          fw_bd   = sum(output_X * (water_bd + N_bd + P_bd))
#          terr_bd = sum(output_X * (landuse_bd + CH4_bd + CO2_bd + N2O_bd))
#          total   = (fw_bd + terr_bd) / 2
#   Matching the bd_fp_all = (bd_fp_fw + bd_fp_terrestrial) / 2 formula
#   used throughout the main analysis.
#
# Eli Wilson - elisha.wilson@ntnu.no

library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(paletteer)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

save <- TRUE

# ── Load data ──────────────────────────────────────────────────────────────────
eb      <- readRDS("data/E/E_bd_2020.rds")
items   <- fread("inst/items_full.csv")
regions <- fread("inst/regions_fabio.csv")

year  <- 2020
Y_all <- readRDS("data/IO tables/Y.rds")
Y     <- as.matrix(Y_all[[as.character(year)]])
L     <- readRDS(paste0("data/IO tables/", year, "_L_mass.rds"))

# ── Row names matching the sector ordering in L and Y ─────────────────────────
combinations <- expand.grid(items$comm_code, regions$area_code)
row_names    <- paste(combinations$Var1, combinations$Var2, sep = "_")

# ── Precompute per-sector BD intensity vectors (NAs → 0) ──────────────────────
# Freshwater stressors
e_water <- as.numeric(eb["water_bd",   ]); e_water[is.na(e_water)] <- 0
e_N     <- as.numeric(eb["N_bd",       ]); e_N[is.na(e_N)]         <- 0
e_P     <- as.numeric(eb["P_bd",       ]); e_P[is.na(e_P)]         <- 0
e_fw    <- e_water + e_N + e_P

# Terrestrial stressors
e_land  <- as.numeric(eb["landuse_bd", ]); e_land[is.na(e_land)]   <- 0
e_CH4   <- as.numeric(eb["CH4_bd",     ]); e_CH4[is.na(e_CH4)]     <- 0
e_CO2   <- as.numeric(eb["CO2_bd",     ]); e_CO2[is.na(e_CO2)]     <- 0
e_N2O   <- as.numeric(eb["N2O_bd",     ]); e_N2O[is.na(e_N2O)]     <- 0
e_terr  <- e_land + e_CH4 + e_CO2 + e_N2O

# Total demand across all Y columns (all consumption categories, all countries)
y_total <- rowSums(Y)


# ── Core function: demand-side supply chain BD footprint ──────────────────────
# Computes (fw_bd + terr_bd) / 2 for the global demand for a set of comm_codes.
# Uses the vector form E^T * L * y_X to avoid computing the full L*Y matrix.
total_bd_demand <- function(comm_codes) {
  # Rows in Y belonging to these commodities
  row_idx <- grep(paste0("^(", paste(comm_codes, collapse = "|"), ")_"),
                  row_names)

  # Demand vector: total final demand for these sectors, zeros elsewhere
  y_X        <- numeric(length(y_total))
  y_X[row_idx] <- y_total[row_idx]

  # Total output of every sector needed to satisfy this demand (full supply chain)
  output_X <- as.numeric(L %*% y_X)

  fw_bd   <- sum(output_X * e_fw,   na.rm = TRUE)
  terr_bd <- sum(output_X * e_terr, na.rm = TRUE)

  (fw_bd + terr_bd) / 2
}

# ── Helper for item-name-based lookup (stimulants) ────────────────────────────
total_bd_demand_items <- function(item_names) {
  codes <- items$comm_code[items$item %in% item_names]
  total_bd_demand(codes)
}


# ── Commodity group definitions ────────────────────────────────────────────────

# Stimulants (as per main analysis)
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

# Food comparators — comm_codes from inst/items_full.csv
# c002 = Wheat and products
# c015 = Sugar cane
# c090 = Wine
# c113 = Bovine Meat
codes_wheat      <- "c002"
codes_sugarcane  <- "c015"
codes_wine       <- "c090"
codes_beef       <- "c113"


# ── Compute totals ─────────────────────────────────────────────────────────────
cat("Computing demand-side footprints...\n")

totals_df <- data.frame(
  commodity = c("Coffee", "Cocoa", "Tea", "Tobacco",
                "Wheat", "Sugar cane", "Wine", "Beef"),
  type      = c(rep("Stimulant", 4), rep("Food", 4)),
  total_bd  = c(
    total_bd_demand_items(items_coffee),
    total_bd_demand_items(items_cocoa),
    total_bd_demand_items(items_tea),
    total_bd_demand_items(items_tobacco),
    total_bd_demand(codes_wheat),
    total_bd_demand(codes_sugarcane),
    total_bd_demand(codes_wine),
    total_bd_demand(codes_beef)
  ),
  stringsAsFactors = FALSE
)

cat("Totals (sorted):\n")
print(totals_df[order(totals_df$total_bd), ])


# ── Build number-line plot ─────────────────────────────────────────────────────

scale_width_in  <- 7.0
scale_height_in <- 2.2

totals_strip <- totals_df[order(totals_df$total_bd), ]
totals_strip$rank <- seq_len(nrow(totals_strip))

# Alternating label placement: odd ranks above bar, even ranks below
totals_strip$above   <- totals_strip$rank %% 2 == 1
totals_strip$name_y  <- ifelse(totals_strip$above, 0.90, 0.22)
totals_strip$val_y   <- ifelse(totals_strip$above, 0.75, 0.07)
totals_strip$tick_y0 <- ifelse(totals_strip$above, 0.56, 0.44)
totals_strip$tick_y1 <- ifelse(totals_strip$above, 0.65, 0.28)
totals_strip$name_vj <- ifelse(totals_strip$above, 0, 1)
totals_strip$val_vj  <- ifelse(totals_strip$above, 0, 1)

# Dot fill: heatmap yellow → dark red by BD magnitude
heatmap_pal <- colorRampPalette(c("#FFF5B0", "#FD8D3C", "#BD0026"))(100)
bd_vals     <- totals_strip$total_bd
bd_norm     <- (bd_vals - min(bd_vals)) / (max(bd_vals) - min(bd_vals))
totals_strip$dot_colour <- heatmap_pal[pmax(1L, ceiling(bd_norm * 100))]

# Text colour: stimulants in dark teal, food comparators in charcoal
totals_strip$text_colour <- ifelse(totals_strip$type == "Stimulant",
                                   "#1B6B6B",   # dark teal for stimulants
                                   "grey25")    # charcoal for food

# Text weight: stimulants bold, food plain
totals_strip$text_face <- ifelse(totals_strip$type == "Stimulant", "bold", "plain")

# Log-scale x range
x_lo <- min(bd_vals) / 1.5
x_hi <- max(bd_vals) * 1.5

p_numberline <- ggplot() +
  # background ruler bar
  annotate("rect",
           xmin = x_lo, xmax = x_hi, ymin = 0.44, ymax = 0.56,
           fill = "grey88", colour = NA) +
  # tick from bar to label group
  geom_segment(data = totals_strip,
               aes(x = total_bd, xend = total_bd,
                   y = tick_y0,  yend = tick_y1),
               colour = "grey65", linewidth = 0.3) +
  # dots — filled by BD magnitude
  geom_point(data = totals_strip,
             aes(x = total_bd, y = 0.5),
             fill   = totals_strip$dot_colour,
             shape  = 21, size = 4.5,
             colour = "white", stroke = 0.6) +
  # commodity name — colour + weight encode type
  geom_text(data = totals_strip,
            aes(x        = total_bd,
                y        = name_y,
                label    = commodity,
                vjust    = name_vj,
                colour   = I(text_colour),
                fontface = text_face),
            size = 3.2) +
  # value below name
  geom_text(data = totals_strip,
            aes(x     = total_bd,
                y     = val_y,
                label = formatC(total_bd, format = "e", digits = 1),
                vjust = val_vj),
            size = 2.8, colour = "grey50") +
  scale_x_log10(expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  labs(x = NULL, y = NULL,
       title = "Global biodiversity footprint by commodity group (2020)",
       subtitle = "Stimulant crops in bold teal  ·  Food comparators in grey  ·  Full supply chain demand-side footprint") +
  theme_void() +
  theme(
    plot.title      = element_text(size = 9,  colour = "grey20", hjust = 0.5,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 7.5, colour = "grey45", hjust = 0.5,
                                   margin = margin(b = 6)),
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin     = margin(8, 12, 8, 12)
  )

print(p_numberline)

if (save) {
  out_path <- "X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\bd_impact_numberline.png"
  ggsave(out_path, plot = p_numberline,
         width = scale_width_in, height = scale_height_in,
         dpi = 300, bg = "white")
  cat("Saved:", out_path, "\n")
}
