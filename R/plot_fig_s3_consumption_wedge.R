
# CONSUMPTION WEDGE CHARTS – STIMULANT COMMODITY GROUPS
# Stacked area charts: domestic consumption by country (2000–2021)
# Eli Wilson - elisha.wilson@ntnu.no

# Data source: FAOSTAT CBS aggregate items (cbs_tidy.rds)
#   2630  Coffee and products  → consumption column: food
#   2633  Cocoa and products   → consumption column: food
#   2635  Tea including mate   → consumption column: food (made-tea equivalent)
#                                with corrections for known fresh-leaf artefacts
#   2671  Tobacco              → consumption column: other
#
# All values are in primary-commodity equivalent (kt).
#
# Tea data quality notes (item 2635):
#   Several major producers switched FAOSTAT production reporting from made-tea
#   to fresh green leaf weight at different times. The CBS absorbed this inflated
#   production into "processing", driving the food column to zero for those
#   countries in those years. Two corrections are applied:
#
#   1. China, mainland 2014+: production ÷ 4.3 (fresh-leaf → made-tea).
#      2020 additionally has a 100× transcription error: ÷ 100 × ÷ 4.3.
#      For these years, domestic availability = (production_corrected + imports
#      − exports) replaces the zero food column.
#
#   2. India 2020+: production ÷ 4.3, same recalculation.
#
#   All other countries use the CBS food column as supplied.

library(data.table)
library(ggplot2)
library(scales)

setwd("C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants")

# ── Load data ──────────────────────────────────────────────────────────────────

cbs <- as.data.table(readRDS("data/tidy/cbs_tidy.rds"))
cbs <- cbs[year >= 2000]

# ── Configuration ──────────────────────────────────────────────────────────────

# Each entry: CBS aggregate item code, display label, consumption column, output file
ITEMS <- list(
  list(item_code = 2630L, label = "Coffee",  consum_col = "food",  file = "coffee_consumption_wedge.png"),
  list(item_code = 2633L, label = "Cocoa",   consum_col = "food",  file = "cocoa_consumption_wedge.png"),
  list(item_code = 2635L, label = "Tea",     consum_col = "food",  file = "tea_consumption_wedge.png"),
  list(item_code = 2671L, label = "Tobacco", consum_col = "other", file = "tobacco_consumption_wedge.png")
)

N_TOP <- 10  # number of countries labelled individually

# 10 visually distinct colours – ColorBrewer Paired
COL_TOP <- c(
  "#1F78B4", "#E31A1C", "#33A02C", "#FF7F00", "#6A3D9A",
  "#B15928", "#A6CEE3", "#FB9A99", "#B2DF8A", "#FDBF6F"
)
COL_ROW <- "#BBBBBB"   # grey for Rest of World

# ── Loop over commodity groups ─────────────────────────────────────────────────

for (g in ITEMS) {

  ic    <- g$item_code
  label <- g$label
  ccol  <- g$consum_col

  dat <- cbs[item_code == ic]
  if (nrow(dat) == 0) { cat("No data for", label, "– skipping.\n"); next }

  # ── Tea-specific corrections ───────────────────────────────────────────────
  if (ic == 2635L) {
    # China 2014+: fresh-leaf reporting (÷4.3); 2020 also has ×100 typo (÷100)
    dat[area == "China, mainland" & year >= 2014L & year != 2020L,
        production := production / 4.3]
    dat[area == "China, mainland" & year == 2020L,
        production := production / (100 * 4.3)]
    # India 2020+: fresh-leaf reporting (÷4.3)
    dat[area == "India" & year >= 2020L,
        production := production / 4.3]
    # Recompute total_supply for corrected rows
    dat[(area == "China, mainland" & year >= 2014L) | (area == "India" & year >= 2020L),
        total_supply := production + imports]
    # For corrected countries: use (total_supply_corrected − exports) as domestic
    # availability; for all others use the food column (already in made-tea kt)
    dat[, consum_kt := pmax(0,
      fifelse(
        (area == "China, mainland" & year >= 2014L) | (area == "India" & year >= 2020L),
        (total_supply - exports) / 1e3,
        food / 1e3
      ),
      na.rm = TRUE
    )]
  } else {
    # Consumption in kt (treat NA as zero)
    dat[, consum_kt := pmax(0, get(ccol) / 1e3, na.rm = TRUE)]
  }

  # Diagnostic: global totals by year
  cat(sprintf("── %s (item %d, col '%s') global consumption (kt) ──\n", label, ic, ccol))
  print(dat[, .(kt = round(sum(consum_kt))), by = year][order(year)])

  # Top N consumers (mean across 2000–2021)
  avg        <- dat[, .(avg_kt = mean(consum_kt, na.rm = TRUE)), by = area][order(-avg_kt)]
  top_ctries <- avg[seq_len(min(N_TOP, .N)), area]

  cat("\nTop", N_TOP, "consumers:\n")
  print(avg[seq_len(min(N_TOP, .N))])
  cat("\n")

  # Label and aggregate
  dat[, country := fifelse(area %in% top_ctries, area, "Rest of World")]
  agg <- dat[, .(consum_kt = sum(consum_kt, na.rm = TRUE)), by = .(year, country)]

  # Factor order: first level → top of geom_area stack (ggplot2 3.5 behaviour,
  # empirically verified); top consumer first, Rest of World last (bottom)
  country_levels <- c(top_ctries, "Rest of World")
  agg[, country := factor(country, levels = country_levels)]

  # Named colour palette
  pal <- c(
    setNames(COL_TOP[seq_along(top_ctries)], top_ctries),
    "Rest of World" = COL_ROW
  )

  # ── Plot ────────────────────────────────────────────────────────────────────

  p <- ggplot(agg, aes(x = year, y = consum_kt, fill = country)) +
    geom_area(alpha = 0.9, colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = pal, guide = guide_legend(reverse = FALSE)) +
    scale_x_continuous(breaks = seq(2000, 2021, by = 4), expand = c(0, 0)) +
    scale_y_continuous(labels = label_comma(suffix = " kt"), expand = c(0, 0)) +
    labs(
      title    = paste0("Domestic Consumption of ", label, " by Country (2000\u20132021)"),
      subtitle = paste0("Source: FAOSTAT CBS; primary-commodity equivalent",
                        if (ic == 2635L) "; China & India fresh-leaf corrected (\u00f74.3)" else ""),
      x        = NULL,
      y        = "Consumption (thousand tonnes)",
      fill     = "Country"
    ) +
    theme_classic(base_size = 13) +
    theme(
      legend.position    = "right",
      legend.title       = element_text(face = "bold"),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(colour = "grey40", size = 10),
      axis.line          = element_line(colour = "grey40"),
      axis.ticks         = element_line(colour = "grey40"),
      panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.4)
    )

  fname <- paste0("figures/", g$file)
  ggsave(fname, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  cat("Saved:", fname, "\n\n")
}

cat("Done.\n")
