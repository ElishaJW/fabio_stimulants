
# GLOBAL PRODUCTION VOLUMES OVER TIME - STIMULANT COMMODITY GROUPS
# Stacked area (wedge) chart: coffee, cocoa, tea, tobacco (2000-2021)
# Eli Wilson - elisha.wilson@ntnu.no

# Data source: FAOSTAT production statistics (crop_tidy.rds, crop_full.rds)
# All values in thousand tonnes (kt) of primary commodity.
#
# Item codes used:
#   Coffee  - item 1610 "Coffee, green"           (crop_tidy)
#   Cocoa   - item 661  "Cocoa beans"              (crop_full)
#   Tea     - item 667  "Tea leaves" / 4.3         (crop_full; fresh-leaf -> made-tea)
#           + item 1630 "Mate leaves"              (crop_tidy; already in dry-leaf kt)
#   Tobacco - item 826  "Unmanufactured tobacco"   (crop_tidy)
#
# Tea fresh-leaf conversion: FAOSTAT reports tea production in fresh green leaf
# weight. Dividing by 4.3 converts to made-tea equivalent (the standard used for
# all other commodities). Mate is already reported in dry-leaf weight and is added
# directly to the tea group without conversion.

library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)

setwd("C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants")

# ---- Load FAOSTAT production statistics -------------------------------------

crop_tidy <- as.data.table(readRDS("data/tidy/crop_tidy.rds"))
crop_full  <- as.data.table(readRDS("data/tidy/crop_full.rds"))

# Helper: aggregate global production to kt for one item
global_prod <- function(dt, ic, grp, scale = 1) {
  dt[item_code == ic & element == "Production" & year >= 2000,
     .(commodity_group = grp,
       production_kt   = sum(value, na.rm = TRUE) / 1000 / scale),
     by = year]
}


# ---- Build each commodity group ----------------------------------------------

coffee <- global_prod(crop_tidy, 1610, "Coffee")
cocoa  <- global_prod(crop_full,   661, "Cocoa")

# Tea = fresh-leaf / 4.3 (made-tea equivalent) + Mate (dry leaf, no conversion)
tea_leaves <- global_prod(crop_full,  667, "Tea", scale = 4.3)
mate       <- global_prod(crop_tidy, 1630, "Tea")
tea        <- rbind(tea_leaves, mate)[, .(production_kt = sum(production_kt)), by = .(year, commodity_group)]

tobacco <- global_prod(crop_tidy, 826, "Tobacco")


# ---- Combine -----------------------------------------------------------------

prod <- rbindlist(list(coffee, cocoa, tea, tobacco))


# ---- Summary check -----------------------------------------------------------

cat("Global production summary (kt):\n")
print(dcast(prod, year ~ commodity_group, value.var = "production_kt")[,
            lapply(.SD, round), .SDcols = c("Coffee", "Cocoa", "Tea", "Tobacco")])


# ---- Factor order for stacking (bottom to top) -------------------------------

prod[, commodity_group := factor(
  commodity_group,
  levels = c("Tobacco", "Cocoa", "Coffee", "Tea")
)]


# ---- Plot --------------------------------------------------------------------

# group_colours <- c(
#   "Tea"     = "#4E8B3A",   # deep green
#   "Coffee"  = "#8B5E3C",   # warm brown
#   "Cocoa"   = "#5C3317",   # dark chocolate
#   "Tobacco" = "#C4A35A"    # golden straw
# )

group_colours <- c(
  "Coffee"  = "#A6761D",   # warm brown
  "Cocoa"   = "#D95F02",   # burnt orange
  "Tea"     = "#1B9E77",   # teal-green
  "Tobacco" = "#666666"    # neutral grey
)

p <- ggplot(prod, aes(x = year, y = production_kt, fill = commodity_group)) +
  geom_area(alpha = 0.9, colour = "white", linewidth = 0.3) +
  scale_fill_manual(
    values = group_colours,
    guide  = guide_legend(reverse = FALSE)
  ) +
  scale_x_continuous(
    breaks = seq(2000, 2021, by = 4),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    labels = label_comma(suffix = " kt"),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Global Production of Stimulant Commodities (2000\u20132021)",
    subtitle = "Source: FAOSTAT production statistics; tea = made-tea equivalent (fresh leaf \u00f7 4.3) + mat\u00e9",
    x        = NULL,
    y        = "Production (thousand tonnes)",
    fill     = "Commodity group"
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

print(p)

ggsave("figures/global_production_wedge.png",
       plot = p, width = 9, height = 5.5, dpi = 300, bg = "white")

cat("\nFigure saved to figures/global_production_wedge.png\n")
