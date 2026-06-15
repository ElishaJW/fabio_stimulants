
# BIODIVERSITY IMPACT CONTRIBUTIONS BY STIMULANT COMMODITY GROUP
# Stacked bar chart: 4 bars (commodity groups), stacked segments = impact categories.
# All segments share the same unit (PDF·yr), so stacking is valid.
# Bar height = total biodiversity impact for that commodity (sum across all impact categories).
#
# Data: FABIO E_bd_2020.rds (_bd rows only) × output vector X_2020
# Impact categories (all in PDF·yr):
#   Land use  → landuse_bd
#   Water use → water_bd
#   N & P     → N_bd + P_bd
#   GHG       → CH4_bd + CO2_bd + N2O_bd
#
# Eli Wilson - elisha.wilson@ntnu.no

library(data.table)
library(ggplot2)
library(scales)

setwd("C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants")

# ── Load data ──────────────────────────────────────────────────────────────────

eb    <- readRDS("data/E/E_bd_2020.rds")
items <- fread("inst/items_full.csv")

# X: 26112 × 12 output matrix; row ORDER matches E column order 
X     <- readRDS("data/IO tables/X.rds")
x2020 <- X[, "2020"]

# ── Commodity group item codes ─────────────────────────────────────────────────

GROUPS <- list(
  Coffee  = c(1610, 23911, 2391202, 2391201),
  Cocoa   = c(1640, 23620, 3915002, 2361001, 665, 666),
  Tea     = c(1620, 1630, 23914),
  Tobacco = c(826, 829, 831, 828)
)

# ── Aggregate E_bd × X across all country-columns for each commodity group ─────

results <- list()

for (grp in names(GROUPS)) {
  codes     <- GROUPS[[grp]]
  grp_codes <- items[item_code %in% codes, comm_code]
  grp_vec   <- setNames(rep(0, nrow(eb)), rownames(eb))

  for (cc in grp_codes) {
    col_idx <- grep(paste0("_", cc, "$"), colnames(eb))
    if (length(col_idx) > 0) {
      grp_vec <- grp_vec + rowSums(
        t(t(eb[, col_idx, drop = FALSE]) * x2020[col_idx])
      )
    }
  }

  results[[grp]] <- data.table(
    group    = grp,
    stressor = names(grp_vec),
    value    = grp_vec
  )
}

dat <- rbindlist(results)

# ── Map _bd rows to 4 impact categories (all in PDF·yr) ───────────────────────

dat[, category := fcase(
  stressor == "landuse_bd",                   "Land use",
  stressor == "water_bd",                     "Water use",
  stressor %in% c("N_bd", "P_bd"),            "N & P application",
  stressor %in% c("CH4_bd", "CO2_bd", "N2O_bd"), "GHG emissions",
  default = NA_character_
)]

agg <- dat[!is.na(category),
           .(total = sum(value, na.rm = TRUE)),
           by = .(group, category)]

cat("\nBiodiversity impact totals (PDF\u00b7yr) by group \u00d7 category:\n")
print(dcast(agg, category ~ group, value.var = "total",
            fun.aggregate = function(x) formatC(sum(x), format = "e", digits = 3)))

# ── Scale to ×10⁻³ PDF·yr for readability ─────────────────────────────────────

agg[, display_val := total * 1e3]

# ── Factor orders ──────────────────────────────────────────────────────────────

# Commodity order (x-axis)
agg[, group := factor(group, levels = c("Coffee", "Cocoa", "Tea", "Tobacco"))]

# Impact category stacking order: largest (land use) at bottom for visual stability
# geom_bar FIRST factor level → BOTTOM
agg[, category := factor(category, levels = c(
  "Land use", "N & P application", "Water use", "GHG emissions"
))]

# ── Colour palette for impact categories ───────────────────────────────────────
# Intuitive: land = brown-green, N&P = amber, water = blue, GHG = red

category_colours <- c(
  "Land use"          = "#5E813F",   # earthy green
  "N & P application" = "#E8A838",   # amber (fertilisers)
  "Water use"         = "#4A90C4",   # blue (water)
  "GHG emissions"     = "#C0392B"    # red (heat/emissions)
)

# ── Publication-quality plot ───────────────────────────────────────────────────

p <- ggplot(agg, aes(x = group, y = display_val, fill = category)) +
  geom_bar(stat = "identity", width = 0.58, colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = ifelse(display_val / ave(display_val, group, FUN = sum) >= 0.05,
                       sprintf("%.2f", display_val), "")),
    position  = position_stack(vjust = 0.5),
    colour    = "white",
    size      = 3.1,
    fontface  = "bold",
    family    = "sans"
  ) +
  scale_fill_manual(
    values = category_colours,
    guide  = guide_legend(reverse = TRUE)   # top legend entry = top stack
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.04)),
    labels = label_number(accuracy = 0.1)
  ) +
  labs(
    x       = NULL,
    y       = expression("Summed Biodiversity impact (" * 10^{-3} * " PDF" %.% "yr)"),
    fill    = "Impact category",
  ) +
  theme_classic(base_size = 10) +
  theme(
    # Axes
    axis.line         = element_line(colour = "grey20", linewidth = 0.35),
    axis.ticks        = element_line(colour = "grey20", linewidth = 0.35),
    axis.ticks.length = unit(3, "pt"),
    axis.text.x       = element_text(colour = "grey10", size = 10),
    axis.text.y       = element_text(colour = "grey10", size = 9),
    axis.title.y      = element_text(colour = "grey10", size = 9.5,
                                     margin = margin(r = 6)),
    # Grid
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    # Legend
    legend.position    = "right",
    legend.title       = element_text(size = 9.5, colour = "grey10", face = "bold"),
    legend.text        = element_text(size = 9,   colour = "grey10"),
    legend.key.size    = unit(0.42, "cm"),
    legend.key.spacing.y = unit(2, "pt"),
    legend.margin      = margin(l = 4),
    # Caption
    plot.caption       = element_text(colour = "grey50", size = 7.5,
                                      hjust = 0, margin = margin(t = 6)),
    plot.margin        = margin(8, 6, 4, 6)
  )

ggsave("figures/biodiv_contributions.png",
       plot = p, width = 6.5, height = 4.0, dpi = 600, bg = "white")
cat("\nSaved: figures/biodiv_contributions.png\n")
