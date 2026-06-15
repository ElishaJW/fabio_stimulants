
# STRESSOR CONTRIBUTIONS BY STIMULANT COMMODITY GROUP
# 100% stacked bar chart: 4 bars (stressor categories), 4 fill segments (commodity groups)
# Each bar sums to 100 %; segment height = group's share of total stimulant stressor.
#
# Data: FABIO extension matrix E_bd_2020.rds (most recent year available: 2020)
# Stressors used:
#   Land use        → landuse          (ha)
#   Water use       → blue             (m³, blue water)
#   N & P appl.     → n_application + p_application  (kg)
#   Carbon emissions→ CH4 + CO2 + N2O  (kg CO₂-eq)
#
# Commodity group items (all FABIO items; primary + processed):
#   Coffee  : 1610 (green), 23911 (roasted), 2391202 (extracts), 2391201 (substitutes)
#   Cocoa   : 1640 (beans), 23620 (butter), 3915002 (husks), 2361001 (paste),
#             665 (powder & cake), 666 (chocolate)
#   Tea     : 1620 (tea leaves), 1630 (mate leaves), 23914 (extracts/concentrates)
#   Tobacco : 826 (unmanufactured), 829 (cigars), 831 (other manufactured), 828 (cigarettes)
#
# Eli Wilson - elisha.wilson@ntnu.no

library(data.table)
library(ggplot2)
library(scales)

setwd("C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants")

# ── Load data ──────────────────────────────────────────────────────────────────

eb    <- readRDS("data/E/E_bd_2020.rds")
items <- fread("inst/items_full.csv")

# X: 26112 × 12 output matrix (tonnes per country-item × year).
# Rownames are comm_codes (c001, c002, ...) without area prefix, but the row
# ORDER matches the column order of E exactly (both indexed by region × item).
X     <- readRDS("data/IO tables/X.rds")
x2020 <- X[, "2020"]   # length-26112 vector; position i matches E column i

cat("Stressor rows in E_bd_2020:\n")
cat(paste(rownames(eb), collapse = "\n"), "\n\n")

# ── Commodity group item codes ─────────────────────────────────────────────────

GROUPS <- list(
  Coffee  = c(1610, 23911, 2391202, 2391201),
  Cocoa   = c(1640, 23620, 3915002, 2361001, 665, 666),
  Tea     = c(1620, 1630, 23914),
  Tobacco = c(826, 829, 831, 828)
)

# ── Aggregate E across all country-columns for each commodity group ────────────

results <- list()

for (grp in names(GROUPS)) {
  codes     <- GROUPS[[grp]]
  grp_codes <- items[item_code %in% codes, comm_code]

  cat("Group:", grp, "| comm_codes:", paste(grp_codes, collapse = ", "), "\n")

  grp_vec <- setNames(rep(0, nrow(eb)), rownames(eb))

  for (cc in grp_codes) {
    # Columns follow the pattern "{area_code}_{cc}" (e.g., "10_c042")
    col_idx <- grep(paste0("_", cc, "$"), colnames(eb))
    if (length(col_idx) > 0) {
      # Multiply intensity (stressor/tonne) by output (tonnes) for each country-item,
      # using positional indexing (X rows and E columns are in the same order).
      # t(t(M) * v) scales each column of M by the corresponding element of v.
      grp_vec <- grp_vec + rowSums(
        t(t(eb[, col_idx, drop = FALSE]) * x2020[col_idx])
      )
    } else {
      cat("  WARNING: no columns for", cc, "\n")
    }
  }

  results[[grp]] <- data.table(
    group    = grp,
    stressor = names(grp_vec),
    value    = grp_vec
  )
}

dat <- rbindlist(results)

# ── Diagnostic: raw global totals by stressor ──────────────────────────────────

cat("\nGlobal stimulant stressor totals (sum across all groups):\n")
print(dat[, .(total = sum(value)), by = stressor][order(-abs(total))])

# ── Map individual stressor rows to the 4 chart categories ────────────────────

dat[, category := fcase(
  stressor == "landuse",                              "Land use",
  stressor == "blue",                                 "Water use",
  stressor %in% c("n_application", "p_application"), "N & P application",
  stressor %in% c("CH4", "CO2", "N2O"),              "Carbon emissions",
  default = NA_character_
)]

agg <- dat[!is.na(category),
           .(total = sum(value, na.rm = TRUE)),
           by = .(group, category)]

# Relative contribution within each stressor category (rows sum to 100%)
agg[, pct := total / sum(total) * 100, by = category]

cat("\nPercentage breakdown (rows = stressor categories):\n")
print(dcast(agg, category ~ group, value.var = "pct",
            fun.aggregate = function(x) round(sum(x), 1)))

# ── Factor orders ──────────────────────────────────────────────────────────────

# Stressor categories (x-axis order)
agg[, category := factor(category, levels = c(
  "Land use", "Water use", "N & P application", "Carbon emissions"
))]

# Group stacking order: Coffee at bottom, Tobacco at top.
# In geom_bar(position="stack"), FIRST factor level → BOTTOM, LAST → TOP.
agg[, group := factor(group, levels = c("Coffee", "Cocoa", "Tea", "Tobacco"))]

# ── Colour palette ─────────────────────────────────────────────────────────────
# RColorBrewer Dark2 (4 colours): colorblind-safe and designed for print.

group_colours <- c(
  "Coffee"  = "#A6761D",   # warm brown
  "Cocoa"   = "#D95F02",   # burnt orange
  "Tea"     = "#1B9E77",   # teal-green
  "Tobacco" = "#666666"    # neutral grey
)

# ── X-axis labels with line breaks ─────────────────────────────────────────────

axis_labels <- c(
  "Land use"          = "Land\nuse",
  "Water use"         = "Water\nuse",
  "N & P application" = "N & P\napplication",
  "GHG emissions"  = "GHG\nemissions"
)

# ── Publication-quality plot ───────────────────────────────────────────────────

p <- ggplot(agg, aes(x = category, y = pct, fill = group)) +
  geom_bar(stat = "identity", width = 0.58, colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = ifelse(pct >= 5, sprintf("%.1f%%", pct), "")),
    position  = position_stack(vjust = 0.5),
    colour    = "white",
    size      = 3.1,
    fontface  = "bold",
    family    = "sans"
  ) +
  scale_fill_manual(
    values = group_colours,
    guide  = guide_legend(reverse = TRUE)   # matches stacking: top bar = top legend
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.005)),
    limits = c(0, 101),
    breaks = seq(0, 100, 25)
  ) +
  scale_x_discrete(labels = axis_labels) +
  labs(
    x       = NULL,
    y       = "Share of global stressor emission (%)",
    fill    = NULL,
  ) +
  theme_classic(base_size = 10) +
  theme(
    # Axes
    axis.line          = element_line(colour = "grey20", linewidth = 0.35),
    axis.ticks         = element_line(colour = "grey20", linewidth = 0.35),
    axis.ticks.length  = unit(3, "pt"),
    axis.text.x        = element_text(colour = "grey10", size = 9.5, lineheight = 1.1),
    axis.text.y        = element_text(colour = "grey10", size = 9),
    axis.title.y       = element_text(colour = "grey10", size = 9.5,
                                      margin = margin(r = 6)),
    # Horizontal guide lines only
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.3),
    panel.grid.minor   = element_blank(),
    # Legend
    legend.position    = "right",
    legend.text        = element_text(size = 9.5, colour = "grey10"),
    legend.key.size    = unit(0.42, "cm"),
    legend.key.spacing.y = unit(2, "pt"),
    legend.margin      = margin(l = 4),
    # Caption (data source note)
    plot.caption       = element_text(colour = "grey50", size = 7.5,
                                      hjust = 0, margin = margin(t = 6)),
    plot.margin        = margin(8, 6, 4, 6)
  )

ggsave("figures/stressor_contributions.png",
       plot = p, width = 6.5, height = 3.8, dpi = 600, bg = "white")
cat("\nSaved: figures/stressor_contributions.png\n")
