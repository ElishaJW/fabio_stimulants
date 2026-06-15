
# BIODIVERSITY FOOTPRINT BAR CHARTS BY CONSUMING COUNTRY — STIMULANT COMMODITIES
# V2.0 — uses E_bd_2020.rds (CFs already applied as per-tonne intensities),
#         skipping raw CF loading & multiplication done in analysis_footprints_by_country.R.
#         Adds climate change (GHG: CH4, CO2, N2O) impacts.
#         Fixes: bd_fp_N was missing from bd_fp_total sum (bd_fp_water was counted twice).
#         Fixes: contribution_n used bd_fp_water instead of bd_fp_N.
#
#
# NOTE: The per-capita section (bottom) requires a 'world_bank_pop' object.
#       Load it separately before running that section.
#
# Eli Wilson - elisha.wilson@ntnu.no


# Load the libraries
library(tidyverse)
library(data.table)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(colorspace)
library(dichromat)
library(scales)
library(ggpattern)
library(svglite)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')


# ── Load data ──────────────────────────────────────────────────────────────────
# E_bd has per-tonne biodiversity intensity rows (_bd suffix), CFs already applied.
# Path: data/E/E_bd_2020.rds  (change here if using a different file e.g. "E.rds")
eb      <- readRDS("data/E/E_bd_2020.rds")
items   <- fread("inst/items_full.csv")
regions <- fread("inst/regions_fabio.csv")

items_stim <- items[items$comm_group == "Coffee, tea, cocoa" |
                    items$comm_group == "Tobacco, rubber" & items$comm_code != 'c060']$comm_code

# Identify all stimulant commodities
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


# NOTE: CH4_bd/CO2_bd label swap was fixed at the source in 17_biodiv_calculations.R.
# No correction needed here after regenerating E_bd files.


# ── Load IO tables ─────────────────────────────────────────────────────────────
year  <- 2020
Y_all <- readRDS("data/IO tables/Y.rds")
Y <- Y_all[[as.character(year)]]
L <- readRDS(paste0("data/IO tables/", year, "_L_mass.rds"))

# Compute L × Y (total output of each sector allocated to each final demand)
cat("Computing L x Y for", year, "...\n")
Ly <- L %*% Y

# World map with country info
world_blank <- ne_countries(scale = "medium", returnclass = "sf")


# ── Build row names for the full 26112-row footprint matrix ───────────────────
item_country_combinations <- expand.grid(items$comm_code, regions$area_code)
row_names <- paste(item_country_combinations$Var1, item_country_combinations$Var2, sep = "_")


# ── Compute per-stressor biodiversity footprints ───────────────────────────────
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

bd_fp_total <- (bd_fp_fw + bd_fp_terrestrial) / 2

# Choose from individual impact categories or the combined form
bd_fp_for_chosen_impact_category <- bd_fp_total


# CHOOSE ONE OF THE FOLLOWING: -------------------------------------------------
# 1) SUM ACROSS CONSUMPTION CATEGORIES FOR EACH COUNTRY-------------------------
country_code <- regions$area_code
# Now sum across consumption categories for each country
bd_fp_total_by_country_filtered <- bd_fp_for_chosen_impact_category
# Initialize an empty matrix to store the results
bd_fp_total_by_country <- matrix(0, nrow = nrow(bd_fp_total_by_country_filtered),
                                 ncol = length(unique(sub("_.*", "", colnames(bd_fp_total_by_country_filtered)))))
# Set the row and column names for the result matrix
rownames(bd_fp_total_by_country) <- row_names
colnames(bd_fp_total_by_country) <- unique(sub("_.*", "", colnames(bd_fp_total_by_country_filtered)))
# Loop over each unique country code and sum across the consumption categories
for (country in colnames(bd_fp_total_by_country)) {
  country_columns <- grep(paste0("^", country, "_"), colnames(bd_fp_total_by_country_filtered))
  bd_fp_total_by_country[, country] <- rowSums(bd_fp_total_by_country_filtered[, country_columns], na.rm = TRUE)
}
# Convert to df
bd_fp_total_by_country <- as.data.frame(bd_fp_total_by_country)


# 2) OR SUM ACROSS CONSUMPTION CATEGORIES FOR EACH COUNTRY EXCEPT 'OTHER'-------
bd_fp_total_by_country_ex_other_filtered <- bd_fp_for_chosen_impact_category[, !grepl("other", colnames(bd_fp_for_chosen_impact_category))]
bd_fp_total_by_country_ex_other <- matrix(0, nrow = nrow(bd_fp_total_by_country_ex_other_filtered),
                                          ncol = length(unique(sub("_.*", "", colnames(bd_fp_total_by_country_ex_other_filtered)))))
rownames(bd_fp_total_by_country_ex_other) <- row_names
colnames(bd_fp_total_by_country_ex_other) <- unique(sub("_.*", "", colnames(bd_fp_total_by_country_ex_other_filtered)))
for (country in colnames(bd_fp_total_by_country_ex_other)) {
  country_columns <- grep(paste0("^", country, "_"), colnames(bd_fp_total_by_country_ex_other_filtered))
  bd_fp_total_by_country_ex_other[, country] <- rowSums(bd_fp_total_by_country_ex_other_filtered[, country_columns], na.rm = TRUE)
}
bd_fp_total_by_country_ex_other <- as.data.frame(bd_fp_total_by_country_ex_other)


# 3) OR JUST TAKE THE FOOD CONSUMPTION CATEGORY---------------------------------
bd_fp_food_by_country <- bd_fp_for_chosen_impact_category
bd_fp_food_by_country <- bd_fp_food_by_country[,grep("food", colnames(bd_fp_food_by_country))]
colnames(bd_fp_food_by_country) <- unique(country_code)
rownames(bd_fp_food_by_country) <- row_names
bd_fp_food_by_country <- as.data.frame(bd_fp_food_by_country)


# CHOOSE CONSUMPTION CATEGORIES HERE:-------------------------------------------
# Coffee, tea, cocoa: food consumption category only.
# Tobacco: all consumption categories (it is not a food product, so restricting
# to "food" would undercount its impacts). Matches the convention used in
# plot_fig_1_footprints_regional.R.
items_tobac_codes <- items[comm_group == "Tobacco, rubber" & comm_code != "c060"]$comm_code

bd_fp_for_chosen_cons_category <- bd_fp_food_by_country
tobac_rows <- sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items_tobac_codes
# Align by column name in case the two matrices' column orders differ
bd_fp_for_chosen_cons_category[tobac_rows, ] <-
  bd_fp_total_by_country[tobac_rows, colnames(bd_fp_for_chosen_cons_category)]


# SORT ACCORDING TO COMMODITIES-------------------------------------------------
bd_fp_total_by_country_all_stim <- bd_fp_for_chosen_cons_category[sub("_.*", "", rownames(bd_fp_for_chosen_cons_category)) %in% items_stim, ]


# HERE WE CHOOSE COMMODITY TO PLOT:---------------------------------------------
commodities_to_assess <- bd_fp_total_by_country_all_stim


# Sum the footprints across all rows for each consuming country-----------------
top_10_consumers <- as.data.frame(colSums(commodities_to_assess, na.rm = TRUE))
colnames(top_10_consumers) <- "total_footprint"
top_10_consumers <- top_10_consumers %>%
  arrange(desc(total_footprint)) %>%
  head(25)
# Calculate the share of production countries and commodities for the top 10 consumers
top_10_consumers_shares <- commodities_to_assess[, colnames(commodities_to_assess) %in% rownames(top_10_consumers)]
# Convert to long format
top_10_consumers_shares <- top_10_consumers_shares %>%
  rownames_to_column("prod_commodity") %>%
  pivot_longer(cols = -prod_commodity, names_to = "consuming_country", values_to = "footprint")
# Separate producer and commodity
top_10_consumers_shares <- top_10_consumers_shares %>%
  separate(prod_commodity, into = c("commodity", "producing_country"), sep = "_", remove = TRUE)
# Convert country codes to integers for later join
top_10_consumers_shares$consuming_country <- as.integer(top_10_consumers_shares$consuming_country)
top_10_consumers_shares$producing_country <- as.integer(top_10_consumers_shares$producing_country)
# Replace consuming country codes with names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = area) %>%
  dplyr::select(-c(consuming_country, iso3c, continent))
# Replace producing country codes with names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(regions, by = c("producing_country" = "area_code")) %>%
  rename(producer = area) %>%
  dplyr::select(-c(producing_country, iso3c, continent))
# Replace commodity codes with commodity names
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(items, by = c("commodity" = "comm_code")) %>%
  rename(commodity_name = item) %>%
  dplyr::select(-c(commodity, item_code, unit, group, moisture, feedtype, comm_group))
# Arrange in descending order for visualization
top_10_consumers_shares <- top_10_consumers_shares %>%
  arrange(desc(footprint))
# Change names to iso3
top_10_consumers_shares <- top_10_consumers_shares %>%
  left_join(regions, by = c("consumer" = "area")) %>%
  dplyr::select(-c(consumer, area_code, continent)) %>%
  rename(consumer = iso3c) %>%
  left_join(regions, by = c("producer" = "area")) %>%
  dplyr::select(-c(producer, area_code, continent)) %>%
  rename(producer = iso3c)
# Summarise the footprint contributions
top_10_consumers_shares <- top_10_consumers_shares %>%
  group_by(consumer, commodity_name, producer) %>%
  summarise(total_footprint = sum(footprint, na.rm = TRUE), .groups = 'drop')
# Identify top contributors and group the rest as 'Other'
top_10_consumers_shares <- top_10_consumers_shares %>%
  group_by(consumer) %>%
  mutate(rank = rank(-total_footprint, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    prod_commodity_name = ifelse(rank <= 2, paste(producer, commodity_name, sep = "_"), "other")
  ) %>%
  group_by(consumer, prod_commodity_name) %>%
  summarise(total_footprint = sum(total_footprint, na.rm = TRUE), .groups = 'drop')
top_10_consumers_shares$prod_commodity_name <- sub("_", " ", top_10_consumers_shares$prod_commodity_name)

# Create a color palette for each commodity group
commodity_palettes <- list(
  "Coffee" = colorRampPalette(brewer.pal(9, "Reds")[2:9]),
  "Cocoa" = colorRampPalette(brewer.pal(9, "Greens")[2:9]),
  "Tea" = colorRampPalette(brewer.pal(9, "Blues")[2:9]),
  "Tobacco" = colorRampPalette(brewer.pal(7, "Purples")[3:7]),
  "other" = function(n) rep("#d3d3d3", n)
)

# Simplify commodity names
simplified_names <- c(
  "Unmanufactured tobacco" = "Tobacco",
  "Cocoa beans" = "Cocoa",
  "Coffee, green" = "Coffee",
  "Tea leaves" = "Tea"
)

# Simplify and group data
top_10_consumers_shares <- top_10_consumers_shares %>%
  mutate(
    producer = sub(" .*", "", prod_commodity_name),
    commodity_name = sub("^[A-Z]{3} ", "", prod_commodity_name),
    simplified_name = recode(commodity_name, !!!simplified_names),
    legend_label = paste(simplified_name, producer, sep = " "),
    legend_group = factor(simplified_name, levels = c("Coffee", "Cocoa", "Tea", "Tobacco", "other"))
  ) %>%
  arrange(legend_group) %>%
  mutate(legend_label = factor(legend_label, levels = unique(legend_label)))

# Change 'other other' to 'Other'
top_10_consumers_shares <- top_10_consumers_shares %>%
  mutate(legend_label = recode(legend_label, "other other" = "Other"))

# Generate colors for legend labels
prod_commodity_colors <- setNames(
  unlist(lapply(levels(top_10_consumers_shares$legend_group), function(group) {
    group_rows <- top_10_consumers_shares %>% filter(legend_group == group)
    n <- n_distinct(group_rows$legend_label)
    commodity_palettes[[group]](n)
  })),
  levels(top_10_consumers_shares$legend_label)
)

# Per-label pattern assignment: cycle through 6 distinct pattern+angle combos
# within each commodity group so same-hue bars stay distinguishable.
# "other" always gets "none" (no pattern needed for the residual category).
.PATTERN_OPTS <- c("stripe", "stripe", "stripe", "stripe", "crosshatch", "circle")
.ANGLE_OPTS   <- c(45, 135, 0, 90, 45, 45)

make_label_patterns <- function(df) {
  label_meta <- df %>%
    distinct(legend_label, legend_group) %>%
    arrange(match(as.character(legend_label),
                  levels(df$legend_label))) %>%
    group_by(legend_group) %>%
    mutate(
      grp_idx   = row_number(),
      opt_idx   = ((grp_idx - 1L) %% length(.PATTERN_OPTS)) + 1L,
      pattern_v = ifelse(legend_group == "other", "none", .PATTERN_OPTS[opt_idx]),
      angle_v   = ifelse(legend_group == "other", 45,    .ANGLE_OPTS[opt_idx])
    ) %>%
    ungroup()
  list(
    patterns = setNames(label_meta$pattern_v, as.character(label_meta$legend_label)),
    angles   = setNames(label_meta$angle_v,   as.character(label_meta$legend_label))
  )
}

pat_total   <- make_label_patterns(top_10_consumers_shares)

# Create the bar plot
bar_plot <- ggplot(
  top_10_consumers_shares,
  aes(
    x       = reorder(consumer, -total_footprint),
    y       = total_footprint,
    fill          = legend_label,
    pattern       = legend_label,
    pattern_angle = legend_label
  )
) +
  geom_bar_pattern(
    stat                   = "identity",
    position               = "stack",
    pattern_fill           = "white",
    pattern_colour         = "grey45",
    pattern_density        = 0.010,
    pattern_spacing        = 0.05,
    pattern_key_scale_factor = 0.5
  ) +
  scale_y_continuous(labels = scales::scientific_format()) +
  scale_fill_manual(values = prod_commodity_colors) +
  scale_pattern_manual(values = pat_total$patterns) +
  scale_pattern_angle_manual(values = pat_total$angles) +
  labs(
    x = "Consuming Country",
    y = "Biodiversity Footprint (PDF*yr)",
    fill = "Commodity & Producer"
  ) +
  theme_minimal() +
  guides(
    fill          = guide_legend(title = "Commodity & Producer", ncol = 3),
    pattern       = guide_legend(title = "Commodity & Producer", ncol = 3),
    pattern_angle = guide_legend(title = "Commodity & Producer", ncol = 3)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "inside",
    legend.position.inside = c(1, 0.95),
    legend.justification = c(1, 1),
    legend.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

# Axis break: if one consuming country bar is >2.5x the second tallest,
# insert a diagonal-slash break so the scale isn't dominated by the outlier.
.consumer_totals <- top_10_consumers_shares %>%
  group_by(consumer) %>%
  summarise(tot = sum(total_footprint), .groups = "drop") %>%
  arrange(desc(tot))
.y1 <- .consumer_totals$tot[1]
.y2 <- .consumer_totals$tot[2]
if (.y1 > 2.5 * .y2) {
  # Cap y-axis at 125 % of second-tallest bar and draw manual break marks on
  # the outlier bar (which is always at x = 1 after reorder(..., -total)).
  .y_cap   <- .y2 * 1.25
  .y_lim   <- .y_cap * 1.06   # small headroom above cap for the upper bar stub
  .gap_lo  <- .y_cap * 0.910
  .gap_hi  <- .y_cap * 0.975
  .gap_mid <- (.gap_lo + .gap_hi) / 2

  bar_plot <- bar_plot +
    coord_cartesian(ylim = c(0, .y_lim), clip = "off") +
    # Solid white rectangle fully covers the gap — colour = "white" with linewidth
    # seals edge artefacts where ggpattern hatch lines bleed through
    annotate("rect",
             xmin = 0.45, xmax = 1.55,
             ymin = .gap_lo, ymax = .gap_hi,
             fill = "white", colour = "white", linewidth = 1.5) +
    # Left diagonal slash
    annotate("segment",
             x = 0.58, xend = 0.84,
             y = .gap_lo, yend = .gap_hi,
             colour = "grey30", linewidth = 0.7) +
    # Right diagonal slash
    annotate("segment",
             x = 0.94, xend = 1.42,
             y = .gap_lo, yend = .gap_hi,
             colour = "grey30", linewidth = 0.7) +
    # Value label above the bar top — clip = "off" lets it sit outside the panel
    annotate("label",
             x = 1, y = .y_lim * 1.02,
             label = formatC(.y1, format = "e", digits = 2),
             size = 2.5, vjust = 0, hjust = 0.5, colour = "grey25",
             fill = "white", label.size = 0.3,
             label.padding = unit(0.15, "lines"))
}

print(bar_plot)

ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar.png",
       plot = bar_plot, width = 180, height = 140, unit = 'mm', dpi = 300)

ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar.svg",
       plot = bar_plot, width = 180, height = 140, unit = 'mm')


# THE SAME BUT PER CAPITA-------------------------------------------------------
# NOTE: requires 'world_bank_pop' to be loaded before running this section.
top_10_consumers_per_cap <- as.data.frame(colSums(commodities_to_assess, na.rm = TRUE))
colnames(top_10_consumers_per_cap) <- "total_footprint"
top_10_consumers_per_cap <- top_10_consumers_per_cap %>%
  mutate(consuming_country = as.integer(rownames(top_10_consumers_per_cap))) %>%
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = iso3c) %>%
  left_join(world_bank_pop[world_bank_pop$indicator == "SP.POP.TOTL", ][c("country", "2017")],
            by = c("consumer" = "country")) %>%
  rename(pop_est = "2017") %>%
  mutate(total_footprint_per_cap = if_else(pop_est == 0, 0, total_footprint / pop_est)) %>%
  dplyr::select(-c(total_footprint, consuming_country, continent, area)) %>%
  arrange(desc(total_footprint_per_cap)) %>%
  filter(pop_est >= 1000000) %>%
  dplyr::select(-c(pop_est)) %>%
  head(25)

# Calculate the share of production countries and commodities for the top consumers
top_10_consumers_shares_per_cap <- commodities_to_assess[, colnames(commodities_to_assess) %in% regions$area_code[regions$iso3c %in% top_10_consumers_per_cap$consumer]]
# Convert to long format
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  rownames_to_column("prod_commodity") %>%
  pivot_longer(cols = -prod_commodity, names_to = "consuming_country", values_to = "footprint")
# Separate producer and commodity
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  separate(prod_commodity, into = c("commodity", "producing_country"), sep = "_", remove = TRUE)
# Convert country codes to integers for later join
top_10_consumers_shares_per_cap$consuming_country <- as.integer(top_10_consumers_shares_per_cap$consuming_country)
top_10_consumers_shares_per_cap$producing_country <- as.integer(top_10_consumers_shares_per_cap$producing_country)
# Replace consuming country codes with names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(regions, by = c("consuming_country" = "area_code")) %>%
  rename(consumer = area) %>%
  dplyr::select(-c(consuming_country, iso3c, continent))
# Replace producing country codes with names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(regions, by = c("producing_country" = "area_code")) %>%
  rename(producer = area) %>%
  dplyr::select(-c(producing_country, iso3c, continent))
# Replace commodity codes with commodity names
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(items, by = c("commodity" = "comm_code")) %>%
  rename(commodity_name = item) %>%
  dplyr::select(-c(commodity, item_code, unit, group, moisture, feedtype, comm_group))
# Arrange in descending order for visualization
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  arrange(desc(footprint))
# Change names to iso3
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  left_join(regions, by = c("consumer" = "area")) %>%
  dplyr::select(-c(consumer, area_code, continent)) %>%
  rename(consumer = iso3c) %>%
  left_join(regions, by = c("producer" = "area")) %>%
  dplyr::select(-c(producer, area_code, continent)) %>%
  rename(producer = iso3c)
# Summarise the footprint contributions
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  group_by(consumer, commodity_name, producer) %>%
  summarise(total_footprint = sum(footprint, na.rm = TRUE), .groups = 'drop')
# Identify top contributors and group the rest as 'Other'
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  group_by(consumer) %>%
  mutate(rank = rank(-total_footprint, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    prod_commodity_name = ifelse(rank <= 2, paste(producer, commodity_name, sep = "_"), "other")
  ) %>%
  group_by(consumer, prod_commodity_name) %>%
  summarise(total_footprint = sum(total_footprint, na.rm = TRUE), .groups = 'drop') %>%
  left_join(world_bank_pop[world_bank_pop$indicator == "SP.POP.TOTL", ][c("country", "2017")],
            by = c("consumer" = "country")) %>%
  rename(pop_est = "2017") %>%
  mutate(total_footprint_per_cap = if_else(pop_est == 0, 0, total_footprint / pop_est)) %>%
  dplyr::select(-c(total_footprint, pop_est)) %>%
  mutate(prod_commodity_name = as.character(prod_commodity_name)) %>%
  mutate(prod_commodity_name = str_replace_all(prod_commodity_name, "_", " "))

# Simplify and group data for legend simplicity
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  mutate(
    producer = sub(" .*", "", prod_commodity_name),
    commodity_name = sub("^[A-Z]{3} ", "", prod_commodity_name),
    simplified_name = recode(commodity_name, !!!simplified_names),
    legend_label = paste(simplified_name, producer, sep = " "),
    legend_group = factor(simplified_name, levels = c("Coffee", "Cocoa", "Tea", "Tobacco", "other"))
  ) %>%
  arrange(legend_group) %>%
  mutate(legend_label = factor(legend_label, levels = unique(legend_label)))

# Change 'other other' to 'Other'
top_10_consumers_shares_per_cap <- top_10_consumers_shares_per_cap %>%
  mutate(legend_label = recode(legend_label, "other other" = "Other"))

# Generate colors for legend labels
prod_commodity_colors_per_cap <- setNames(
  unlist(lapply(levels(top_10_consumers_shares_per_cap$legend_group), function(group) {
    group_rows <- top_10_consumers_shares_per_cap %>% filter(legend_group == group)
    n <- n_distinct(group_rows$legend_label)
    commodity_palettes[[group]](n)
  })),
  levels(top_10_consumers_shares_per_cap$legend_label)
)

pat_per_cap <- make_label_patterns(top_10_consumers_shares_per_cap)

# Create the bar plot
bar_plot_per_cap <- ggplot(
  top_10_consumers_shares_per_cap,
  aes(
    x       = reorder(consumer, -total_footprint_per_cap),
    y       = total_footprint_per_cap,
    fill          = legend_label,
    pattern       = legend_label,
    pattern_angle = legend_label
  )
) +
  geom_bar_pattern(
    stat                   = "identity",
    position               = "stack",
    pattern_fill           = "white",
    pattern_colour         = "grey45",
    pattern_density        = 0.010,
    pattern_spacing        = 0.05,
    pattern_key_scale_factor = 0.5
  ) +
  scale_y_continuous(labels = scales::scientific_format()) +
  scale_fill_manual(values = prod_commodity_colors_per_cap) +
  scale_pattern_manual(values = pat_per_cap$patterns) +
  scale_pattern_angle_manual(values = pat_per_cap$angles) +
  labs(
    x = "Consuming Country",
    y = "Biodiversity Footprint per Capita (PDF*yr/cap)",
    fill = "Commodity & Producer"
  ) +
  guides(
    fill          = guide_legend(title = "Commodity & Producer", ncol = 3),
    pattern       = guide_legend(title = "Commodity & Producer", ncol = 3),
    pattern_angle = guide_legend(title = "Commodity & Producer", ncol = 3)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_rect(fill = "white", color = "grey"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.5, "cm"),
    legend.position = "inside",
    legend.position.inside = c(1, 0.95),
    legend.justification = c(1, 1),
    legend.title = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white")
  )

print(bar_plot_per_cap)

ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar_per_cap.png",
       plot = bar_plot_per_cap, width = 180, height = 140, unit = 'mm', dpi = 300)

ggsave("X:\\Eli\\PROJECTS\\fabio_stimulants\\results_figures\\country_impacts\\cons_country_stacked_bar_per_cap.svg",
       plot = bar_plot_per_cap, width = 180, height = 140, unit = 'mm')


##############################################################################
# Stressor contribution breakdown by consuming country

# Calculate the contribution of each stressor as a fraction of the total
contribution_land  <- bd_fp_landuse / bd_fp_total
contribution_water <- bd_fp_water   / bd_fp_total
contribution_n     <- bd_fp_N       / bd_fp_total   # fixed: was bd_fp_water in original
contribution_p     <- bd_fp_P       / bd_fp_total
contribution_ghg   <- bd_fp_GHG     / bd_fp_total   # new: climate change

# Restore rownames
rownames(contribution_land)  <- row_names
rownames(contribution_water) <- row_names
rownames(contribution_n)     <- row_names
rownames(contribution_p)     <- row_names
rownames(contribution_ghg)   <- row_names

# Sum across rows
contribution_land_sum  <- data.frame(land  = colSums(contribution_land,  na.rm = TRUE))
contribution_water_sum <- data.frame(water = colSums(contribution_water, na.rm = TRUE))
contribution_n_sum     <- data.frame(n     = colSums(contribution_n,     na.rm = TRUE))
contribution_p_sum     <- data.frame(p     = colSums(contribution_p,     na.rm = TRUE))
contribution_ghg_sum   <- data.frame(ghg   = colSums(contribution_ghg,   na.rm = TRUE))

# Combine into single df
footprint_contributions_sums_combined <- as.data.frame(cbind(
  land  = contribution_land_sum,
  water = contribution_water_sum,
  n     = contribution_n_sum,
  p     = contribution_p_sum,
  ghg   = contribution_ghg_sum
))
footprint_contributions_sums_combined$total <- rowSums(footprint_contributions_sums_combined)

# Top 25 consumers by total contribution score
footprint_contributions_top_25 <- footprint_contributions_sums_combined %>%
  arrange(desc(total)) %>%
  head(25)

# Reshape into long format
footprint_contributions_top_25 <- footprint_contributions_top_25 %>%
  rownames_to_column(var = "consuming_country") %>%
  pivot_longer(cols = c(land, water, n, p, ghg), names_to = "footprint_type", values_to = "contribution")

footprint_contributions_top_25 <- footprint_contributions_top_25 %>%
  mutate(country = as.integer(sub("_.*", "", consuming_country)),
         consumption_category = sub(".*_", "", consuming_country)) %>%
  left_join(regions, by = c("country" = "area_code")) %>%
  dplyr::select(-c(consuming_country, country, area, continent))

# PLOT!
ggplot(footprint_contributions_top_25, aes(x = reorder(iso3c, -total), y = contribution, fill = footprint_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Footprint by Consuming Country",
       x = "Consuming Country",
       y = "Total Footprint",
       fill = "Footprint Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
