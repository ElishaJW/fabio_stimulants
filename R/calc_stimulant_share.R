
# Quick calculation: stimulant BD footprint as % of total FABIO BD footprint
# Uses identical methodology to plot_fig_x_bd_impact_numberline.R

library(data.table)

setwd('C:\\Users\\elishaw\\OneDrive - NTNU\\BAMBOO-personal\\WP3 - FABIO Development\\fabio_stimulants')

cat("Loading data...\n")
eb      <- readRDS("data/E/E_bd_2020.rds")
items   <- fread("inst/items_full.csv")
regions <- fread("inst/regions_fabio.csv")

year  <- 2020
Y_all <- readRDS("data/IO tables/Y.rds")
Y     <- as.matrix(Y_all[[as.character(year)]])
L     <- readRDS(paste0("data/IO tables/", year, "_L_mass.rds"))

combinations <- expand.grid(items$comm_code, regions$area_code)
row_names    <- paste(combinations$Var1, combinations$Var2, sep = "_")

e_water <- as.numeric(eb["water_bd",   ]); e_water[is.na(e_water)] <- 0
e_N     <- as.numeric(eb["N_bd",       ]); e_N[is.na(e_N)]         <- 0
e_P     <- as.numeric(eb["P_bd",       ]); e_P[is.na(e_P)]         <- 0
e_fw    <- e_water + e_N + e_P

e_land  <- as.numeric(eb["landuse_bd", ]); e_land[is.na(e_land)]   <- 0
e_CH4   <- as.numeric(eb["CH4_bd",     ]); e_CH4[is.na(e_CH4)]     <- 0
e_CO2   <- as.numeric(eb["CO2_bd",     ]); e_CO2[is.na(e_CO2)]     <- 0
e_N2O   <- as.numeric(eb["N2O_bd",     ]); e_N2O[is.na(e_N2O)]     <- 0
e_terr  <- e_land + e_CH4 + e_CO2 + e_N2O

y_total <- rowSums(Y)

total_bd_demand <- function(comm_codes) {
  row_idx  <- grep(paste0("^(", paste(comm_codes, collapse = "|"), ")_"), row_names)
  y_X      <- numeric(length(y_total))
  y_X[row_idx] <- y_total[row_idx]
  output_X <- as.numeric(L %*% y_X)
  fw_bd    <- sum(output_X * e_fw,   na.rm = TRUE)
  terr_bd  <- sum(output_X * e_terr, na.rm = TRUE)
  (fw_bd + terr_bd) / 2
}

# ── All FABIO commodities ──────────────────────────────────────────────────────
cat("Computing total FABIO footprint (all commodities)...\n")
output_all <- as.numeric(L %*% y_total)
fw_all     <- sum(output_all * e_fw,   na.rm = TRUE)
terr_all   <- sum(output_all * e_terr, na.rm = TRUE)
total_all  <- (fw_all + terr_all) / 2

# ── Stimulant commodity codes ──────────────────────────────────────────────────
stim_codes <- items$comm_code[
  items$comm_group == "Coffee, tea, cocoa" |
  (items$comm_group == "Tobacco, rubber" & items$comm_code != "c060")
]
cat("Stimulant commodity codes:", paste(stim_codes, collapse = ", "), "\n")

cat("Computing stimulant footprint...\n")
items_coffee  <- c("Coffee, green", "Coffee, decaffeinated or roasted",
                   "Coffee extracts", "Coffee substitutes")
items_tea     <- c("Tea leaves",
                   "Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate",
                   "Mate leaves")
items_cocoa   <- c("Cocoa beans", "Cocoa butter, fat and oil",
                   "Cocoa husks and shells", "Cocoa paste not defatted",
                   "Cocoa powder and cake", "Chocolate products nes")
items_tobacco <- c("Unmanufactured tobacco", "Cigars and cheroots",
                   'Other manufactured tobacco and manufactured tobacco substitutes; homogenized"" or ""reconstituted"" tobacco; tobacco extracts and essences"',
                   "Cigarettes")

total_bd_demand_items <- function(item_names) {
  codes <- items$comm_code[items$item %in% item_names]
  total_bd_demand(codes)
}

bd_coffee  <- total_bd_demand_items(items_coffee)
bd_cocoa   <- total_bd_demand_items(items_cocoa)
bd_tea     <- total_bd_demand_items(items_tea)
bd_tobacco <- total_bd_demand_items(items_tobacco)
bd_stims   <- bd_coffee + bd_cocoa + bd_tea + bd_tobacco

# ── Results ───────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════\n")
cat(sprintf("Total FABIO BD footprint (all commodities): %.4e PDF·yr\n", total_all))
cat(sprintf("  Coffee:  %.4e PDF·yr\n", bd_coffee))
cat(sprintf("  Cocoa:   %.4e PDF·yr\n", bd_cocoa))
cat(sprintf("  Tea:     %.4e PDF·yr\n", bd_tea))
cat(sprintf("  Tobacco: %.4e PDF·yr\n", bd_tobacco))
cat(sprintf("  Total stimulants: %.4e PDF·yr\n", bd_stims))
cat(sprintf("\nStimulants as %% of all FABIO: %.4f%%\n", bd_stims / total_all * 100))
cat(sprintf("  Coffee:  %.4f%%\n", bd_coffee  / total_all * 100))
cat(sprintf("  Cocoa:   %.4f%%\n", bd_cocoa   / total_all * 100))
cat(sprintf("  Tea:     %.4f%%\n", bd_tea     / total_all * 100))
cat(sprintf("  Tobacco: %.4f%%\n", bd_tobacco / total_all * 100))
cat("══════════════════════════════════════════════\n")
