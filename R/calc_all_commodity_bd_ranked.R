
# BD footprint for every FABIO commodity code, ranked highest to lowest
# Same methodology as plot_fig_x_bd_impact_numberline.R

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

total_bd_demand <- function(comm_code) {
  row_idx  <- grep(paste0("^", comm_code, "_"), row_names)
  if (length(row_idx) == 0) return(0)
  y_X      <- numeric(length(y_total))
  y_X[row_idx] <- y_total[row_idx]
  output_X <- as.numeric(L %*% y_X)
  fw_bd    <- sum(output_X * e_fw,   na.rm = TRUE)
  terr_bd  <- sum(output_X * e_terr, na.rm = TRUE)
  (fw_bd + terr_bd) / 2
}

# ── All FABIO total ────────────────────────────────────────────────────────────
cat("Computing total FABIO footprint...\n")
output_all <- as.numeric(L %*% y_total)
total_all  <- (sum(output_all * e_fw, na.rm = TRUE) +
               sum(output_all * e_terr, na.rm = TRUE)) / 2

# ── Stimulant flag ─────────────────────────────────────────────────────────────
stim_codes <- items$comm_code[
  items$comm_group == "Coffee, tea, cocoa" |
  (items$comm_group == "Tobacco, rubber" & items$comm_code != "c060")
]

# ── Loop over every unique commodity code ──────────────────────────────────────
all_codes <- unique(items$comm_code)
cat(sprintf("Computing footprint for %d commodity codes...\n", length(all_codes)))

results <- lapply(seq_along(all_codes), function(i) {
  code <- all_codes[i]
  if (i %% 20 == 0) cat(sprintf("  %d / %d\n", i, length(all_codes)))
  bd <- total_bd_demand(code)
  item_name  <- items$item[items$comm_code == code][1]
  comm_group <- items$comm_group[items$comm_code == code][1]
  data.frame(
    comm_code  = code,
    item       = item_name,
    comm_group = comm_group,
    is_stimulant = code %in% stim_codes,
    bd_footprint = bd,
    stringsAsFactors = FALSE
  )
})

ranked <- do.call(rbind, results)
ranked <- ranked[order(ranked$bd_footprint, decreasing = TRUE), ]
ranked$rank    <- seq_len(nrow(ranked))
ranked$pct     <- ranked$bd_footprint / total_all * 100
ranked$cum_pct <- cumsum(ranked$pct)

cat("\n══════════════════════════════════════════════════════════════════════\n")
cat(sprintf("Total FABIO BD footprint: %.4e PDF·yr\n\n", total_all))
cat(sprintf("%-4s  %-8s  %-45s  %-12s  %6s  %8s\n",
            "Rank", "Code", "Commodity", "Comm group", "BD (PDF·yr)", "% total"))
cat(strrep("-", 100), "\n")
for (i in seq_len(nrow(ranked))) {
  r <- ranked[i, ]
  stim_flag <- if (r$is_stimulant) " *" else "  "
  cat(sprintf("%4d%s %-8s  %-45s  %-20s  %.3e  %6.3f%%  (cum: %6.2f%%)\n",
              r$rank, stim_flag, r$comm_code,
              substr(r$item, 1, 45),
              substr(r$comm_group, 1, 20),
              r$bd_footprint, r$pct, r$cum_pct))
}
cat(strrep("=", 100), "\n")
cat("* = stimulant commodity\n")

# Save CSV
out_csv <- "R/all_commodity_bd_ranked.csv"
fwrite(ranked, out_csv)
cat(sprintf("\nSaved: %s\n", out_csv))
