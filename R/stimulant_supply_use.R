
# SUPPLY-USE TABLE CONSTRUCTION FOR STIMULANT COMMODITIES
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no

# Still very much not functional. Converted directly from python code via 
#  chatGPT and still requires debugging. No trade data yet. No tobacco products 
#  yet. Modify as needed.

# Load necessary libraries
library(tidyverse)

# Set paths
project_path <- getwd()
fabio_path <- "C:\\Users\\elishaw\\OneDrive - NTNU\\Git\\fabio"

# Fetching data on just cocoa production in Cote d'Ivoire (test data)
#data_cocoa_cotedivoire_2021_path <- paste0(project_path, "/SUA data/FAOSTAT_data_cocoa_cotedivoire_2021_table.csv")
#data_cocoa_cotedivoire_2021 <- read.csv(data_cocoa_cotedivoire_2021_path)

cat("Fetching all SUA crop and livestock data for stimulant commodities.\n")
sua_full_path <- paste0(fabio_path, "\\input\\fao\\SUA_Crops_Livestock_E_All_Data_(Normalized).csv")
sua_full <- read.csv(sua_full_path, encoding = 'ISO-8859-1')

sua_full <- sua_full %>%
  filter(Item %in% c('Chocolate products nes',
                     'Cocoa beans',
                     'Cocoa butter, fat and oil',
                     'Cocoa husks and shells',
                     'Cocoa paste not defatted',
                     'Cocoa powder and cake',
                     'Tea leaves',
                     'Extracts, essences and concentrates of tea or mate, and preparations with a basis thereof or with a basis of tea or mate',
                     'Coffee, green',
                     'Coffee, decaffeinated or roasted',
                     'Coffee extracts',
                     'Coffee substitutes',
                     'Maté leaves')) %>%
  droplevels()

colnames(sua_full) <- c("area_code",
                        "area_code_m49",
                        "area",
                        "item_code",
                        "item",
                        "element_code",
                        "element",
                        "year_code",
                        "year",
                        "unit",
                        "value",
                        "flag")
print(sua_full)

# Set SUA data to the SUA_full dataset. 
# Alternatively can be tested with smaller Cote d'Ivoire cocoa data.
sua <- sua_full %>%
  # Rename columns, eliminate unnecessary characters
  select("unit","flag",'year_code') %>%
  #rename(item_code="Item.Code..CPC.", item=Item, area_code="Area.Code..M49.", 
  #       area=Area, year=Year, element=Element, value=Value) %>%
  mutate(item_code = gsub("'", "", item_code),
         area_code = gsub("'", "", area_code),
         item_code = gsub("^0+", "", item_code),
         item = gsub("é", "e", item))

#filtered_data <- btd_full %>%
#  filter(from == "Tanzania", grepl('Cotton', item), year == 2021)

# List of Items (full, and in supply/use tables)
items_full_path <- paste0(fabio_path, "\\inst\\items_full_new.csv")
items <- read.csv(items_full_path)

# Importing supply items
items_supply_path <- paste0(fabio_path, "\\inst\\items_supply_new.csv")
supply_items <- read.csv(items_supply_path)

# Importing use items 
items_use_path <- file.path(fabio_path, "inst", "items_use_new.csv")
use_items <- read.csv(items_use_path)


#Supply-------------------------------------------------------------------------

cat("Building the supply tables.\n")

# Supply table takes only production values. Subset sua data for these values.
sua_supply <- sua %>%
  filter(element == 'Production')

# Merge the larger SUA production data with the supply items
#  (only with sup_items commodities included in the sua data being used).
sup <- left_join(
  select(sua_supply, area_code, area, year, item_code, item, value),
  filter(supply_items, item_code %in% unique(sua_supply$item_code)),
  by = c("item_code", "item")
) %>%
  mutate(supply = value) %>%
  select(-value)

# Correct commodity codes, column names
sup <- sup %>%
  select(-value) %>%
  mutate(supply = value)

# Check if cocoa Cote d'Ivoire data matches...
sup[sup$area == "Côte d'Ivoire",][grepl('Cocoa', sup$item),][sup$year == 2021,]


#Use----------------------------------------------------------------------------

cat("Filtering SUA data for the use items.\n")

# Simplified table (no feed or seedwaste)?
sua_use <- sua[sua$element %in% c("Production", "Processed", "Loss"), ]

# Pivoting creates columns for production and processing of commodities shown 
#  in each use item.
sua_use_piv <- reshape::cast(sua_use,
                             year+area_code+area+item_code+item~element,
                             value = "value",
                             fun = sum)
sua_use <- data.frame(sua_use_piv)
# Return to original index and clean up column names
colnames(sua_use) <- c("year","area_code","area","item_code","item","losses",
                       "processed","production")

cat("Building the use table.\n")

# Merge the larger SUA data with the use items and their respective processes
use <- merge(sua_use[c("area_code","area","year","item_code","item","processed",
                       "production","losses")],
             use_items[use_items$item_code %in% sua_use$item_code, ],
             by = c("item_code", "item"),
             all.x = TRUE,
             suffixes = c("_cbs","_use"))

#Start with use value of 0 for all commodities.
use$use <- NA

# Calculate 'use' values for commodities with a TCF:

# Create dataframe containing the TCFs for stimulant commodities.
# Does not include tea extracts or tobacco products yet!!
# TCF conv. represents commodities whose input products must be converted into 
#  the output commodity via a change in mass. 
TCF_conv <- data.frame(proc_code = c("p128",
                                     "p124",
                                     "p125"),
                       proc = c("Coffee substitute production",
                                "Coffee production, decaffeinated or roasted",
                                "Coffee extract production"),
                       inp_code = c("1610",
                                    "1610",
                                    "1610"),
                       inp = c("Coffee, green",
                               "Coffee, green",
                               "Coffee, green"),
                       out_code = c("23912.01",
                                    "23911",
                                    "23912.02"),
                       out = c("Coffee substitutes",
                               "Coffee, decaffeinated or roasted",
                               "Coffee extracts"),
                       value = c(0.7,
                                 0.84,
                                 0.38))

# Create dataframe of use table objects which require TCFs.
TCF_objs <- use[use$type == "TCF", ]

# Dataframe of production (supply) data from corresponding processes:
TCF_prod <- sup[sup$proc %in% unique(TCF_objs$proc), c("area_code","year",
                                                       "proc_code","proc",
                                                       "supply")]

# Merging the two dataframes to create new column 'supply' in use table 
#  objects that require TCFs.
TCF_use <- merge(TCF_objs,
                 TCF_prod,
                 by = c("area_code","year","proc_code","proc"),
                 all = FALSE)

# For all processes which require TCFs...
for (proc in unique(TCF_use$proc)) {
  # Fetch the TCF for the given process
  TCF <- TCF_conv[TCF_conv$proc == proc, "value"]
  # Set 'use' column equal to the 'supply' column divided by the TCF
  TCF_use$use[TCF_use$proc == proc] <- TCF_use$supply[TCF_use$proc == proc] / TCF
}
colnames(TCF_use) <- c("area_code","area","year","item_code","item","processed",
                       "production","losses","type","proc_code","proc","supply",
                       "use")

# Add use values calculated from TCFs back into the use dataframe
TCF_use <- TCF_use[c("area_code","area","year","item_code","item","use")]
use <- merge(use,
             TCF_use,
             by = c("area_code","area","year","item_code","item","type",
                    "proc_code","proc"),
             all = TRUE)

# Set 'use' values equal to the 'TCF_use' values we just calculated
use[use$type == "TCF", "use"] <- use[use$type == "TCF", "use.x"]
use <- use[c("area_code","area","year","item_code","item","processed",
             "production","losses","type","proc_code","proc","use")]

# Add 'use' values for commodities with 100% use in the next commodity
use[use$type == "100%", "use"] <- use[use$type == "100%", "processed"]

# Add 'use' values for seedwaste
use[use$type == "seedwaste", "use"] <- use[use$type == "seedwaste", "losses"]

# Allocate cocoa husks across livestock feed.
#! Currently not an accurate estimate. Just evenly distributed! Must fix.
use[use$type == "feed", "use"] <- use[use$type == "feed", "production"] / length(unique(use[use$type == "feed", "proc"]))


# Drop unnecessary columns and dropping commodities with no use process:

# Preserve a use table with processed values for the residual calculations.
use_processed <- use[, c("area_code","area","year","item_code","item",
                         "processed","production","losses")]

# Eliminate non-use commodities.
use <- use[!is.na(use$proc), ]

# Check if cocoa Cote d'Ivoire data matches...
use[use$area == "Côte d'Ivoire",][grepl('Cocoa', use$item),][use$year == 2021,]


#Final Demand Table-------------------------------------------------------------

cat("Calculating total Use and Residuals values for the final demand table.\n")

# Create a pivot table from the Use table by process;
#  essentially a complete use table.
use_pivot <- reshape2::dcast(use,
                             year+area_code+area+item_code+item+comm_code~proc,
                             value.var = "use")

# Sum up the total use values by commodity into a new 'use_tot' column
use_pivot$use_tot <- rowSums(use_pivot[, 7:ncol(use_pivot)], na.rm = TRUE)
use_pivot <- use_pivot[, c("year","area_code","area","item_code","item",
                           "comm_code","use_tot")]

# Get residual values from TCF calculations
TCF_items_in_use <- use[use$type == "TCF", ]
TCF_items_in_sua <- sua_use[sua_use$item %in% TCF_items_in_use$item, ]
TCF_residual <- merge(TCF_items_in_sua,
                      TCF_items_in_use,
                      by = c("year","area_code","area","item_code","item"),
                      all.x = TRUE)

# Now we group all TCF commodities (sum them for each year and area and item,
#  which should only be green coffee at this point).
TCF_residual_grouped <- aggregate(TCF_residual["use"],
                                  by = TCF_residual[c("year","area_code","area",
                                                      "item_code","item")],
                                  sum)
# Rename 'use' column to be more specific.
colnames(TCF_residual_grouped) <- c("year","area_code","area","item_code",
                                    "item","total_comm_tcf_use")

# Now we add back the data on the amount of each commodity that is 'processed',
#  which comes from the ungrouped dataframe.
TCF_residual_grouped <- merge(TCF_residual_grouped,
                              TCF_residual,
                              by = c("year","area_code","area","item_code",
                                     "item"),
                              all.x = TRUE)

# Use totals for TCF processes are now grouped, so no need for use data for
#  specific processes.
TCF_residual_grouped <- TCF_residual_grouped[, c("year","area_code","area",
                                                 "item_code","item","processed",
                                                 "total_comm_tcf_use")]

# Calculate the difference between the amount of each commodity processed,
#  and the amount of each commodity that is used based on our TCF calculations. 
#  The result is called the 'residuals' from the TCF calculation.
TCF_residual_grouped$residuals_from_TCF <- TCF_residual_grouped$processed - TCF_residual_grouped$total_comm_tcf_use

# Clean up columns and drop duplicate items 
TCF_residual_grouped <- TCF_residual_grouped[, c("year","area_code","area",
                                                 "item_code","item",
                                                 "residuals_from_TCF")]
TCF_residual_grouped <- unique(TCF_residual_grouped)

# Add residuals from TCFs back to the use pivot table
use_pivot <- merge(use_pivot,
                   TCF_residual_grouped,
                   by = c("year","area_code","area","item_code","item",
                          "comm_code"),
                   all.x = TRUE)

print(use_pivot)

cat("Finalizing final demand table.\n")

# Filter SUA data for the final demand categories
sua_fd <- sua[sua$element %in% c("Residuals",
                                 "Stock Variation",
                                 "Tourist consumption",
                                 "Food supply quantity (tonnes)"), ]

# Pivoting creates columns for production and processing of commodities shown in
#  each use item:
sua_fd_piv <- reshape2::dcast(sua_fd,
                              year+area_code+area+item_code+item~element,
                              value.var = "value",
                              fun = sum)
sua_fd <- data.frame(sua_fd_piv)

# Return to original index and clean up column names.
colnames(sua_fd) <- c("year","area_code","area","item_code","item","residuals",
                      "stock_addition","food","tourist")
sua_fd$intermediate_cons <- NULL
sua_fd$balancing <- NULL

# Add the intermediate consumption values from the Use table into the final
#  demand table.
sua_fd <- merge(sua_fd,
                use_pivot[, c("year","area_code","area","item_code","item",
                              "use_tot","residuals_from_TCF")],
                by = c("year","area_code","area","item_code","item"),
                all.x = TRUE)
sua_fd$intermediate_cons <- sua_fd$use_tot

# Fill data in the 'balance' column
sua_fd$balancing <- sua_fd$residuals + sua_fd$residuals_from_TCF
sua_fd <- sua_fd[, c("year","area_code","area","item_code","item","residuals",
                     "stock_addition","food","tourist","intermediate_cons",
                     "balancing")]

print(sua_fd)

