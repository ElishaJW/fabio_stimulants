
# Graphing SUA DATA FOR STIMULANT COMMODITIES
# V1.0
# Eli Wilson - elisha.wilson@ntnu.no


# Load necessary libraries --------------------------------------------------------------
library(tidyverse)
library(ggplot2)
# install.packages("stringi")
library(stringi)
library(tidyr)
library(dplyr)


# Reread stimulant data
stim_sua <- readRDS("data/stim_sua.rds")

# Get FABIO Regions
fabio_regions_path <- paste0(fabio_path,
                             "\\inst\\regions_fabio.csv")
fabio_regions <- read.csv(fabio_regions_path, encoding="UTF-8")
fabio_regions$area <- stri_trans_general(fabio_regions$area, "Latin-ASCII")


########### Production of a commodity by country for a single year #############

YearToPlot <- 2021
ItemToPlot <- 'Mate leaves'

# Subset the stim_sua dataframe for the specified year and item
df_subset <- stim_sua[stim_sua$year == YearToPlot, ]
df_subset <- df_subset[df_subset$item == ItemToPlot, ]

# Match with Fabio regions, i.e., only countries, no regions
df_subset <- df_subset[df_subset$area_code %in% fabio_regions$area_code, ]

df_subset$area <- stri_trans_general(df_subset$area, "Latin-ASCII")
df_subset$area <- gsub("\u00F4", "o", df_subset$area)

# Order the data by production values in descending order
df_subset <- df_subset[order(-df_subset$production), ]

# Take the top N countries, e.g., top 10
top_N <- 25
df_top <- head(df_subset, top_N)

# Create a bar plot
ggplot(df_top, aes(x = reorder(area, -production), y = production, fill = area)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Top", top_N, "Countries with Highest", ItemToPlot,  "Production in", YearToPlot),
       x = "Country",
       y = "Production") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')


########### Production over time for a single country ##################

CountryToPlot <- "China"
ItemToPlot <- 'Tea leaves'

# Subset the stim_sua dataframe for the specified area and item
df_subset <- stim_sua[stim_sua$area == CountryToPlot, ]
df_subset <- df_subset[df_subset$item == ItemToPlot, ]

# Create a bar plot
ggplot(df_subset, aes(x = year, y = production)) +
  geom_line() +
  labs(title = paste("Annual Production of", ItemToPlot, "in", CountryToPlot),
       x = "Year",
       y = "Production") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')


ggplot(df_subset, aes(x = year, y = production)) +
  geom_line(color = "darkgreen", size = 1.5) +
  labs(title = paste("Annual Production of", ItemToPlot, "in", CountryToPlot),
       x = "Year",
       y = "Production (tonnes)",
       caption = "Source: FAOSTAT SUA") +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        legend.position = "none") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = unique(df_subset$year), minor_breaks = unique(df_subset$year)) 


########## Consumption Category for a commodity for a country ##############

CountryToPlot <- "China"
YearToPlot <- 2020
ItemToPlot <- 'Tea leaves'

# Subset the stim_sua dataframe for the specified year and item
df_subset <- stim_sua[stim_sua$year == YearToPlot, ]
df_subset <- df_subset[df_subset$item == ItemToPlot, ]
df_subset <- df_subset[df_subset$area == CountryToPlot, ]

cons_cats <- c('exports',
               'feed', 'food',
               'losses', 'other', 'processing',
               'residuals', 'seed', 'tourist', 'stock_addition',
               'balancing')#, 'unspecified')

df_subset <- df_subset[cons_cats]

df_long <- df_subset %>%
  gather(key = "Category", value = "Consumption")

# Create a bar plot
ggplot(df_long, aes(x = Category, y = Consumption)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = paste("Distribution of", ItemToPlot, "by consumption category in", CountryToPlot, "in", YearToPlot),
       x = "Category",
       y = "Consumption",
       caption = "Source: Your Data Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major = element_line(color = "gray", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))


