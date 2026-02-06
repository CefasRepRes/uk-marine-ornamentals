#### Provenance analysis ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(data.table) # Better data wrangling
library(magrittr) # Pipes %>%
library(ggplot2) # Plotting
library(patchwork) # Plot arranging

# Get rid of scientific notation
options(scipen = 9999)

# source functions
source(here::here("functions", "numValSppTally.R"))
source(here::here("functions", "donutPlot.R"))

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "import-data-final.csv"))
import_data <- data.table(import_data)

# check designations
unique(import_data$Captive_wild)

# NA --> Unknown
import_data[is.na(Captive_wild), Captive_wild := "Unknown"]

# Tallies ----------------------------------------------------------------------

# Fish 
fish_captive <- numValSppTally(import_data[Group == "Bony fishes"],
                               "Captive_wild")

# Verts 
verts_captive <- numValSppTally(import_data[Group == "Other vertebrates"],
                                "Captive_wild")

# Inverts 
inverts_captive <- numValSppTally(import_data[Group == "Other invertebrates"],
                                  "Captive_wild")

# Stony corals
corals_captive <- numValSppTally(import_data[Group == "Stony corals"],
                                 "Captive_wild")

# Algae
algae_captive <- numValSppTally(import_data[Group == "Algae"],
                                "Captive_wild")


# Threatened species 
threatened_captive <- numValSppTally(import_data[IUCN_listing %chin% c("EN", "VU", "CR")],
                                     "Captive_wild")

# CITES species only
cites_captive <- numValSppTally(import_data[!is.na(CITES_listing) &
                                              CITES_listing != ""],
                                "Captive_wild")

# All taxa
captive_overall <- numValSppTally(import_data,
                                  "Captive_wild")

# Plots ------------------------------------------------------------------------

# create palette
captive_palette <- c("#88CCEE",
                     "#DDAA33",
                     "#EE7733",
                     "#4477AA")

# assign names to palette
names(captive_palette) <- c("Unknown", "farmed_likely", 
                            "farmed_declared", "wild_declared")

# plot for each group
fish_donut <- donutPlot(fish_captive, "Bony fishes")
verts_donut <- donutPlot(verts_captive, "Other vertebrates")
corals_donut <- donutPlot(corals_captive, "Stony corals")
inverts_donut <- donutPlot(inverts_captive, "Other invertebrates")
algae_donut <- donutPlot(algae_captive, "Algae")
groups_donut <- fish_donut | verts_donut | corals_donut | inverts_donut | algae_donut
groups_donut

# Save
png(filename = here::here("outputs", "plots", "groups_donut.png"),
    width = 8, height = 6, res = 400, units = "in")
print(groups_donut)
dev.off()

# Plot different listings
cites_donut <- donutPlot(cites_captive, "CITES listed taxa") 
iucn_donut <- donutPlot(threatened_captive, "IUCN threatened taxa") 
listings_donut <- plot_spacer() | cites_donut | iucn_donut | plot_spacer()
listings_donut

# Save
png(filename = here::here("outputs", "plots", "listings_donut.png"),
    width = 8, height = 6, res = 400, units = "in")
print(listings_donut)
dev.off()

# Plot overall
overall_donut <- donutPlot(captive_overall, "Overall")
overall_donut <- cites_donut | iucn_donut | overall_donut
overall_donut <- overall_donut & theme(legend.position = "bottom")
overall_donut

# Save
png(filename = here::here("outputs", "plots", "overall_donut.png"),
    width = 8, height = 6, res = 400, units = "in")
print(overall_donut)
dev.off()
