#### Conservation listing analysis ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(data.table) # Better data wrangling
library(magrittr) # Pipes %>%
library(ggplot2) # Plotting
library(patchwork) # Plot arranging

# Get rid of scientific notation
options(scipen = 9999)

# source numValSppTally function
source(here::here("functions", "numValSppTally.R"))

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "import-data-final.csv"))
import_data <- data.table(import_data)

# subset threatened taxa
threatened_dat <- import_data[IUCN_listing %in% c("EN", "CR", "VU")]

# Tallies ----------------------------------------------------------------------

# Species by IUCN listing
iucn_species_count <- numValSppTally(data = import_data,
                                     grouping = "IUCN_listing")

# Conservation listing groups
iucn_group_count <- numValSppTally(data = import_data,
                                   grouping = c("IUCN_listing", "Group"))

# Threatened by group
threatened_group <- numValSppTally(data = threatened_dat,
                                   grouping = c("IUCN_listing", "Group"))

# Threatened overall
threatened_taxa <- numValSppTally(data = threatened_dat,
                                  grouping = c("Binomial",
                                               "Family",
                                               "Order",
                                               "Class",
                                               "IUCN_listing",
                                               "CITES_listing"))

# Species by CITES listing
spp_cites <- numValSppTally(data = import_data,
                               grouping = "CITES_listing")

# Group by CITES listing
cites_group <- numValSppTally(data = import_data,
                                 grouping = c("CITES_listing", "Group"))
