#### Country diversity indices ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(stringr) # String handling
library(ggplot2) # Plotting
# Community analyses
library(vegan)
library(labdsv)

# Get rid of scientific notation
options(scipen = 9999)

# source numValSppTally function
source(here::here("functions", "numValSppTally.R"))

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "import-data-final.csv"))
import_data <- data.table(import_data)

# Data prep --------------------------------------------------------------------

# tally by country
country_tally <- numValSppTally(data = import_data,
                                grouping = "Origin_country")

# tally only those at species and subspecies level
country_species <- numValSppTally(data = import_data[Rank %in% c("SPECIES", "SUBSPECIES")],
                                  grouping = c("Origin_country",
                                               "Binomial"))

# select columns
country_species <- country_species[, c("Origin_country",
                                       "Binomial",
                                       "Number")]

# make into matrix
country_species <- matrify(country_species)

# Diversity indices ------------------------------------------------------------

# Shannon index
country_species_shannon <- vegan::diversity(country_species,
                                            index = "shannon")
country_species_shannon <- as.data.frame(country_species_shannon)
country_species_shannon$country <- rownames(country_species_shannon)

# Simpson's index index
country_species_simpson <- vegan::diversity(country_species,
                                            index = "simpson")
country_species_simpson <- as.data.frame(country_species_simpson)
country_species_simpson$country <- rownames(country_species_simpson)

# Join
country_tally <- merge.data.frame(country_tally, country_species_shannon,
                                  by.x = "Origin_country",
                                  by.y = "country") %>% 
  merge.data.frame(country_species_simpson,
                   by.x = "Origin_country",
                   by.y = "country")
country_tally

# Group tallies ----------------------------------------------------------------

# aggregate by group
country_group <- numValSppTally(import_data, grouping = c("Origin_country", "Group"))

# aggregate within groups
country_fish <- numValSppTally(import_data[Group == "Bony fishes"], grouping = c("Origin_country"))
country_verts <- numValSppTally(import_data[Group == "Other vertebrates"], grouping = c("Origin_country"))
country_corals <- numValSppTally(import_data[Group == "Stony corals"], grouping = c("Origin_country"))
country_inverts <- numValSppTally(import_data[Group == "Other invertebrates"], grouping = c("Origin_country"))
country_algae <- numValSppTally(import_data[Group == "Algae"], grouping = c("Origin_country"))
