#### Bony fishes ####

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

# subset to fish
fish_dat <- import_data[Group == "Bony fishes"]

# Tallies ----------------------------------------------------------------------

## family =====

# by family
fish_families <- numValSppTally(data = fish_dat, grouping = c("Family",
                                                              "Order",
                                                              "Class"))

# top ten families by number
fish_families_number <- head(fish_families[order(-Number)], n = 10)

# top ten families by value
fish_families_value <- head(fish_families[order(-Value_USD)], n = 10)

## species =====

# by species
fish_species <- numValSppTally(data = fish_dat, grouping = c("Binomial",
                                                             "Family",
                                                             "Order",
                                                             "Class"))

# top ten species by number
fish_species_number <- head(fish_species[order(-Number)], n = 10)

# top ten species by value
fish_species_value <- head(fish_species[order(-Value_USD)], n = 10)

# Plot -------------------------------------------------------------------------

# create palette
fish_fam_palette <- c("#332288",
                      "#0077BB",
                      "#88CCEE",
                      "#44AA99",
                      "#117733",
                      "#999933",
                      "#DDCC77",
                      "#CC6677",
                      "#CC3311",
                      "#882255",
                      "#AA4499",
                      "#DDDDDD")

# assign names
names(fish_fam_palette) <- c(unique(fish_families_number$Family), 
                             "Microdesmidae") # add for values plot

# plot by number
fish_number <- ggplot(fish_families_number,
                      aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals imported") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = fish_fam_palette,
                    name = "Order") +
  theme_bw() +
  labs(tag = "A")

# plot by value
fish_value <- ggplot(fish_families_value,
                aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports \n($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = fish_fam_palette,
                    name = "Order") +
  theme_bw() +
  labs(tag = "B")

# combine plots
fish_plot <- fish_number + fish_value & theme(legend.position = "none")
fish_plot

# save
png(filename = here::here("outputs", "plots", "fish_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(fish_plot)
dev.off()
