#### Stony corals ####

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

# subset to coral
coral_dat <- import_data[Group == "Stony corals"]

# Tallies -----------------------------------------------------------------------

## families =====

# by family
coral_families <- numValSppTally(data = coral_dat, grouping = c("Family",
                                                                "Order",
                                                                "Class"))

# top ten families by number
coral_families_number <- head(coral_families[order(-Number)], n = 10)

# top ten families by value
coral_families_value <- head(coral_families[order(-Value_USD)], n = 10)

## species =====

# by species
coral_species <- numValSppTally(data = coral_dat, grouping = c("Binomial",
                                                               "Family",
                                                               "Order",
                                                               "Class"))

# top ten species by number
coral_species_number <- head(coral_species[order(-Number)], n = 10)

# top ten species by value
coral_species_value <- head(coral_species[order(-Value_USD)], n = 10)

# Plots ------------------------------------------------------------------------

# create palette
coral_fam_palette <- c("#332288",
                       "#BBBBBB",
                       "#0077BB",
                       "#88CCEE",
                       "#44AA99",
                       "#117733",
                       "#999933",
                       "#DDCC77",
                       "#CC6677",
                       "#882255",
                       "#AA4499")

names(coral_fam_palette) <- unique(coral_families_number$Family)

# plot by number
stony_corals_number <- ggplot(coral_families_number,
                              aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = coral_fam_palette) +
  theme_bw() +
  labs(tag = "A")

# plot by value
stony_corals_value <- ggplot(coral_families_value,
                             aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = coral_fam_palette) +
  theme_bw() +
  labs(tag = "B")

# combine plots
stony_corals_plot <- stony_corals_number + stony_corals_value & theme(legend.position = "none")
stony_corals_plot

# save
png(filename = here::here("outputs", "plots", "stony_corals_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(stony_corals_plot)
dev.off()
