#### Algae ####

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

# subset to algae
algae_dat <- import_data[Group == "Algae"]

# Tallies ----------------------------------------------------------------------

# by family
algae_families <- numValSppTally(data = algae_dat, grouping = c("Family",
                                                                "Order",
                                                                "Class"))

# by species
algae_species <- numValSppTally(data = algae_dat, grouping = c("Binomial",
                                                               "Family",
                                                               "Order",
                                                               "Class"))

# Plots ------------------------------------------------------------------------

# create palette
algae_family_palette <- c( "#33BBEE",
                           "#DDCC77",
                           "#88CCEE",
                           "#44AA99",
                           "#117733",
                           "#999933")

# assign names to palette
names(algae_family_palette) <- unique(algae_species$Family)

# plot by number
algae_number <- ggplot(algae_families,
                       aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = algae_family_palette) +
  theme_bw() +
  labs(tag = "A")

# plot by value
algae_value <- ggplot(algae_families,
                      aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = algae_family_palette) +
  theme_bw() +
  labs(tag = "B")

# combine plots
algae_plot <- algae_number + algae_value & theme(legend.position = "none")
algae_plot

# save
png(filename = here::here("outputs", "plots", "algae_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(algae_plot)
dev.off()
