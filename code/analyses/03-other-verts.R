#### Other vertebrates ####

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

# subset to verts
verts_dat <- import_data[Group == "Other vertebrates"]

# Tallies -----------------------------------------------------------------------

# by family
verts_families <- numValSppTally(data = verts_dat, grouping = c("Family",
                                                              "Order",
                                                              "Class"))

# by species
verts_species <- numValSppTally(data = verts_dat, grouping = c("Binomial",
                                                             "Family",
                                                             "Order",
                                                             "Class"))

# Plots ------------------------------------------------------------------------

# create palette
verts_fam_palette <- c("#332288",
                       "#0077BB",
                       "#88CCEE",
                       "#44AA99")

# assign names to palette
names(verts_fam_palette) <- unique(verts_families$Family)

# plot by number
verts_number <- ggplot(verts_families,
                       aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals imported") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = verts_fam_palette) +
  theme_bw() +
  labs(tag = "A")

# plot by value
verts_value <- ggplot(verts_families,
                      aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family") +
  ylab("\n Total value of imports \n ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = verts_fam_palette) +
  theme_bw() +
  labs(tag = "B")

# combine plots
verts_plot <- verts_number + verts_value & theme(legend.position = "none")
verts_plot

# save
png(filename = here::here("outputs", "plots", "verts_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(verts_plot)
dev.off()

