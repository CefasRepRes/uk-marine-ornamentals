#### Taxa overview ####

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

# Numbers ----------------------------------------------------------------------

# Number of records
count_records <- unique(import_data$AWB_ID) %>% length()

# Number of species
count_species <- unique(import_data$Binomial[import_data$Rank %in% c("SPECIES",
                                                                     "SUBSPECIES")]) %>% length()

# Number of taxa
count_taxa <- unique(import_data$Binomial) %>% length()

# Number of families
count_families <- unique(import_data$Family) %>% length()

# Number of classes
count_classes <- unique(import_data$Class) %>% length()

# Tallys -----------------------------------------------------------------------

# species
spp_tally <- numValSppTally(data = import_data[Rank %in% c("SPECIES", "SUBSPECIES")],
                            grouping = c("Binomial", "Family", "Order", "Group"))

# number of species imported once
imported_once <- spp_tally[spp_tally$Number == 1] %>% nrow()

# class
class_tally <- numValSppTally(data = import_data,
                              grouping = "Class")

# taxonomic group
group_tally <- numValSppTally(data = import_data,
                              grouping = "Group")

# Plot -------------------------------------------------------------------------

# set palette
group_palette <- c("#EE6677",
                   "#66CCEE",
                   "#CCBB44",
                   "#228833",
                   "#4477BB")

# assign names to palette
names(group_palette) <- unique(group_tally$Group)

# plot by number
group_number <- ggplot(data = group_tally, 
                       aes(x = reorder(Group, Number), 
                           y = Number/1000, fill = Group)) +
  geom_col() +
  xlab("") +
  ylab("\nNumber of individuals \nimported (thousands)") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 200, 25)) +
  scale_fill_manual(values = group_palette) +
  coord_flip() +
  theme_bw() +
  labs(tag = "A")

# plot by value
group_value <- ggplot(data = group_tally, 
                      aes(x = reorder(Group, Number), 
                          y = Value_USD/1000, fill = Group)) +
  geom_col() +
  xlab("") +
  ylab("\nTotal value of imports \n($1000 USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = group_palette) +
  coord_flip() +
  theme_bw() +
  labs(tag = "C")

# plot by species
group_spp <- ggplot(data = group_tally, 
                    aes(x = reorder(Group, Number), 
                        y = Species, fill = Group)) +
  geom_col() +
  xlab("") +
  ylab("\nNumber of species imported") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 750, 250)) +
  scale_fill_manual(values = group_palette) +
  coord_flip() +
  theme_bw() +
  labs(tag = "B")

group_plot <- group_number / group_spp / group_value & theme(legend.position = "none")
group_plot

# save
png(filename = here::here("outputs", "plots", "taxa_groups.png"),
    width = 8, height = 10, res = 400, units = "in")
print(group_plot)
dev.off()
