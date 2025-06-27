#### Tables ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(stringr) # String handling
# Taxonomic packages
library(rfishbase)
library(worrms)
# Plotting
library(ggplot2)
library(patchwork)

# Get rid of scientific notation
options(scipen = 9999)

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "invoice-data",
                                            "07-import-data-final.csv"))
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

# Create function to aggregate number, value and tally -------------------------

num.val.spp.tally <- function(data, grouping){
  # Convert to data table
  data <- data.table(data) 
  # Tally by species
  tally <- data[, by = c(grouping),
                .(Number = sum(Number, na.rm = T),
                  Value_USD = sum(Value_USD, na.rm = T),
                  Consignments = uniqueN(AWB_ID))]
  # Tally by grouping
  cols <- c("Binomial", grouping)
  tally_spp <- data[, ..cols] %>% unique()
  tally_spp <- tally_spp[, b = c(grouping),
                         .N] 
  total_con <- uniqueN(data$AWB_ID)
  # Join
  tally_merge <- merge.data.table(tally, tally_spp, by = grouping)
  # Get percentages
  tally_merge$per_num <- round(tally_merge$Number / sum(tally_merge$Number) * 100, digits = 1)
  tally_merge$per_val <- round(tally_merge$Value_USD / sum(tally_merge$Value_USD) * 100, digits = 1)
  tally_merge$per_con <- round((tally_merge$Consignments / total_con) * 100, digits = 1)
  tally_merge$per_spp <- round(tally_merge$N / sum(tally_merge$N) * 100, digits = 1)
  colnames(tally_merge)[colnames(tally_merge) == "N"] <- "Species"
  tally_merge$Value_USD <- round(tally_merge$Value_USD)
  # Order by number
  tally_merge <- tally_merge[order(-Number)]
  # Get cumulative summary
  tally_merge$cumsum_num <- cumsum(tally_merge$per_num)
  return(tally_merge)
}

# Species ----------------------------------------------------------------------

# Species tally
spp_tally <- num.val.spp.tally(data = import_data[Rank %in% c("SPECIES", "SUBSPECIES")],
                               grouping = c("Binomial", "Family", "Order", "Group"))

# Most common import species
top_spp <- spp_tally[1]

# Number of species imported once
imported_once <- spp_tally[spp_tally$Number == 1] %>% nrow()

# Class ------------------------------------------------------------------------

class_tally <- num.val.spp.tally(data = import_data,
                                 grouping = "Class")

# Group ------------------------------------------------------------------------

group_tally <- num.val.spp.tally(data = import_data,
                                 grouping = "Group")

# plot
group_palette <- c("#66CCEE",
                   "#4477BB",
                   "#228833",
                   "#CCBB44")
names(group_palette) <- unique(group_tally$Group)

group_number <- ggplot(data = group_tally, aes(x = reorder(Group, Number), y = Number/1000, fill = Group)) +
  geom_col() +
  xlab("") +
  ylab("\nNumber of individuals \nimported (thousands)") +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(0, 200, 25)) +
  scale_fill_manual(values = group_palette) +
  coord_flip() +
  theme_bw() +
  labs(tag = "A")

group_value <- ggplot(data = group_tally, aes(x = reorder(Group, Number), y = Value_USD/1000, fill = Group)) +
  geom_col() +
  xlab("") +
  ylab("\nTotal value of imports \n($1000 USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = group_palette) +
  coord_flip() +
  theme_bw() +
  labs(tag = "C")

group_spp <- ggplot(data = group_tally, aes(x = reorder(Group, Number), y = Species, fill = Group)) +
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

# Conservation listing ---------------------------------------------------------

## IUCN ========================================================================

# Species by IUCN listing
iucn_species_count <- num.val.spp.tally(data = import_data,
                                        grouping = "IUCN_listing")

# Conservation listing groups
iucn_group_count <- num.val.spp.tally(data = import_data,
                                      grouping = c("IUCN_listing", "Group"))

# Threatened by group
threatened_group <- num.val.spp.tally(data = import_data[import_data$IUCN_listing %chin% c("EN", "CR", "VU")],
                                      grouping = c("IUCN_listing", "Group"))

# Threatened overall
threatened_taxa <- import_data[IUCN_listing %chin% c("EN", "CR", "VU")][, c("Binomial",
                                                                            "Family",
                                                                            "Order",
                                                                            "Class",
                                                                            "IUCN_listing",
                                                                            "Population_trend",
                                                                            "CITES_listing")][order(IUCN_listing)] %>%
  unique()
threatened_taxa <- merge(threatened_taxa,
                         spp_tally,
                         by = "Binomial")
threatened_taxa$CITES_listing[threatened_taxa$CITES_listing == "NC"] <- NA

## CITES =======================================================================

# Species by CITES listing
spp_cites <- num.val.spp.tally(data = import_data,
                               grouping = "CITES_listing")

# CITES listing
cites_group <- num.val.spp.tally(data = import_data,
                                 grouping = c("CITES_listing", "Group"))

# By group ---------------------------------------------------------------------

## Fish ========================================================================

marine_fish_families <- num.val.spp.tally(data = import_data[Group == "Bony fishes"],
                                          grouping = c("Family",
                                                       "Order",
                                                       "Class"))

marine_fish_species <- num.val.spp.tally(data = import_data[Group == "Bony fishes"],
                                         grouping = c("Binomial",
                                                      "Family",
                                                      "Order",
                                                      "Class"))

# Top 10 by number
marine_fish_families_number <- head(marine_fish_families[order(-Number)], n = 10)

# Create palette
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

names(fish_fam_palette) <- c(unique(marine_fish_families_number$Family), 
                             "Microdesmidae")

marine_number <- ggplot(marine_fish_families_number,
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

# Top 10 by value
marine_fish_families_value <- head(marine_fish_families[order(-Value_USD)], n = 10)

marine_value <- ggplot(marine_fish_families_value,
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

marine_fish <- marine_number + marine_value & theme(legend.position = "none")
marine_fish

# save
png(filename = here::here("outputs", "plots", "marine_fish_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(marine_fish)
dev.off()

## Other verts =================================================================

marine_verts_families <- num.val.spp.tally(data = import_data[Group == "Other vertebrates"],
                                           grouping = c("Family",
                                                        "Order",
                                                        "Class"))

marine_verts_species <- num.val.spp.tally(data = import_data[Group == "Other vertebrates"],
                                          grouping = c("Binomial",
                                                       "Family",
                                                       "Order",
                                                       "Class"))

# Create palette
verts_fam_palette <- c("#999933",
                       "#DDCC77",
                       "#CC6677",
                       "#882255")

names(verts_fam_palette) <- unique(marine_verts_families$Family)

marine_verts_number <- ggplot(marine_verts_families,
                              aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals imported") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = verts_fam_palette) +
  theme_bw() +
  labs(tag = "A")

marine_verts_value <- ggplot(marine_verts_families,
                             aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family") +
  ylab("\n Total value of imports \n ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = verts_fam_palette) +
  theme_bw() +
  labs(tag = "B")

marine_verts <- marine_verts_number + marine_verts_value & theme(legend.position = "none")
marine_verts

# save
png(filename = here::here("outputs", "plots", "marine_verts_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(marine_verts)
dev.off()

## Stony corals ================================================================

marine_corals_families <- num.val.spp.tally(data = import_data[Group == "Stony corals"],
                                            grouping = c("Family",
                                                         "Order",
                                                         "Class"))

marine_corals_species <- num.val.spp.tally(data = import_data[Group == "Stony corals"],
                                           grouping = c("Binomial",
                                                        "Family",
                                                        "Order",
                                                        "Class"))

# Add in unidentified
marine_corals_families[Family == "", Family := "Unidentified"]
marine_corals_species[Family == "", Family := "Unidentified"]

# Top 10 by number
marine_corals_families_number <- head(marine_corals_families[order(-Number)], n = 10)

# Create palette
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

names(coral_fam_palette) <- unique(marine_corals_families_number$Family)

stony_corals_number <- ggplot(marine_corals_families_number,
                              aes(x = reorder(Family, Number), y = Number, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = coral_fam_palette) +
  theme_bw() +
  labs(tag = "A")

# Top 10 by value
marine_corals_families_value <- head(marine_corals_families[order(-Value_USD)], n = 10)

stony_corals_value <- ggplot(marine_corals_families_value,
                             aes(x = reorder(Family, Value_USD), y = Value_USD, fill = Family)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = coral_fam_palette) +
  theme_bw() +
  labs(tag = "B")

stony_corals <- stony_corals_number + stony_corals_value & theme(legend.position = "none")
stony_corals

# save
png(filename = here::here("outputs", "plots", "stony_corals_families.png"),
    width = 8, height = 4, res = 400, units = "in")
print(stony_corals)
dev.off()

## Other inverts ===============================================================

marine_inverts_orders <- num.val.spp.tally(data = import_data[Group == "Other invertebrates"],
                                           grouping = c("Order",
                                                        "Class"))

marine_inverts_families <- num.val.spp.tally(data = import_data[Group == "Other invertebrates"],
                                             grouping = c("Family",
                                                          "Order",
                                                          "Class"))

marine_inverts_species <- num.val.spp.tally(data = import_data[Group == "Other invertebrates"],
                                            grouping = c("Binomial",
                                                         "Family",
                                                         "Order",
                                                         "Class"))

# Top 10 by number
marine_inverts_order_number <- head(marine_inverts_orders[order(-Number)], n = 10)

# Create palette
inverts_order_palette <- c("#332288",
                           "#0077BB",
                           "#33BBEE",
                           "#88CCEE",
                           "#44AA99",
                           "#117733",
                           "#999933",
                           "#DDCC77",
                           "#EE7733",
                           "#CC3311",
                           "#CC6677",
                           "#882255",
                           "#AA4499")

names(inverts_order_palette) <- c(unique(marine_inverts_order_number$Order),
                                  "Cardiida", "Scleralcyonacea")

inverts_number <- ggplot(marine_inverts_order_number,
                         aes(x = reorder(Order, Number), y = Number, fill = Order)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = inverts_order_palette) +
  theme_bw() +
  labs(tag = "A")

# Top 10 by value
marine_inverts_order_value <- head(marine_inverts_orders[order(-Value_USD)], n = 10)

inverts_value <- ggplot(marine_inverts_order_value,
                        aes(x = reorder(Order, Value_USD), y = Value_USD, fill = Order)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = inverts_order_palette) +
  theme_bw() +
  labs(tag = "B")

inverts <- inverts_number + inverts_value & theme(legend.position = "none")
inverts

# save
png(filename = here::here("outputs", "plots", "marine_inverts_orders.png"),
    width = 8, height = 4, res = 400, units = "in")
print(inverts)
dev.off()
