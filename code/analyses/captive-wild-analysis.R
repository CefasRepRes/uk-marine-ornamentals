#### Captive/wild analyses ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(stringr) # String handling
library(ggplot2) # Plotting

# Get rid of scientific notation
options(scipen = 9999)

# Run tables code --------------------------------------------------------------

source(here::here("code", "analyses", "taxa-analysis.R"))

# Value per unit ---------------------------------------------------------------

# Calculate value per unit
import_data$Value_USD_Unit <- import_data$Value_USD / import_data$Number

# Summarise for species
species_values <- import_data[, by = c("Binomial", "Family", "Order"),
                              .(mean = mean(Value_USD_Unit, na.rm = T),
                                sd = sd(Value_USD_Unit, na.rm = T),
                                min = min(Value_USD_Unit, na.rm = T),
                                max = max(Value_USD_Unit, na.rm = T))] 
species_values[, 4:7] <- round(species_values[, 4:7], digits = 2)

# Create function to summarise
val.unit.tally <- function(data, grouping){
  data <- data.table(data)
  tally <- data[, by = c("Binomial", "Family", "Order"),
                .(mean = mean(Value_USD_Unit, na.rm = T),
                  sd = sd(Value_USD_Unit, na.rm = T),
                  min = min(Value_USD_Unit, na.rm = T),
                  max = max(Value_USD_Unit, na.rm = T))] 
  cols <- c("Binomial", grouping)
  tally_spp <- data[, ..cols] %>% unique()
  tally_spp <- tally_spp[, by = c(grouping),
                         .N] 
  tally_merge <- merge.data.table(tally, tally_spp, by = grouping)
  tally_merge$mean <- round(tally_merge$mean / sum(tally_merge$mean) * 100, digits = 1)
  tally_merge$sd <- round(tally_merge$Value_USD / sum(tally_merge$Value_USD) * 100, digits = 1)
  tally_merge$per_spp <- round(tally_merge$N / sum(tally_merge$N) * 100, digits = 1)
  colnames(tally_merge)[colnames(tally_merge) == "N"] <- "Species"
  tally_merge$Value_USD <- round(tally_merge$Value_USD)
  tally_merge <- tally_merge[order(-Number)]
  return(tally_merge)
}

# Origin comparison ------------------------------------------------------------

# Update designations
import_data$Captive_wild[import_data$Captive_wild == ""] <- "Unknown"
import_data$Captive_wild[is.na(import_data$Captive_wild)] <- "Unknown"
import_data$Captive_wild[import_data$Captive_wild == "wild_declared"] <- "Declared wild"
import_data$Captive_wild[import_data$Captive_wild == "farmed_declared"] <- "Declared farmed"
import_data$Captive_wild[import_data$Captive_wild %in% c("farmed_likely",
                                                         "farmed_supplier",
                                                         "part_farmed_supplier")] <- "Likely farmed"

# Banggai cardinalfish only
banggai_captive <- num.val.spp.tally(import_data[Binomial == "Pterapogon kauderni"],
                                     "Captive_wild")

# Red sea goby only
amikami_captive <- num.val.spp.tally(import_data[Binomial == "Callogobius amikami"],
                                     "Captive_wild")

# Fish only
fish_captive <- num.val.spp.tally(import_data[Group == "Bony fishes"],
                                  "Captive_wild")

# Verts only
verts_captive <- num.val.spp.tally(import_data[Group == "Other vertebrates"],
                                   "Captive_wild")

# Inverts only
inverts_captive <- num.val.spp.tally(import_data[Group == "Other invertebrates"],
                                     "Captive_wild")

# Stony corals
corals_captive <- num.val.spp.tally(import_data[Group == "Stony corals"],
                                    "Captive_wild")

# Algae
algae_captive <- num.val.spp.tally(import_data[Group == "Algae"],
                                    "Captive_wild")


# Threatened species only
threatened_captive <- num.val.spp.tally(import_data[IUCN_listing %chin% c("EN", "VU", "CR")],
                                        "Captive_wild")

# CITES species only
cites_captive <- num.val.spp.tally(import_data[!is.na(CITES_listing) &
                                                 CITES_listing != ""],
                                   "Captive_wild")

# All taxa
captive_overall <- num.val.spp.tally(import_data,
                                     "Captive_wild")

# Plot -------------------------------------------------------------------------

# Create palette
captive_palette <- c("#88CCEE",
                     "#DDAA33",
                     "#EE7733",
                     "#4477AA")
names(captive_palette) <- c("Unknown", "Likely farmed", 
                            "Declared farmed", "Declared wild")

# Create donut plot function
donutPlot <- function(data_frame, title){
  donut <- data.frame(captive_bred = data_frame[, Captive_wild],
                      ymax = cumsum(data_frame[, per_num]),
                      y = data_frame[, per_num]) %>% data.table()
  donut$ymin <- c(0, head(donut[, ymax], n = -1))
  donut_plot <- ggplot(donut, aes(ymax = ymax, 
                                  ymin = ymin, 
                                  xmax = 4, 
                                  xmin = 3.5, 
                                  fill = captive_bred)) +
    geom_rect() +
    scale_fill_manual(values = captive_palette, name = "Farmed or wild") +
    coord_polar(theta = "y") +
    xlim(c(3, 4)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(title) 
  return(donut_plot)
}

# Endangered -------------------------------------------------------------------

# Plot for endangered species
unique(import_data[IUCN_listing == "EN"]$Binomial)

banggai_donut <- donutPlot(banggai_captive, "Banggai cardinalfish")
amikami_donut <- donutPlot(amikami_captive, "Amikami goby")
endangered_donut <- plot_spacer() | banggai_donut | amikami_donut | plot_spacer() 
endangered_donut

# Save plot
png(filename = here::here("outputs", "plots", "endangered_donut.png"),
    width = 8, height = 6, res = 400, units = "in")
print(endangered_donut)
dev.off()

# Groups -----------------------------------------------------------------------

# Plot for each group
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

# Listings ---------------------------------------------------------------------

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

# Overall ----------------------------------------------------------------------

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
