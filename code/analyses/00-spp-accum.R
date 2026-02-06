#### Species accumulation ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(data.table) # Better data wrangling
library(magrittr) # Pipes %>%
library(ggplot2) # Plotting
library(patchwork) # Plot arranging
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

# subset to species and subspecies
spp_dat <- import_data[Rank %in% c("SPECIES", "SUBSPECIES")]

# Data preparation -------------------------------------------------------------

# tally species
species_records_tally <- spp_dat[, c("AWB_ID", "Binomial", "Group")] %>% unique()
species_records_tally <- species_records_tally[, by = c("Binomial", "Group"), .N]

# get species records 
species_records <- numValSppTally(data = spp_dat, grouping = c("AWB_ID",
                                                               "Binomial"))

# select columns
species_records <- species_records[, c("AWB_ID",
                                       "Binomial",
                                       "Number")]

# make into matrix
species_records <- matrify(species_records)

# Species accumulation ---------------------------------------------------------

# get spp accumulation
species_records_accum <- vegan::specaccum(species_records,
                                          method = "random",
                                          ci.type = "polygon")

# make into data frame
species_records_accum_df <- data.frame(records = species_records_accum$sites,
                                       richness = species_records_accum$richness,
                                       rich_min = species_records_accum$richness - 2 * species_records_accum$sd,
                                       rich_max = species_records_accum$richness + 2 * species_records_accum$sd)

# get Chao estimator
chao <- vegan::specpool(x = species_records)
chao

# Species accumulation curve ---------------------------------------------------

# plot
spec_accum_plot <- ggplot(species_records_accum_df, aes(x = records,
                                                        y = richness)) +
  geom_line() +
  geom_ribbon(aes(x = records,
                  ymin = rich_min,
                  ymax = rich_max),
              alpha = 0.2) +
  xlab("\nNumber of records") +
  ylab("Species richness\n") +
  scale_x_continuous(breaks = seq(0, 300, 50)) +
  scale_y_continuous(breaks = seq(0, 1400, 200)) +
  theme_bw() +
  theme(text = element_text(size = 15))
spec_accum_plot

# save
png(filename = here::here("outputs", "plots", "spec_accum_plot.png"),
    width = 8, height = 6, res = 400, units = "in")
print(spec_accum_plot)
dev.off()

