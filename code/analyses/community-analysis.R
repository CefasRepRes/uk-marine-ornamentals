#### Community analyses ####

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

# Run tables code --------------------------------------------------------------

source(here::here("code", "analyses", "taxa-analysis.R"))

# Species accumulation ---------------------------------------------------------

species_records_tally <- import_data[Rank %in% c("SPECIES", "SUBSPECIES")][, c("AWB_ID", "Binomial", "Group")] %>% unique()
species_records_tally <- species_records_tally[, by = c("Binomial", "Group"), .N]

species_records <- num.val.spp.tally(data = import_data[Rank %in% c("SPECIES", "SUBSPECIES")],
                                     grouping = c("AWB_ID",
                                                  "Binomial"))

species_records <- species_records[, c("AWB_ID",
                                       "Binomial",
                                       "Number")]

species_records <- matrify(species_records)

species_records_accum <- specaccum(species_records,
                                   method = "random",
                                   ci.type = "polygon")

species_records_accum_df <- data.frame(records = species_records_accum$sites,
                                       richness = species_records_accum$richness,
                                       rich_min = species_records_accum$richness - 2 * species_records_accum$sd,
                                       rich_max = species_records_accum$richness + 2 * species_records_accum$sd)

# Species accumulation curve ---------------------------------------------------

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
  scale_y_continuous(breaks = seq(0, 1200, 200)) +
  theme_bw() +
  theme(text = element_text(size = 15))
spec_accum_plot

# save
png(filename = here::here("outputs", "plots", "spec_accum_plot.png"),
    width = 8, height = 6, res = 400, units = "in")
print(spec_accum_plot)
dev.off()

# chao estimator
chao <- specpool(x = species_records)
chao

# Diversity by country ---------------------------------------------------------

country_tally <- num.val.spp.tally(data = import_data,
                                   grouping = "Origin_country")

country_species <- num.val.spp.tally(data = import_data[Rank %in% c("SPECIES", "SUBSPECIES")],
                                     grouping = c("Origin_country",
                                                  "Binomial"))

country_species <- country_species[, c("Origin_country",
                                       "Binomial",
                                       "Number")]

country_species <- matrify(country_species)

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

# Aggregate by group
country_group <- num.val.spp.tally(import_data, grouping = c("Origin_country", "Group"))

# Aggregate within groups
country_fish <- num.val.spp.tally(import_data[Group == "Bony fishes"], grouping = c("Origin_country"))
country_verts <- num.val.spp.tally(import_data[Group == "Other vertebrates"], grouping = c("Origin_country"))
country_corals <- num.val.spp.tally(import_data[Group == "Stony corals"], grouping = c("Origin_country"))
country_inverts <- num.val.spp.tally(import_data[Group == "Other invertebrates"], grouping = c("Origin_country"))
