#### Maps ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(data.table) # Better data wrangling
library(magrittr) # Pipes %>%
library(ggplot2) # Plotting
# Maps
library(geosphere)
library(maps)
# Community analyses
library(vegan)
library(labdsv)

# Get rid of scientific notation
options(scipen = 9999)

# source functions
source(here::here("functions", "numValSppTally.R"))
source(here::here("functions", "connectionMap.R"))

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "import-data-final.csv"))
import_data <- data.table(import_data)

# Data prep --------------------------------------------------------------------

# aggregate by country and taxonomic group
country_group <- numValSppTally(import_data, 
                                grouping = c("Origin_country", "Group"))

# get a list of countries
countries <- as.data.frame(unique(country_group$Origin_country))
colnames(countries) <- "countries"

# load centroids
# from https://github.com/gavinr/world-countries-centroids
centroids <- data.table::fread(here::here("data",
                                          "original-data",
                                          "country-centroids.csv"))

# join to data
countries <- merge.data.table(countries, centroids[, 1:3], by.x = "countries",
                              by.y = "COUNTRY", all.x = TRUE) %>% data.table()

# fill in Taiwan and USA
countries[countries == "USA",
          `:=` (latitude = centroids[COUNTRY == "United States", latitude],
                longitude = centroids[COUNTRY == "United States", longitude])]
countries[countries == "Taiwan",
          `:=` (latitude = 23.9738611,
                longitude = 120.982)]

# extract United Kingdom
gbr <- centroids[COUNTRY == "United Kingdom"]

# create paths
path <- data.frame()
for(i in 1:nrow(countries)){
  inter <- geosphere::gcIntermediate(countries[i, c(longitude, latitude)],
                                     gbr[, c(longitude, latitude)],
                                     n = 50, addStartEnd = TRUE,
                                     breakAtDateLine = TRUE)
  if("list" %in% class(inter)){
    inter_1 <- as.data.frame(inter[[1]])
    inter_1$country <- paste0(countries[i, countries], "_1")
    inter_2 <- as.data.frame(inter[[2]])
    inter_2$country <- paste0(countries[i, countries], "_2")
    inter <- rbind(inter_1, inter_2)
    inter$Origin_country <- countries[i, countries]
  } else {
    inter <- as.data.frame(inter)
    inter$country <- countries[i, countries]
    inter$Origin_country <- countries[i, countries]
  }
  path <- rbind(path, inter)
}
path <- data.table(path)

# Plots ------------------------------------------------------------------------

# fish
fish_map <- connectionMap(group = "Bony fishes", colour = "#66CCEE", tags = c("A", "B"))
fish_map
png(filename = here::here("outputs", "plots", "fish_map.png"),
    width = 8, height = 3, res = 400, units = "in")
print(fish_map)
dev.off()

# verts
verts_map <- connectionMap(group = "Other vertebrates", colour = "#4477BB", tags = c("C", "D"))
verts_map
png(filename = here::here("outputs", "plots", "verts_map.png"),
    width = 8, height = 3, res = 400, units = "in")
print(verts_map)
dev.off()

# stony corals
corals_map <- connectionMap(group = "Stony corals", colour = "#CCBB44", tags = c("E", "F"))
corals_map
png(filename = here::here("outputs", "plots", "corals_map.png"),
    width = 8, height = 3, res = 400, units = "in")
print(corals_map)
dev.off()

# inverts
inverts_map <- connectionMap(group = "Other invertebrates", colour = "#EE6677", tags = c("G", "H"))
inverts_map
png(filename = here::here("outputs", "plots", "inverts_map.png"),
    width = 8, height = 3, res = 400, units = "in")
print(inverts_map)
dev.off()

# algae
algae_map <- connectionMap(group = "Other invertebrates", colour = "#228833", tags = c("I", "J"))
algae_map
png(filename = here::here("outputs", "plots", "algae_map.png"),
    width = 8, height = 3, res = 400, units = "in")
print(algae_map)
dev.off()
