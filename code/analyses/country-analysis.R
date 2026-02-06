#### Maps ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(stringr) # String handling
library(ggplot2) # Plotting
# Maps
library(geosphere)
library(maps)
# Community analyses
library(vegan)
library(labdsv)

# Get rid of scientific notation
options(scipen = 9999)

# Run tables code --------------------------------------------------------------

source(here::here("code", "analyses", "taxa-analysis.R"))

# Plot -------------------------------------------------------------------------

# Aggregate by group
country_group <- num.val.spp.tally(import_data, grouping = c("Origin_country", "Group"))

# Countries
countries <- as.data.frame(unique(country_group$Origin_country))
colnames(countries) <- "countries"

# Load centroids
centroids <- data.table::fread(here::here("data",
                                          "original-data",
                                          "country-centroids.csv"))

# Join
countries <- merge.data.table(countries, centroids[, 1:3], by.x = "countries",
                              by.y = "COUNTRY", all.x = TRUE) %>% data.table()

# Fill in Taiwan and USA
countries[countries == "USA",
          `:=` (latitude = centroids[COUNTRY == "United States", latitude],
                longitude = centroids[COUNTRY == "United States", longitude])]
countries[countries == "Taiwan",
          `:=` (latitude = 23.9738611,
                longitude = 120.982)]

gbr <- centroids[COUNTRY == "United Kingdom"]

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

# World map
world <- map_data("world")

# Create function
connectionMap <- function(group, colour, tags){
  # extract data 
  map_data <- country_group[Group == group, c("Origin_country", "Number", "Value_USD")]
  
  # extract points
  map_points <- merge.data.table(map_data, countries, by.x = "Origin_country",
                                 by.y = "countries")
  # extract path
  map_path <- merge.data.table(map_data, path, by = "Origin_country")
  
  # create map by number
  map_number <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "#DDDDDD", col = "white") +
    geom_path(data = map_path, aes(x = lon, y = lat, group = country, linewidth = Number),
              alpha = 0.5, col = colour) +
    geom_point(data = map_points, aes(x = longitude, y = latitude,
                                      size = Number), col = colour) +
    scale_size(labels = scales::comma) +
    scale_linewidth(labels = scales::comma) +
    theme_void() +
    labs(tag = tags[1])
  theme(legend.position = "bottom")
  
  map_value <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "#DDDDDD", col = "white") +
    geom_path(data = map_path, aes(x = lon, y = lat, group = country, linewidth = Value_USD),
              alpha = 0.5, col = colour) +
    geom_point(data = map_points, aes(x = longitude, y = latitude,
                                      size = Value_USD), col = colour) +
    scale_size(name = "Value ($ USD)", labels = scales::comma) +
    scale_linewidth(name = "Value ($ USD)", labels = scales::comma) +
    theme_void() +
    labs(tag = tags[2])
  theme(legend.position = "bottom")
  
  return(map_number + map_value)
}

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
