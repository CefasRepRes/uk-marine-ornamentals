#### Get IUCN and CITES info ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(beepr) # Beep when done
# Taxnonomic packages
library(worrms)
library(rgbif)
# List packages
library(rredlist)
# Get rid of scientific notation
options(scipen = 9999)

# Load access tokens
# You will need an access token from https://api.iucnredlist.org/users/sign_up
source(here::here(".Renviron"))

# Load data --------------------------------------------------------------------

import_data_gbif <- data.table::fread(here::here("data",
                                                 "modified-data",
                                                 "05-import-data-marine-gbif.csv"))

import_data_gbif <- data.table(import_data_gbif)

# Get IUCN listing -------------------------------------------------------------

# Get unique listings
get_listings <- import_data_gbif[, .(canonicalName, 
                                     rank_gbif)] %>% unique()

# Separate genus and species
get_listings <- separate(get_listings,
                         col = canonicalName,
                         into = c("Genus", "Species"),
                         sep = c(" "),
                         remove = F) %>% data.table()

# Remove those not identified to species level
get_listings <- get_listings[rank_gbif == "SPECIES"]

# Set column for IUCN listing
get_listings[, IUCN_listing := NA]

# There are lots of try states here, otherwise if the API can't find something R 
# throws an error and ends the loop
for(i in 1:nrow(get_listings)){
  listing <- get_listings[i, ]
  # Try retrieving rredlist result
  iucn <- try(rredlist::rl_species(genus = listing$Genus, species = listing$Species,
                                   key = KEY))
  if(class(iucn) != "try-error"){
    iucn <- try(iucn[["assessments"]])
    if(length(iucn) > 0){
    iucn <- try(unique(iucn$red_list_category_code))
    get_listings$IUCN_listing[i] <- iucn
    }
  }
  iucn <- NULL
  print(i)
  # Add a 10s pause after every 10th iteration - stops the API overloading
  if (i %% 10 == 0) Sys.sleep(10)
  
};beep()

# Search for CITES listing -----------------------------------------------------

# Load cites listings data
# from https://checklist.cites.org/#/en
cites_listings <- read.csv(here::here("data",
                                      "original-data",
                                      "Index_of_CITES_Species_2025-11-26 16_41.csv")) %>%
  data.table()

# Select columns
cites_listings <- cites_listings[, .(FullName, Genus, Species, RankName, CurrentListing)]

# Rename columns
colnames(cites_listings)[colnames(cites_listings) == "CurrentListing"] <- "CITES_listing"

# join to listings
get_listings <- merge.data.frame(get_listings,
                                 cites_listings,
                                 by.x = c("canonicalName", "Genus", "Species", "rank_gbif"),
                                 by.y = c("FullName", "Genus", "Species", "RankName"),
                                 all.x = TRUE)

# Join to data
data_iucn_cites <- data.table::merge.data.table(import_data_gbif,
                                                get_listings,
                                                by.x = c("canonicalName", "rank_gbif"),
                                                by.y = c("canonicalName", "rank_gbif"),
                                                all.x = TRUE) %>% data.table()

# Check and correct listings ---------------------------------------------------

# Correct Scleractinians
# All Scleractinians are CITES II listed
data_iucn_cites[order_gbif == "Scleractinia", CITES_listing := "II" ]

# save
data.table::fwrite(get_listings, here::here("data",
                                            "modified-data",
                                            "listings.csv"))

# Save -------------------------------------------------------------------------

# Save
data.table::fwrite(data_iucn_cites, here::here("data",
                                               "modified-data",
                                               "06-import-data-marine-iucn-cites.csv"),
                   na = NA)
