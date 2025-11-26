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
library(rcites)

# Get rid of scientific notation
options(scipen = 9999)

# Load access tokens
source(here::here(".Renviron"))

# Load data --------------------------------------------------------------------

# Current data
import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "invoice-data",
                                            "04a-import-data-marine.csv"))
import_data <- data.table(import_data)

# Correct genus and species
import_data <- separate(import_data,
                        col = Binomial,
                        into = c("Genus", "Species"),
                        sep = c(" "),
                        remove = F)

get_listings <- import_data[, c("Binomial", "Genus", "family", "order")] %>% unique()

# Get gbif backbone taxonomy ---------------------------------------------------

gbif_taxonomy <- data.frame()
for(i in 1:nrow(get_listings)){
  gbif <- try(rgbif::name_backbone(get_listings$Binomial[i],
                                   genus = get_listings$Genus[i],
                                   family = get_listings$family[i],
                                   order = get_listings$order[i]))
  if("data.frame" %in% class(gbif)){
    gbif_taxonomy <- dplyr::bind_rows(gbif_taxonomy, gbif)
  }
  print(i)
}

gbif_taxonomy_results <- gbif_taxonomy[, c("verbatim_name", "verbatim_genus", "verbatim_family", "verbatim_order",
                                           "rank", "class", "order", "family", "genus", "canonicalName")]

# Join
import_data_gbif <- merge.data.table(import_data, gbif_taxonomy,
                                     by.x = c("Binomial", "Genus", "family", "order"),
                                     by.y = c("verbatim_name", "verbatim_genus", "verbatim_family", "verbatim_order"),
                                     suffixes = c("_worms", "_gbif"),
                                     all.x = TRUE) %>% data.table()

# Get IUCN listing -------------------------------------------------------------

# Get unique listings
get_listings <- import_data_gbif[, .(Binomial, 
                                     rank_gbif)] %>% unique()

# Separate genus and species
get_listings <- separate(get_listings,
                         col = Binomial,
                         into = c("Genus", "Species"),
                         sep = c(" "),
                         remove = F) %>% data.table()

# Set column for IUCN listing
get_listings[, IUCN_listing := NA]

# Create progress bar
# There are lots of try states here, otherwise if the API can't find something R 
# throws an error and ends the loop
for(i in 1:nrow(get_listings)){
  listing <- get_listings[i, ]
  # Try retrieving rredlist result
  if(listing$rank_gbif == "SPECIES"){
    iucn <- try(rredlist::rl_species(genus = listing$Genus, species = listing$Species,
                                     key = KEY))
    if(class(iucn) != "try-error"){
      iucn <- try(iucn[["assessments"]])
      iucn <- try(unique(iucn$red_list_category_code))
      get_listings$IUCN_listing[i] <- iucn
    }
  }
  iucn <- NULL
  print(i)
};beep()


# Search for CITES listing -----------------------------------------------------

# Set column for CITES listing
get_listings[, CITES_listing := NA]

# Load cites listings data
# from https://checklist.cites.org/#/en
cites_listings <- read.csv(here::here("data",
                                      "original-data",
                                      "Index_of_CITES_Species_2025-11-26 16_41.csv")) %>%
  data.table()

# Select columns
cites_listings <- cites_listings[, .(FullName, Genus, Species, RankName, CurrentListing)]

# join to listings
get_listings <- merge.data.frame(get_listings,
                                 cites_listings,
                                 by.x = c("Binomial", "Genus", "Species", "rank_gbif"),
                                 by.y = c("FullName", "Genus", "Species", "RankName"),
                                 all.x = TRUE)

# Join to data
data_iucn_cites <- data.table::merge.data.table(import_data_gbif,
                                                get_listings,
                                                by = "Binomial",
                                                all.x = TRUE) %>% data.table()

# Check and correct listings ---------------------------------------------------

# Correct Scleractinians
# All Scleractinians are CITES II listed
data_iucn_cites[order == "Scleractinia",
                cites := "II" ]

# Save -------------------------------------------------------------------------

# Save
data.table::fwrite(data_iucn_cites, here::here("data",
                                               "modified_data",
                                               "invoice_data",
                                               "05-import-data-marine-iucn-cites.csv"),
                   na = NA)
