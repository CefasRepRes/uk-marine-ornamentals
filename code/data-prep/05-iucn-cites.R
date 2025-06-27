#### Get IUCN and CITES info ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
# Taxnonomic packages
library(worrms)
library(rgbif)
# List packages
library(rredlist)
library(rcites)

# Get rid of scientific notation
options(scipen = 9999)

# Remember to set access tokens

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
                                     all.x = TRUE)

# Get IUCN listing -------------------------------------------------------------

get_listings <- import_data_gbif$canonicalName %>% unique()

# Create progress bar
pb <- progress_bar$new(total = length(get_listings))
# There are lots of try states here, otherwise if the API can't find something R 
# throws an error and ends the loop
iucn_listings <- data.frame()
for(i in 1:length(get_listings)){
  # Try retrieving rredlist result
  iucn <- try(rredlist::rl_search(name = as.character(get_listings[i])))
  iucn <- try(iucn[["result"]])
  iucn_listings <- try(rbind(iucn_listings, iucn))
  pb$tick()
  print(i)
};beep()

# Select fields of interest
iucn_listings_tidy <- iucn_listings[, c("scientific_name",
                                        "category",
                                        "criteria",
                                        "population_trend")] %>% unique()

colnames(iucn_listings_tidy)[2] <- "iucn_listing"

# Search for CITES listing -----------------------------------------------------

# Create progress bar
pb <- progress_bar$new(total = length(get_listings))
# Loop over 
cites_listings <- data.frame(listing = get_listings,
                             cites = NA) %>% data.table()
cites_listings$cites <- as.character(cites_listings$cites)
for(i in 1:nrow(cites_listings)){
  cites <- try(rcites::spp_taxonconcept(query_taxon = cites_listings$listing[i],
                                        verbose = F))
  if(class(cites) == "spp_taxon"){
    cites <- cites[["general"]]
    cites_result <- cites$cites_listing
    cites_listings[i, cites := cites_result]
  }
  pb$tick()
  print(i)
}

# Join to data -----------------------------------------------------------------

# Combine with previous listings
listings_iucn_cites <- merge.data.frame(iucn_listings_tidy,
                                        cites_listings,
                                        by.x = "scientific_name",
                                        by.y = "listing",
                                        all = TRUE) %>% unique()


# Check for duplicates
listings_dupes <- rbind(dplyr::filter(listings_iucn_cites, duplicated(scientific_name)),
                        dplyr::filter(listings_iucn_cites, duplicated(scientific_name, fromLast = TRUE)))

# Join to data
data_iucn_cites <- data.table::merge.data.table(import_data_gbif,
                                                listings_iucn_cites,
                                                by.x = "Binomial",
                                                by.y = "scientific_name",
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
