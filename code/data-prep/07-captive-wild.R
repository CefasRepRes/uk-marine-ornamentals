#### Assign captive/wild ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
# Location packages
library(robis)

# Get rid of scientific notation
options(scipen = 9999)

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "06-import-data-marine-iucn-cites.csv"))

get_listings <- import_data[rank_gbif %chin% c("GENUS", "SPECIES",
                                               "SUBSPECIES"), c("canonicalName", "Origin_Country", "Wild_Bred")] %>%
  unique()

# Load retrieved listings
retrieved_listings <- read.csv(here::here("data",
                                          "modified-data",
                                          "origin-country-captive.csv"))

# Get those not found previously
new_listings <- anti_join(get_listings, retrieved_listings, by = c("canonicalName", "Origin_Country"))

# Find if likely captive bred by native range ----------------------------------

# robis 
countries <- data.frame()
# Loop over
for(i in 223:nrow(new_listings)){
  # Get range if not already retrieved
  # This step saves time for taxa with a lot of records
  if(!(new_listings$canonicalName[i] %in% countries$scientificName) |
     !(new_listings$canonicalName[i] == new_listings$canonicalName[i - 1])){
    countries <- try(robis::occurrence(new_listings$canonicalName[i]))
    # Only subset if country results are returned
    if("country" %in% colnames(countries)){
      countries <- unique(countries[, c("scientificName",
                                        "country")])
    }
  }
  # If outside range - likely captive bred
  if(!(new_listings$Origin_Country[i] %chin% countries$country) &&
     is.data.frame(countries)){
    new_listings$country_captive_bred[i] <- "likely"} else if(length(countries > 0)){
      new_listings$country_captive_bred[i] <- NA
    }
  # Reset the countries data frame if not found
  # Enables loop to continue
  if(!is.data.frame(countries)){
    countries <- data.frame(scientificName = NA,
                            countries = NA)
  }
  # Print for tracking
  print(i)
};beep()

# Append to listings
get_listings <- rbind(retrieved_listings, new_listings)

# Save
data.table::fwrite(get_listings, here::here("data",
                                            "modified-data",
                                            "origin-country-captive.csv"),
                   row.names = FALSE,
                   na = NA)



# Combine with existing data
get_listings_captive <- data.table::merge.data.table(import_data,
                                                     get_listings,
                                                     by = c("canonicalName", 
                                                            "Origin_Country",
                                                            "Wild_Bred"),
                                                     all.x = TRUE)

# Find if captive --------------------------------------------------------------

# Mark those which are cultured
get_listings_captive$Wild_Bred[get_listings_captive$Common_Name %like% "culture"] <- "cultured"

# Mark captive bred
get_listings_captive$country_captive_bred[get_listings_captive$country_captive_bred == "likely"] <- "farmed_likely"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "cultured"] <- "farmed_declared"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "tank bred"] <- "farmed_declared"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "Farm Raised"] <- "farmed_declared"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "Cultured"] <- "farmed_declared"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "WILD"] <- "wild_declared"
get_listings_captive$country_captive_bred[get_listings_captive$Wild_Bred == "Wild"] <- "wild_declared"

# Save -------------------------------------------------------------------------

data.table::fwrite(get_listings_captive, here::here("data",
                                                    "modified-data",
                                                    "07-import-data-marine-captive-wild.csv"),
                   row.names = FALSE,
                   na = NA)
