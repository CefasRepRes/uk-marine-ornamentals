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
                                            "invoice-data",
                                            "05-import-data-marine-iucn-cites.csv"))

get_listings <- import_data[rank_gbif %chin% c("GENUS", "SPECIES",
                                               "SUBSPECIES"), c("canonicalName", "Origin_Country", "Wild_Bred")] %>%
  unique()

# Find if likely captive bred by native range ----------------------------------

# robis 

# Create progress bar
pb <- progress_bar$new(total = nrow(get_listings))
countries <- data.frame()
# Loop over
for(i in 1:nrow(get_listings)){
  # Get range if not already retrieved
  # This step saves time for taxa with a lot of records
  if(!(get_listings$canonicalName[i] %in% countries$scientificName)){
    countries <- try(robis::occurrence(get_listings$canonicalName[i]))
    # Only subset if country results are returned
    if("country" %in% colnames(countries)){
      countries <- unique(countries[, c("scientificName",
                                        "country")])
    }
  }
  # If outside range - likely captive bred
  if(!(get_listings$Origin_Country[i] %chin% countries$country) &&
     is.data.frame(countries)){
    get_listings$country_captive_bred[i] <- "likely"} else if(length(countries > 0)){
      get_listings$country_captive_bred[i] <- NA
    }
  # Reset the countries data frame if not found
  # Enables loop to continue
  if(!is.data.frame(countries)){
    countries <- data.frame(scientificName = NA,
                            countries = NA)
  }
  # Print for tracking
  pb$tick()
  print(i)
}

# Save
data.table::fwrite(get_listings, here::here("data",
                                            "modified-data",
                                            "invoice-data",
                                            "06a-import-data-marine-captive-wild-robis.csv"),
                   row.names = F,
                   na = NA)



# Combine with existing data
get_listings_captive <- data.table::merge.data.table(import_data,
                                                     get_listings,
                                                     by = c("canonicalName", 
                                                            "Origin_Country",
                                                            "Wild_Bred"),
                                                     all.x = TRUE)

# Add in supplier information --------------------------------------------------

# Read
supplier <- fread(here::here("data",
                             "original-data",
                             "supplier-species.csv")) %>% data.table()

# Select columns
supplier <- supplier[, -"Supply chain"]
colnames(supplier)[1] <- "Binomial"

# Join
get_listings_captive <- data.table::merge.data.table(get_listings_captive,
                                                     supplier,
                                                     by = c("Binomial",
                                                            "Origin_Country",
                                                            "Common_Name",
                                                            "Consignee",
                                                            "Export_company"),
                                                     all.x = TRUE)

# Find if captive --------------------------------------------------------------

# Mark those which are cultured
import_data$Wild_Bred[import_data$Common_Name %like% "culture"] <- "cultured"

# Mark captive bred
get_listings_captive$country_captive_bred[get_listings_captive$`Cultured (yes or no)` == "Y SOME"] <- "part_farmed_supplier"
get_listings_captive$country_captive_bred[get_listings_captive$`Cultured (yes or no)` == "Y"] <- "farmed_supplier"
get_listings_captive$country_captive_bred[get_listings_captive$`Cultured (yes or no)` == "MC"] <- "farmed_supplier"
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
                                                    "invoice-data",
                                                    "06-import-data-marine-captive-wild.csv"),
                   row.names = F,
                   na = NA)
