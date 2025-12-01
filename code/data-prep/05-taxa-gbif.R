#### Acquiring taxa step 3 - convert to GBIF backbone taxonomy ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
# Taxnonomic packages
library(rgbif)

# Load data --------------------------------------------------------------------

# Current data
import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "04a-import-data-marine.csv"))
import_data <- data.table(import_data)

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
};beep()

gbif_taxonomy_results <- gbif_taxonomy[, c("verbatim_name", "verbatim_genus", "verbatim_family", "verbatim_order",
                                           "rank", "class", "order", "family", "genus", "canonicalName")]

# Join
import_data_gbif <- merge.data.table(import_data, gbif_taxonomy,
                                     by.x = c("Binomial", "Genus", "family", "order"),
                                     by.y = c("verbatim_name", "verbatim_genus", "verbatim_family", "verbatim_order"),
                                     suffixes = c("_worms", "_gbif"),
                                     all.x = TRUE) %>% data.table()

# Save only GBIF verbatim taxonomy ---------------------------------------------

str(import_data_gbif)

import_data_gbif <- import_data_gbif[, .(REF_PDF,
                                         Import_Date,
                                         AWB,
                                         Consignee,
                                         Consignee_Location_PostTown,
                                         Export_company,
                                         Origin_Country,
                                         Origin_region,
                                         Origin_Airport,
                                         Export_Country,
                                         Transit_Airport,
                                         Transit_Country,
                                         Taxa,
                                         OtherInfo,
                                         Developmental_stage,
                                         Size_Cat,
                                         Sex,
                                         Number,
                                         Unit_cost,
                                         Total_Cost,
                                         Currency,
                                         Comments,
                                         Certified_for,
                                         Aquatic_ecosystem,
                                         Wild_Bred,
                                         Inputter,
                                         UID,
                                         Value_USD,
                                         Environment,
                                         rank_gbif,
                                         class_gbif,
                                         class_worms,
                                         order_gbif,
                                         family_gbif,
                                         genus_gbif,
                                         species_gbif,
                                         canonicalName)]

# save 
data.table::fwrite(import_data_gbif, here::here("data",
                                                "modified-data",
                                                "05-import-data-marine-gbif.csv"),
                   row.names = FALSE,
                   na = NA)
