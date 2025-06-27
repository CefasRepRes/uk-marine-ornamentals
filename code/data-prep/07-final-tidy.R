#### Tidy data for analyses #####

# Load libraries
library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation

# Load data --------------------------------------------------------------------

# Load data
import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "invoice-data",
                                            "06-import-data-marine-captive-wild.csv"))

str(import_data)

# Tidy data --------------------------------------------------------------------

# Replace GBIF class with WORMS class where there is no GBIF class
import_data[is.na(class_gbif), class_gbif := class_worms]

# Select columns of interest
import_data_tidy <- import_data[, c("UID", "AWB", "canonicalName", 
                                    "group", "class_gbif",
                                    "order_gbif", "family_gbif",
                                    "Genus", "Species",
                                    "rank_gbif", "Origin_Country",
                                    "Import_Date", "Sex",
                                    "Number", "Unit_cost", 
                                    "Total_Cost", "Currency", 
                                    "Value_USD", "iucn_listing",
                                    "population_trend", "cites",
                                    "country_captive_bred")]

# Update column names
colnames(import_data_tidy) <- c("UID", "AWB", "Binomial",
                                "Group", "Class",
                                "Order", "Family",
                                "Genus", "Species",
                                "Rank", "Origin_country",
                                "Import_date", "Sex",
                                "Number", "Unit_cost",
                                "Total_cost", "Currency",
                                "Value_USD", "IUCN_listing",
                                "Population_trend", "CITES_listing",
                                "Captive_wild")

# Pseudonymise AWB
AWB <- unique(import_data_tidy$AWB) %>% unique
AWB <- data.frame(AWB = AWB,
                  AWB_ID = 1:length(AWB))
import_data_tidy <- merge.data.table(import_data_tidy, AWB,
                                     by = "AWB")

# Correct entries --------------------------------------------------------------

# Convert farmed_supplier to farmed_likely
import_data_tidy[Captive_wild == "farmed_supplier", Captive_wild := "farmed_likely"]

# Correct Siganus magnificus
import_data_tidy[Binomial == "Magnificus",
                 `:=` (Binomial = "Siganus magnificus",
                       Class = "Teleostei",
                       Order = "Perciformes",
                       Family = "Siganidae",
                       Genus = "Siganus",
                       Species = "magnificus",
                       Rank = "SPECIES",
                       IUCN_listing = "LC",
                       Population_trend = "Unknown")]

# Correct Elasmobranchii
import_data_tidy[Class == "Elasmobranchii", Group := "Other vertebrates"]

# Update IUCN categories
import_data_tidy[IUCN_listing == "LR/cd", IUCN_listing := "NT"]
import_data_tidy[IUCN_listing == "LR/lc", IUCN_listing := "LC"]

# Correct Anthias squamipinnis to Pseudanthias squamipinnis
import_data_tidy[Binomial == "Anthias squammipinnis", Binomial := "Pseudanthias squamipinnis"]

# Correct Urolophus jamaicensis to Urobatis jamaicensis
import_data_tidy[Binomial == "Urolophus jamaicensis", Binomial := "Urobatis jamaicensis"]

# Get genus and species from binomial
# Do in the script so less likely to get typographic errors
# Ignore warning
found_taxa <- separate(import_data_tidy,
                       col = Binomial,
                       into = c("Genus", "Species"),
                       sep = c(" "),
                       remove = F)

# Save -------------------------------------------------------------------------

# Save
data.table::fwrite(import_data_tidy, here::here("data",
                                                "modified-data",
                                                "invoice-data",
                                                "07-import-data-final.csv"))

# CITES only
cites_records <- import_data_tidy[CITES_listing == "II"]
data.table::fwrite(import_data_tidy, here::here("data",
                                                "modified-data",
                                                "invoice-data",
                                                "07a-import-data-cites.csv"))
