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
                                            "07-import-data-marine-captive-wild.csv"))

str(import_data)

# Tidy data --------------------------------------------------------------------

# Replace GBIF class with WORMS class where there is no GBIF class
import_data[is.na(class_gbif), class_gbif := class_worms]

# Select columns of interest
import_data_tidy <- import_data[, c("UID", "AWB", "canonicalName", 
                                    "class_gbif",
                                    "order_gbif", "family_gbif",
                                    "Genus", "Species",
                                    "rank_gbif", "Origin_Country",
                                    "Import_Date", "Sex",
                                    "Number", "Unit_cost", 
                                    "Total_Cost", "Currency", 
                                    "Value_USD", "IUCN_listing",
                                    "CITES_listing",
                                    "country_captive_bred")]

# Update column names
colnames(import_data_tidy) <- c("UID", "AWB", "Binomial",
                                "Class",
                                "Order", "Family",
                                "Genus", "Species",
                                "Rank", "Origin_country",
                                "Import_date", "Sex",
                                "Number", "Unit_cost",
                                "Total_cost", "Currency",
                                "Value_USD", "IUCN_listing",
                                "CITES_listing",
                                "Captive_wild")

# Pseudonymise AWB
AWB <- unique(import_data_tidy$AWB) %>% unique
AWB <- data.frame(AWB = AWB,
                  AWB_ID = 1:length(AWB))
import_data_tidy <- merge.data.table(import_data_tidy, AWB,
                                     by = "AWB")
import_data_tidy <- import_data_tidy[, !c("AWB")]

# Add group --------------------------------------------------------------------

# view classes
unique(import_data_tidy$Class)

import_data_tidy[, Group := as.character(NA)]

# correct class
import_data_tidy[Class == "Eupercaria incertae sedis", Class := "Teleostei"]

# Bony fishes
import_data_tidy[Class %in% c("Teleostei", "Cladistia"), Group := "Bony fishes"]

# Other vertebrates
import_data_tidy[Class %in% c("Elasmobranchii", "Amphibia"), Group := "Other vertebrates"]

# Other invertebrates
import_data_tidy[Class %in% c("Anthozoa", "Echinoidea",
                              "Malacostraca", "Polychaeta",
                              "Gastropoda", "Ophiuroidea",
                              "Cephalopoda", "Crinoidea", 
                              "Scyphozoa", "Ascidiacea",
                              "Hydrozoa", "Asteroidea",
                              "Merostomata", "Bivalvia",
                              "Holothuroidea", "Demospongiae"), Group := "Other invertebrates"]

# Stony corals
import_data_tidy[Order == "Scleractinia", Group := "Stony corals"]

# Algae
import_data_tidy[Class %in% c("Florideophyceae", "Ulvophyceae"), Group := "Algae"]

# Check classes
unique(import_data_tidy[!is.na(Class)]$Group)
import_data_tidy[!is.na(Class) & is.na(Group)]

str(import_data_tidy)

# Final edits ------------------------------------------------------------------

import_data_tidy[Binomial == "Bodianus anthoides",
                 `:=` (Binomial = "Bodianus anthioides",
                       IUCN_listing = "LC",
                       Species = "anthioides")]
import_data_tidy[Binomial == "Bodianus anthioides" &
                   Origin_country == "Kenya", Captive_wild := "farmed_likely"]

import_data_tidy[Binomial == "Zanclus cornatus",
                 `:=` (Binomial = "Zanclus cornutus",
                       IUCN_listing = "LC",
                       Species = "cornutus")]

# Save -------------------------------------------------------------------------

# Convert "" to NA
import_data_tidy[import_data_tidy == ""] <- NA
import_data_tidy[Sex == "NA"]$Sex <- NA

# Save
data.table::fwrite(import_data_tidy, here::here("data",
                                                "modified-data",
                                                "import-data-final.csv"),
                   na = "NA")

# CITES only
cites_records <- import_data_tidy[CITES_listing == "II"]
data.table::fwrite(import_data_tidy, here::here("data",
                                                "modified-data",
                                                "07a-import-data-cites.csv"),
                   na = "NA")
