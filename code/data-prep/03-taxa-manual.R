#### Acquiring taxa step 2 - manual process ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
# Taxnonomic packages
library(worrms)
library(taxize)

# Load data --------------------------------------------------------------------

# Load missing taxa
missing_taxa <- read.csv(here::here("data",
                                    "modified-data",
                                    "missing-taxa.csv"))
colnames(missing_taxa)[1:2] <- c("input_names", "input_names_common")
missing_taxa$input_names[missing_taxa$input_names == ""] <- NA
missing_taxa <- data.table(missing_taxa)

# Load manually found taxa
found_taxa <- read.csv(here::here("data",
                                  "modified-data",
                                  "missing-taxa-identified.csv"))

str(found_taxa)
colnames(found_taxa)[1] <- "input_names"
found_taxa <- data.table(found_taxa)

# Find those still without identification --------------------------------------

# Get those still missing
missing_taxa_unfound <- missing_taxa[!(input_names %in% found_taxa$input_names) |
                                       !(input_names_common %in% found_taxa$Common_Name)]

# Append to found taxa
found_missing_taxa <- rbind(found_taxa,
                            missing_taxa_unfound,
                            use.names = FALSE)

# Write into file
write.csv(found_missing_taxa, here::here("data",
                                         "modified-data",
                                         "missing-taxa-identified.csv"),
          row.names = F)

#################################################
#### Identify taxa before proceeding further ####
#################################################
warning("Identify taxa before proceeding further")

# Join identified taxa to main data set ----------------------------------------

# Read identified taxa back in
found_taxa <- read.csv(here::here("data",
                                  "modified-data",
                                  "missing-taxa-identified.csv"))

# Get genus and species from binomial
# Do in the script so less likely to get typographic errors
# Ignore warning
found_taxa <- separate(found_taxa,
                       col = Binomial,
                       into = c("Genus", "Species"),
                       sep = c(" "),
                       remove = F)
found_taxa <- unique(found_taxa)

## Join ========================================================================

# Load main data set
import_data <- read_csv(here::here("data",
                                   "modified-data",
                                   "02-import-data-taxa-automated.csv"),
                        # Ensure column types are read in correctly
                        col_types = cols(Import_Date = col_date(format = "%Y-%m-%d"),
                                         Unit_cost = col_double(),
                                         Total_Cost = col_double(),
                                         Value_USD = col_double()),
                        na = c("", " ", NA, "N/A", "#NA", "#REF!", "#N/A")) %>% data.table()
# Check data structure
str(import_data)

# Loop to join
for(i in 1:nrow(import_data)){
  # Join
  import_data[submitted_name == found_taxa$input_names[i],
              `:=` (class = found_taxa$class[i],
                    order = found_taxa$order[i],
                    family = found_taxa$family[i],
                    Binomial = found_taxa$Binomial[i],
                    submitted_name = found_taxa$input_names[i])]
  print(i)
}

# Correct class ----------------------------------------------------------------

import_data_taxa <- data.table(import_data)

# Check classes
unique(import_data_taxa$class)

# Correct "NA" to NA
import_data_taxa[class == "NA", class := NA]

# Actinopterygii
import_data_taxa[, class := replace(class, class %in% c("Actinopteri", "Actinopterygii"), "Teleostei")]

# Cladistia
import_data_taxa[, class := replace(class, class == "Cladistii", "Cladistia")]

# Elasmobranchii
import_data_taxa[, class := replace(class, class == "Chondrichthyes", "Elasmobranchii")]

# Insecta (correct whole taxon)
import_data_taxa[class == "Insecta",
                 `:=` (class = "Hexacorallia",
                       order = "Actiniaria",
                       family = "Actiniidae",
                       Genus = "Actinia",
                       Species = NA,
                       Binomial = "Actinia")]

# Discosoma (correct whole taxon)
import_data_taxa[class == "Bacillariophyceae",
                 `:=` (class = "Hexacorallia",
                       order = "Corallimorpharia",
                       family = "Discosomidae",
                       Genus = "Discosoma",
                       Species = NA,
                       Binomial = "Discosoma")]

# Check classes
unique(import_data_taxa$class)

# Check species list -----------------------------------------------------------

# Correct erroneous taxa
corr_taxa <- read.csv(here::here("data", "modified-data", "corrected-taxa.csv"))

## By species ==================================================================

# Do quick check of taxa
spp_check <- import_data_taxa[, .(Binomial, family, order, class)] %>% unique()

# Loop to join - Binomial
for(i in 1:nrow(import_data_taxa)){
  # Join
  import_data_taxa[Binomial == corr_taxa$input_names[i],
              `:=` (class = corr_taxa$class[i],
                    order = corr_taxa$order[i],
                    family = corr_taxa$family[i],
                    Binomial = corr_taxa$Binomial[i],
                    submitted_name = corr_taxa$input_names[i])]
  print(i)
}

# correct Holocathus --> Holacanthus
import_data_taxa$Binomial <- gsub("Holocanthus", "Holacanthus", import_data_taxa$Binomial)

# correct Valencienna --> Valenciennea
import_data_taxa$Binomial <- gsub("Valencienna", "Valenciennea", import_data_taxa$Binomial)

# correct Ricordia --> Ricordea
import_data_taxa$Binomial <- gsub("Ricordia", "Ricordea", import_data_taxa$Binomial)

## By family ===================================================================

# Do quick check of taxa
fam_check <- import_data_taxa[, .(family, order, class)] %>% unique()

# Correct Apogonidae
import_data_taxa[family == "Apogonidae", order := "Kurtiformes"]

# Correct Pseudochromidae
import_data_taxa[family == "Pseudochromidae", order := "Ovalentaria incertae sedis"]

# Correct Zanclidae
import_data_taxa[family == "Zanclidae", order := "Acanthuriformes"]

# Correct Callionymidae
import_data_taxa[family == "Callionymidae", order := "Callionymiformes"]

# Correct Cerianthidae
import_data_taxa[family == "Cerianthidae", order := "Ceriantharia"]
import_data_taxa[order == "Ceriantharia", class := "Hexacorallia"]

# Correct Cirrhitidae
import_data_taxa[family == "Cirrhitidae", order := "Centrarchiformes"]

# Correct Ephippidae
import_data_taxa[family == "Ephippidae", order := "Acanthuriformes"]

# Correct Holocentridae
import_data_taxa[family == "Holocentridae", order := "Holocentriformes"]

# Correct Pomacanthidae
import_data_taxa[family == "Pomacanthidae", order := "Acanthuriformes"]

# Correct Scleralcyonacea
import_data_taxa[order == "Scleralcyonacea", class := "Octocorallia"]

# Correct Chromodorididae
import_data_taxa[family == "Chromodorididae", order := "Doridida"]

# Correct Scleractinia
import_data_taxa[order == "Scleractinia", class := "Hexacorallia"]

# Correct Lutjanidae
import_data_taxa[family == "Lutjanidae", class := "Eupercaria incertae sedis"]

# Correct Scaridae
import_data_taxa[family == "Scaridae", order := "Eupercaria incertae sedis"]
import_data_taxa[family == "Scaridae", family := "Labridae"]

# Correct genus and species
import_data_taxa <- separate(import_data_taxa,
                             col = Binomial,
                             into = c("Genus", "Species"),
                             sep = " ",
                             remove = FALSE)

# Save -------------------------------------------------------------------------

# Write into file
data.table::fwrite(import_data_taxa, here::here("data",
                                                "modified-data",
                                                "03-import-data-taxa-manual.csv"),
                   row.names = F,
                   na = NA)
