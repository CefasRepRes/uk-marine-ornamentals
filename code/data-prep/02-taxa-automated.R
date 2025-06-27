#### Acquiring taxa step 1 - automated process ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(progress) # Progress bar
library(beepr) # Beep when complete
# Taxnonomic packages
library(worrms)
library(rgbif)
library(taxize)

# Load cleaned import data -----------------------------------------------------

import_data <- read_csv(here::here("data",
                                   "modified-data",
                                   "invoice-data",
                                   "01-import-data-currency.csv"),
                        # Ensure column types are read in correctly
                        col_types = cols(Import_Date = col_date(format = "%Y-%m-%d"),
                                         Unit_cost = col_double(),
                                         Total_Cost = col_double(),
                                         Value_USD = col_double()),
                        na = c("", " ", NA, "N/A", "#NA", "#REF!", "#N/A")) %>%
  data.table() # Convert to data table

# Check structure
str(import_data)

# Exclude freshwater entries ----------------------------------------------------

unique(import_data$Aquatic_ecosystem)

import_data <- import_data[!(Aquatic_ecosystem %in% c("Freshwater",
                                                      "FreshwaterTrop",
                                                      "Brackish",
                                                      "Estuarine"))]

# Check those already found ----------------------------------------------------

# Get file path
taxa_fp <- here::here("data",
                      "modified-data",
                      "invoice-data",
                      "03-import-data-taxa-manual.csv")

# Run if the file is available
if(file.exists(taxa_fp)){
  # Load data
  found_taxa <- read.csv(taxa_fp) %>% data.table()
  
  # Select taxonomy
  found_taxonomy <- found_taxa[, c("Binomial",
                                   "Common_Name",
                                   "Genus",
                                   "Species",
                                   "family",
                                   "order",
                                   "class")]
  # Get those found
  found_taxonomy <- found_taxonomy[!is.na(class)] %>% unique()
  
  # Create progress bar
  pb <- progress_bar$new(total = nrow(found_taxonomy))
  
  # Join to data
  for(i in 1:nrow(found_taxonomy)){
    import_data[Binomial == found_taxonomy$Binomial[i] |
                  Common_Name == found_taxonomy$Common_Name[i],
                `:=` (class = found_taxonomy$class[i],
                      order = found_taxonomy$order[i],
                      family = found_taxonomy$family[i])]
    pb$tick()
  }
  # If not, add columns
} else {
  import_data[, `:=` (class = NA,
                      order = NA,
                      family = NA)]
}

# Get taxonomy - WoRMS ---------------------------------------------------------

# Get a list of unique species
# Use binomial here as it is more likely to return the correct outcome
species <- import_data[is.na(class)][, c("Binomial", "Common_Name")] %>% 
  unique()
binomial <- species$Binomial %>% 
  unique() %>% 
  na.omit()

# Create progress bar
pb <- progress_bar$new(total = length(binomial))

# Loop over species list checking for synonyms using Worms
synonyms_worm <- data.frame()
for(i in 1:length(binomial)){
  # Fetch the synonyms from the WoRMS database
  syn <- try(worrms::wm_records_taxamatch(binomial[i],
                                          marine_only = FALSE))
  syn <- syn[[1]]
  # Only save if it returns results
  if("data.frame" %in% class(syn)){
    syn$submitted_name <- binomial[i]
    synonyms_worm <- rbind(synonyms_worm, syn)
  }
  syn <- NULL # reset
  pb$tick()
};beep() # beep when complete

# Convert to data.table
synonyms <- data.table(synonyms_worm)

synonyms <- synonyms[status == "accepted" &
                       rank != "subspecies"]
length(unique(synonyms$submitted_name))

# Remove terrestrial and extinct returns
synonyms <- synonyms[isTerrestrial != 1 |
                       is.na(isTerrestrial)][isExtinct != 1 |
                                               is.na(isExtinct)]

# Ensure genus results for genus and species results for species
synonyms <- synonyms[stringr::str_count(submitted_name, "\\w+") == 1 &
                       rank == "Genus" |
                       stringr::str_count(submitted_name, "\\w+") == 2 &
                       rank == "Species"]

# Select columns
synonyms <- synonyms[, c("submitted_name",
                         "valid_name",
                         "family",
                         "order",
                         "class",
                         "rank")] %>% unique()

# Remove NA
synonyms <- synonyms[submitted_name != "NA"]

## Remove duplicates ===========================================================

# Get duplicates
syn_dupes <- rbind(dplyr::filter(synonyms, duplicated(submitted_name)),
                   dplyr::filter(synonyms, duplicated(submitted_name, fromLast = T)))

# Remove duplicates
synonyms <- synonyms[
  !(submitted_name == "Acropora" &
      valid_name != "Acropora")
][
  !(submitted_name == "Actinia" &
      valid_name != "Actinia")
][
  !(submitted_name == "Alcyonium" &
      valid_name != "Alcyonium")
][
  !(submitted_name == "Aplysia" &
      valid_name != "Aplysia")
][
  !(submitted_name == "Chaetodon aureus" &
      valid_name != "Cheatodon auripes")
][
  !(submitted_name == "Chromis ovalis" &
      valid_name != "Chromis ovalis")
][
  !(submitted_name == "Chromis" &
      valid_name != "Chromis")
][
  !(submitted_name == "Cryptocentrus fasciatus" &
      valid_name != "Cryptocentrus fasciatus")
][
  !(submitted_name == "Diadema" &
      valid_name != "Diadema")
][
  !(submitted_name == "Diodon" &
      valid_name != "Diodon")
][
  !(submitted_name == "Echinaster" &
      valid_name != "Echinaster")
][
  !(submitted_name == "Heteractis" &
      family != "Heteractidae")
][
  !(submitted_name == "Holothuria" &
      valid_name != "Holothuria")
][
  !(submitted_name == "Histrio histrio" &
      valid_name != "Histrio histrio")
][
  !(submitted_name == "Histrio histrio" &
      valid_name != "Histrio histrio")
][
  !(submitted_name == "Leucosia" &
      valid_name != "Leucosia")
][
  !(submitted_name == "Limaria orientalis" &
      valid_name != "Limaria orientalis")
][
  !(submitted_name == "Limaria" &
      valid_name != "Limaria")
][
  !(submitted_name == "Linckia" &
      is.na(valid_name))
][
  !(submitted_name == "Lybia" &
      valid_name != "Lybia")
][
  !(submitted_name == "Paracucumaria" &
      valid_name != "Paracucumaria")
][
  !(submitted_name == "Plectorhinchus pictus" &
      valid_name != "Plectorhinchus pictus")
][
  !(submitted_name == "Porcellana" &
      valid_name != "Porcellana")
][
  !(submitted_name == "Stenopus" &
      valid_name != "Stenopus")
][
  !(submitted_name == "Strombus" &
      is.na(family))
][
  !(submitted_name == "Actina" &
      valid_name != "Actinia")
][
  !(submitted_name == "Actinaria" &
      valid_name != "Actineria")
][
  !(submitted_name == "Acyonium" &
      valid_name != "Alcyonium")
][
  !(submitted_name == "Anthellia" &
      valid_name != "Anthelia")
][
  !(submitted_name == "Antilogorgia" &
      valid_name != "Antillogorgia")
][
  !(submitted_name == "Aphysia" &
      valid_name != "Aplysia")
][
  !(submitted_name == "Bispria" &
      valid_name != "Bispira")
][
  !(submitted_name == "Capnelia" &
      valid_name != "Capnella")
][
  !(submitted_name == "Cardina" &
      valid_name != "Caridina")
][
  !(submitted_name %in% c("Cladellia", "Cladelia") &
      valid_name != "Cladiella")
][
  !(submitted_name == "Cladopora" &
      valid_name != "Cladophora")
][
  !(submitted_name == "Condylactus" &
      valid_name != "Condylactis")
][
  !(submitted_name == "Cyprae" &
      valid_name != "Cypraea")
][
  !(submitted_name == "Dandanus" &
      valid_name != "Dardanus")
][
  !(submitted_name == "Formia" &
      valid_name != "Fromia")
][
  !(submitted_name == "Iybia" &
      valid_name != "Lybia")
][
  !(submitted_name == "Melithsea" &
      valid_name != "Melithaea")
][
  !(submitted_name == "Nerite" &
      valid_name != "Nerita")
][
  !(submitted_name == "Paraperois" &
      valid_name != "Parapercis")
][
  !(submitted_name == "Periclimentes" &
      valid_name != "Periclimenes")
][
  !(submitted_name == "Phillidia" &
      valid_name != "Phyllidia")
][
  !(submitted_name == "Squilia" &
      valid_name != "Squilla")
][
  !(submitted_name == "Srombus" &
      class != "Gastropoda")
][
  !(submitted_name == "Taenira" &
      valid_name != "Taeniura")
][
  !(submitted_name == "Tronchus" &
      valid_name != "Trochus")
][
  !(submitted_name == "Trochus maculatus" &
      valid_name != "Trochus maculatus")
][ # Likely this as algae not recorded as being used in aquaria
  !(submitted_name == "Turbinaria" &
      order != "Scleractinia")
][ # Remove erroneous records
  !(submitted_name %in% c("Annulus", "Nareus", "Nasarius", "Nepthea", "Protorea",
                          "Sole"))
]

# Check again for duplicates
syn_dupes <- rbind(dplyr::filter(synonyms, duplicated(submitted_name)),
                   dplyr::filter(synonyms, duplicated(submitted_name, fromLast = T)))
if(nrow(syn_dupes > 0)){stop("You still have duplicates in your data set. Please edit your dupes lists.")}

# Join to species list
binomial <- data.table(binomial)
species_worms <- data.table::merge.data.table(binomial,
                                              synonyms,
                                              all.x = TRUE,
                                              by.x = "binomial",
                                              by.y = "submitted_name") %>%
  data.table()

# Check if join has been completed successfully
if(nrow(binomial) == nrow(species_worms)){
  message("Join successful")
} else {
  stop("Your join has not been successful")
}

# Remove subgenera
species_worms$valid_name <- gsub("\\s*\\([^\\)]+\\)", "" , as.character(species_worms$valid_name))

## Join back to import data ====================================================

import_data_taxa <- import_data

# Convert column classes
import_data_taxa$class <- as.character(import_data_taxa$class)
import_data_taxa$order <- as.character(import_data_taxa$order)
import_data_taxa$family <- as.character(import_data_taxa$family)

# Create progress bar
pb <- progress_bar$new(total = nrow(species_worms))
# Loop to join
for(i in 1:nrow(species_worms)){
  # Join
  import_data_taxa[Binomial == species_worms$binomial[i],
                   `:=` (class = species_worms$class[i],
                         order = species_worms$order[i],
                         family = species_worms$family[i],
                         Binomial = species_worms$valid_name[i],
                         submitted_name = species_worms$binomial[i],
                         rank = species_worms$rank[i])]
  pb$tick()
}

# Update genus and species
# Ignore missing pieces warning
import_data_taxa <- separate(import_data_taxa,
                             col = Binomial,
                             into = c("Genus", "Species"),
                             sep = " ",
                             remove = F)
import_data_taxa <- data.table(import_data_taxa)

# GBIF -------------------------------------------------------------------------

# Find those with missing taxa
# Use class as not everything is ID-ed to species, genus or family level
species_missing_taxa <- import_data_taxa[is.na(import_data_taxa$class)][
  , "submitted_name"]
species_missing_taxa <- unique(species_missing_taxa)

# Remove NA
species_missing_taxa <- species_missing_taxa[submitted_name != "NA"]

# Create progress bar
pb <- progress_bar$new(total = nrow(species_missing_taxa))

# Get missing taxa from GBIF
taxonomy_gbif <- data.frame()
for(i in 1:nrow(species_missing_taxa)){
  t <- try(rgbif::name_backbone(name = species_missing_taxa$submitted_name[i],
                                strict = FALSE))
  taxonomy_gbif <- dplyr::bind_rows(taxonomy_gbif, t)
  pb$tick()
}

# Select only taxonomy
taxonomy_gbif <- data.table(taxonomy_gbif)
taxonomy_gbif <- taxonomy_gbif[, c("verbatim_name",
                                   "phylum",
                                   "class",
                                   "order",
                                   "family",
                                   "genus",
                                   "canonicalName")] %>% unique()

# Remove erroneous classes
taxonomy_gbif_corr <- taxonomy_gbif[!(class %in% c("Aves", "Insecta",
                                                   "Agaricomycetes", "Magnoliopsida"))]

# Add in Actinopterygii as class
taxonomy_gbif_corr <- taxonomy_gbif_corr[is.na(class) &
                                           !(class %in% c("Elasmobranchii",
                                                          "Amphibia")) &
                                           phylum == "Chordata", class := "Actinopterygii"]

# Remove those which returned no results
taxonomy_gbif_corr <- taxonomy_gbif_corr[!(is.na(phylum))]

## Join back to import data ====================================================

# Create progress bar
pb <- progress_bar$new(total = nrow(import_data_taxa))

# Loop to join
for(i in 1:nrow(import_data_taxa)){
  # Join
  import_data_taxa[submitted_name == taxonomy_gbif_corr$verbatim_name[i],
                   `:=` (class = taxonomy_gbif_corr$class[i],
                         order = taxonomy_gbif_corr$order[i],
                         family = taxonomy_gbif_corr$family[i],
                         Binomial = taxonomy_gbif_corr$canonicalName[i],
                         submitted_name = taxonomy_gbif_corr$verbatim_name[i])]
  pb$tick()
}

# Correct genus and species
import_data_taxa <- separate(import_data_taxa,
                             col = Binomial,
                             into = c("Genus", "Species"),
                             sep = " ",
                             remove = F)

# Convert back to data.table
import_data_taxa <- data.table(import_data_taxa)

# NCBI -------------------------------------------------------------------------

# Find those with missing taxa
# Use class as not everything is ID-ed to species, genus or family level
species_missing_taxa <- import_data_taxa[is.na(import_data_taxa$class)][
  , c("submitted_name", "Common_Name", "Genus", "Species")]
species_missing_taxa <- unique(species_missing_taxa)

# Create progress bar
pb <- progress_bar$new(total = nrow(species_missing_taxa))

# Get missing taxa - NCBI - using taxize package
# Don't use ITIS - it's super slow and inaccurate
# You will have to babysit this
# Keep the original database open and use common names for guidance
taxonomy_ncbi <- data.frame()
for(i in 1:nrow(species_missing_taxa)){
  t <- try(taxize::tax_name(sci = species_missing_taxa$submitted_name[i],
                            get = c("species", "family", "order", "class"),
                            db = "ncbi"))
  taxonomy_ncbi <- rbind(taxonomy_ncbi, t)
  pb$tick()
}

# Tidy the NCBI outputs
taxonomy_ncbi <- data.table(taxonomy_ncbi)
taxonomy_ncbi_tidy <- taxonomy_ncbi[, !"db"]
taxonomy_ncbi_tidy <- taxonomy_ncbi_tidy[!is.na(class) &
                                           !(class %like% "Error")]
taxonomy_ncbi_tidy <- unique(taxonomy_ncbi_tidy)

# Remove incorrect classes
taxonomy_ncbi_tidy <- taxonomy_ncbi_tidy[class != "Agaricomycetes"]

## Join back to import data ====================================================

# Create progress bar
pb <- progress_bar$new(total = nrow(import_data_taxa))

# Loop to join
for(i in 1:nrow(import_data_taxa)){
  # Join
  import_data_taxa[submitted_name == taxonomy_ncbi_tidy$query[i],
                   `:=` (class = taxonomy_ncbi_tidy$class[i],
                         order = taxonomy_ncbi_tidy$order[i],
                         family = taxonomy_ncbi_tidy$family[i],
                         Binomial = taxonomy_ncbi_tidy$species[i],
                         submitted_name = taxonomy_ncbi_tidy$query[i])]
  pb$tick()
}

# Correct genus and species
import_data_taxa <- separate(import_data_taxa,
                             col = Binomial,
                             into = c("Genus", "Species"),
                             sep = " ",
                             remove = F)

# Convert back to data.table
import_data_taxa <- data.table(import_data_taxa)

# Check classes ----------------------------------------------------------------

unique(import_data_taxa$class)

# Change Teleostei etc. to Actinopterygii
import_data_taxa[class %in% c("Teleostei", "Actinopteri"), class := "Actinopterygii"]

# Remove incorrect taxa
import_data_taxa[class %in% c("Agaricomycetes", "Bacillariophyceae"),
                 `:=` (class = NA,
                       order = NA, 
                       family = NA,
                       genus = NA,
                       species = NA,
                       Binomial = NA)]

# Check for duplicates ---------------------------------------------------------

species <- import_data_taxa[, c("Binomial", "Genus", "family", "order", "class")] %>%
  unique() %>% na.omit()

# Get duplicates
species_dupes <- rbind(dplyr::filter(species, duplicated(Binomial)),
                       dplyr::filter(species, duplicated(Binomial, fromLast = T)))

# Correct order for Pomacentridae
import_data_taxa <- import_data_taxa[family == "Pomacentridae", order := "Ovalentaria incertae sedis"]

# Correct order for Labridae
import_data_taxa <- import_data_taxa[family == "Labridae", order := "Eupercaria incertae sedis"]

# Correct order for Blenniidae
import_data_taxa <- import_data_taxa[family == "Blenniidae", order := "Blenniiformes"]

# Correct family for Botia
import_data_taxa <- import_data_taxa[Genus == "Botia", family := "Cobitidae"]

# Correct order for Pomacanthidae
import_data_taxa <- import_data_taxa[family == "Pomacanthidae", order := "Acanthuriformes"]

# Correct order for Chaetodontidae
import_data_taxa <- import_data_taxa[family == "Chaetodontidae", order := "Acanthuriformes"]

# Correct order for Acanthuridae
import_data_taxa <- import_data_taxa[family == "Acanthuridae", order := "Acanthuriformes"]

# Correct order for Carangidae
import_data_taxa <- import_data_taxa[family == "Carangidae", order := "Carangiformes"]

# Correct order for Cichlidae
import_data_taxa <- import_data_taxa[family == "Cichlidae", order := "Cichliformes"]

# Correct class for Corallimorpharia
import_data_taxa <- import_data_taxa[order == "Corallimorpharia", class := "Hexacorallia"]

# Correct order for Gobiidae
import_data_taxa <- import_data_taxa[family == "Gobiidae", order := "Gobiiformes"]

# Correct order for Haemulidae
import_data_taxa <- import_data_taxa[family == "Haemulidae", order := "Eupercaria incertae sedis"]

# Correct order for Helostomatidae
import_data_taxa <- import_data_taxa[family == "Helostomatidae", order := "Anabantiformes"]

# Correct family for Lysmata
import_data_taxa <- import_data_taxa[Genus == "Lysmata", family := "Lysmatidae"]

# Correct order for Microdesmidae
import_data_taxa <- import_data_taxa[family == "Microdesmidae", order := "Gobiiformes"]

# Correct order for Parupeneus
import_data_taxa <- import_data_taxa[family == "Mullidae", order := "Mulliformes"]

# Correct order for Nemitperidae
import_data_taxa <- import_data_taxa[family == "Nemipteridae", order := "Eupercaria incertae sedis"]

# Correct class for Malacalcyonacea
import_data_taxa <- import_data_taxa[order == "Malacalcyonacea", class := "Octocorallia"]

# Correct order for Opistognathidae
import_data_taxa <- import_data_taxa[family == "Opistognathidae", order := "Ovalentaria incertae sedis"]

# Correct order for Scorpaenidae
import_data_taxa <- import_data_taxa[family == "Scorpaenidae", order := "Perciformes"]

# Correct order for Siganidae
import_data_taxa <- import_data_taxa[family == "Siganidae", order := "Acanthuriformes"]

# Correct class for Actiniaria
import_data_taxa <- import_data_taxa[order == "Actiniaria", class := "Hexacorallia"]

# Correct class for Zoantharia
import_data_taxa <- import_data_taxa[order == "Zoantharia", class := "Hexacorallia"]

# Save outputs -----------------------------------------------------------------

# Missing taxa
taxa_missing <- import_data_taxa[is.na(class)][, c("submitted_name", 
                                                   "Common_Name",
                                                   "Taxa",
                                                   "Binomial",
                                                   "class",
                                                   "order",
                                                   "family")] %>% unique()

data.table::fwrite(taxa_missing, here::here("data",
                                            "modified-data",
                                            "invoice-data",
                                            "missing-taxa.csv"), row.names = F)

# Save complete dataset
data.table::fwrite(import_data_taxa, here::here("data",
                                                "modified-data",
                                                "invoice-data",
                                                "02-import-data-taxa-automated.csv"), 
                   row.names = F,
                   na = NA)
