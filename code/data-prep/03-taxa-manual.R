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
                                    "invoice-data",
                                    "missing-taxa.csv"))
colnames(missing_taxa)[1:2] <- c("input_names", "input_names_common")
missing_taxa$input_names[missing_taxa$input_names == ""] <- NA
missing_taxa <- data.table(missing_taxa)

# Load manually found taxa
found_taxa <- read.csv(here::here("data",
                                  "modified-data",
                                  "invoice-data",
                                  "missing-taxa-identified.csv"))

str(found_taxa)
colnames(found_taxa)[1] <- "input_names"
found_taxa <- data.table(found_taxa)

# Join together ----------------------------------------------------------------

missing_taxa_binom <- data.table::merge.data.table(missing_taxa,
                                                   found_taxa[!is.na(input_names)],
                                                   by = "input_names",
                                                   all.x = TRUE)

# Find those still without identification --------------------------------------

found_taxa_all <- data.table(missing_taxa_binom) %>% unique()
found_taxa_correct <- found_taxa_all[!is.na(class)]

# Get those still missing
missing_taxa <- data.table(missing_taxa)
missing_taxa_unfound <- missing_taxa[!(input_names %in% found_taxa_correct$input_binomial) |
                                       !(input_names_common %in% found_taxa_correct$common_name)]
missing_taxa_unfound$binomial <- NA
missing_taxa_unfound$family <- NA
missing_taxa_unfound$order <- NA
missing_taxa_unfound$class <- NA

# Append to found taxa
found_missing_taxa <- rbind(found_taxa,
                            missing_taxa_unfound,
                            use.names = FALSE)

# Write into file
write.csv(found_missing_taxa, here::here("data",
                                         "modified-data",
                                         "invoice-data",
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
                                  "invoice-data",
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
                                   "invoice-data",
                                   "02-import-data-taxa-automated.csv"),
                        # Ensure column types are read in correctly
                        col_types = cols(Import_Date = col_date(format = "%Y-%m-%d"),
                                         Unit_cost = col_double(),
                                         Total_Cost = col_double(),
                                         Value_USD = col_double()),
                        na = c("", " ", NA, "N/A", "#NA", "#REF!", "#N/A")) %>% data.table()
# Check data structure
glimpse(import_data)

# Create progress bar
pb <- progress_bar$new(total = nrow(import_data))

# Loop to join
for(i in 1:nrow(import_data)){
  # Join
  import_data[submitted_name == found_taxa$submitted_name[i],
              `:=` (class = found_taxa$class[i],
                    order = found_taxa$order[i],
                    family = found_taxa$family[i],
                    Binomial = found_taxa$Binomial[i],
                    submitted_name = found_taxa$submitted_name[i])]
  pb$tick()
}

# Correct class ----------------------------------------------------------------

import_data_taxa <- data.table(import_data)

# Check classes
unique(import_data_taxa$class)

# Actinopterygii
import_data_taxa[, class := replace(class, class %in% c("Actinopteri", "Actinopterygii"), "Teleostei")]

# Cladistia
import_data_taxa[, class := replace(class, class == "Cladistii", "Cladistia")]

# Chondrichthyes
import_data_taxa[, class := replace(class, class == "Elasmobranchii", "Chondrichthyes")]

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

# Add group --------------------------------------------------------------------

import_data_taxa$group <- NA
for(i in 1:nrow(import_data_taxa)){
  print(i)
  if(import_data_taxa$class[i] %chin% c("Teleostei", "Cladistia")){
    import_data_taxa$group[i] <- "Bony fishes"
  } else if(import_data_taxa$class[i] %chin% c("Elasmobranchii", "Amphibia")){
    import_data_taxa$group[i] <- "Other vertebrates"
  } else if(import_data_taxa$order[i] %chin% "Scleractinia" &&
            !is.na(import_data_taxa$family[i])){
    import_data_taxa$group[i] <- "Stony corals"
  } else if(!is.na(import_data_taxa$class[i])){
    import_data_taxa$group[i] <- "Other invertebrates"
  } else {
    import_data_taxa$group[i] <- NA
  }
};beep()

# Save -------------------------------------------------------------------------

# Write into file
data.table::fwrite(import_data_taxa, here::here("data",
                                                "modified-data",
                                                "invoice-data",
                                                "03-import-data-taxa-manual.csv"),
                   row.names = F,
                   na = NA)

# Count those still missing
missing <- import_data_taxa[is.na(class)][, .(Binomial, Common_Name)] %>% unique()
