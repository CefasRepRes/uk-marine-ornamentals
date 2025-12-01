#### Environment data separation ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(beepr) # Beep when complete
# Environment package
library(worrms)

# Get rid of scientific notation
options(scipen = 9999)

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "03-import-data-taxa-manual.csv"))

import_data <- import_data[!is.na(class) &
                             class != "NA"]

get_listings <- import_data[, c("Binomial", "Genus", "family", "order", "class")] %>%
  unique() %>% na.omit()

# Correct the environment ------------------------------------------------------

environment <- data.frame()
for(i in 1:nrow(get_listings)){
  # Start with binomial
  if(!is.na(get_listings$Binomial[i])){
    taxa <- get_listings$Binomial[i]
    # Then check Genus
  } else if(is.na(get_listings$Binomial[i]) && !is.na(get_listings$Genus[i])){
    taxa <- get_listings$Genus[i]
    # Then family
  } else if(is.na(get_listings$Genus[i]) && !is.na(get_listings$family[i])){
    taxa <- get_listings$family[i]
    # Then order
  } else if(is.na(get_listings$family[i]) && !is.na(get_listings$order[i])){
    taxa <- get_listings$order[i]
    # Then class
  } else {
    taxa <- get_listings$class[i]
  }
  env <- try(worrms::wm_records_name(taxa, marine_only = FALSE))
  if("data.frame" %in% class(env)){
    env <- env[1, ] # Get first entry only
    environment <- rbind(environment, env)
  }
  print(i)
};beep()

# Filter only relevant columns
str(environment)
environment_results <- environment[, c("scientificname",
                                       "class",
                                       "order",
                                       "family",
                                       "genus",
                                       "isMarine",
                                       "isBrackish",
                                       "isFreshwater")] %>% data.table()

# Correct NAs in environment column
environment_results$isBrackish[is.na(environment_results$isBrackish)] <- 0
environment_results$isMarine[is.na(environment_results$isMarine)] <- 0
environment_results$isFreshwater[is.na(environment_results$isFreshwater)] <- 0

# Update colnames
colnames(environment_results)[1] <- "Binomial"

# Find and remove duplicates ---------------------------------------------------

environment_results <- unique(environment_results)
environ_dupes <- rbind(dplyr::filter(environment_results, duplicated(Binomial)),
                       dplyr::filter(environment_results, duplicated(Binomial, 
                                                                     fromLast = TRUE)))
if(nrow(environ_dupes > 0)){stop("You still have duplicates in your data set. Please edit your dupes lists.")}

# Add environment categorical column -------------------------------------------

environment_results$Environment <- NA

for(i in 1:nrow(environment_results)){
  print(i)
  if(environment_results$isMarine[i] == 1 &
     environment_results$isFreshwater[i] == 0 &
     !is.na(environment_results$isMarine[i])){
    environment_results$Environment[i] <- "Marine"
  } else if(environment_results$isMarine[i] == 0 &
            environment_results$isFreshwater[i] == 1 &
            !is.na(environment_results$isMarine[i])){
    environment_results$Environment[i] <- "Freshwater"
  } else if(environment_results$isMarine[i] ==  0 &
            environment_results$isFreshwater[i] == 0 &
            environment_results$isBrackish[i] == 1 &
            !is.na(environment_results$isMarine[i])){
    environment_results$Environment[i] <- "Brackish"
  } else if(environment_results$isMarine[i] ==  1 &
            environment_results$isFreshwater[i] == 1 &
            environment_results$isBrackish[i] == 1 &
            !is.na(environment_results$isMarine[i])){
    environment_results$Environment[i] <- "Freshwater"
  }
}

# Join to data
import_data_env <- data.table::merge.data.table(import_data,
                                                environment_results[, -c("class", "order", "family", "genus")],
                                                by = c("Binomial"),
                                                all.x = TRUE)
import_data_env <- data.table(import_data_env)

# Loop to join - genus
for(i in 1:nrow(environment_results)){
  # Join
  import_data_env[Genus == environment_results$genus[i] &
                    is.na(Environment),
                  Environment := environment_results$Environment[i]]
  print(i)
}

# Loop to join - family
for(i in 1:nrow(environment_results)){
  # Join
  import_data_env[family == environment_results$family[i] &
                    is.na(Environment),
                  Environment := environment_results$Environment[i]]
  print(i)
}

# Loop to join - order
for(i in 1:nrow(environment_results)){
  # Join
  import_data_env[order == environment_results$order[i] &
                    is.na(Environment),
                  Environment := environment_results$Environment[i]]
  print(i)
}


# Save -------------------------------------------------------------------------

# All
data.table::fwrite(import_data_env, here::here("data",
                                               "modified-data",
                                               "04-import-data-env.csv"),
                   row.names = FALSE,
                   na = NA)

# Marine
import_data_marine <- import_data_env[Environment == "Marine"]

data.table::fwrite(import_data_marine, here::here("data",
                                                  "modified-data",
                                                  "04a-import-data-marine.csv"),
                   row.names = FALSE,
                   na = NA)
