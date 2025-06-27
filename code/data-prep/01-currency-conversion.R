#### Currency conversion ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(progress) # Progress bar

# Load data --------------------------------------------------------------------

import_data <- readr::read_csv(here::here("data",
                                          "original-data",
                                          "invoice-data",
                                          "import-data.csv"),
                               # Ensure correct column types
                               col_types = cols(Import_Date = col_date(format = "%Y-%m-%d"),
                                                Unit_cost = col_double(),
                                                Total_Cost = col_double()),
                               na = c("", " ", NA, "N/A", "#NA", "#REF!", "#N/A")) %>%
  data.table() # Convert to data.table

# Check structure
# Check import date in particular
str(import_data)

# Convert to common currency ---------------------------------------------------

# Find out what currencies there are
unique(import_data$Currency)

# Correct Singapore dollar
import_data[Currency %in% c("SIN USD",
                            "SINUSD",
                            "SGD$",
                            "SIN $",
                            "SIN$"), Currency := "SGD"]

# Correct US dollar
import_data[Currency %in% c("USD",
                            "USD$",
                            "US$"), Currency := "USD"]

# Correct Japanese yen
import_data[Currency == "JYN", Currency := "JPY"]

# List $ and NA as NA
import_data[Currency %in% c("$", "NA"), Currency := NA]

# Check currencies
unique(import_data$Currency)

## Convert to USD ==============================================================

# Create new column
import_data$Value_USD <- NA

# Create progress bar
pb <- progress_bar$new(total = nrow(import_data))

# Loop over the data set, checking the currency and applying the appropriate conversion
# Conversions from XE on 10/09/2019 (modal import date)
for(i in 1:nrow(import_data)){
  import_data$Value_USD[i] <- with(import_data[i, ],
                                   if(is.na(Currency)){
                                     NA
                                     # USD stays the same
                                   } else if(Currency == "USD"){
                                     Total_Cost
                                     # Singapore dollar (SGD)
                                   } else if(Currency == "SGD"){
                                     Total_Cost * 0.7250184458
                                     # Pound sterling (GBP)
                                   } else if(Currency == "GBP"){
                                     Total_Cost * 1.2354212247
                                     # Indonesian rupiah (IDR)
                                   } else if(Currency == "IDR"){
                                     Total_Cost * 0.0000711798
                                     # Japanese yen (JPY)
                                   } else if(Currency == "JPY"){
                                     Total_Cost * 0.0093172594
                                     # Malaysian ringgit
                                   } else if(Currency == "MYR"){
                                     Total_Cost * 0.2400292384
                                     # Euro (EUR)
                                   } else if(Currency == "EUR"){
                                     Total_Cost * 1.1044777642
                                     # Australian dollar (AUD)
                                   } else if(Currency == "AUD"){
                                     Total_Cost * 0.6865519982
                                     # Kenyan shilling (KES)
                                   } else if(Currency == "KES"){
                                     Total_Cost * 0.0096490690
                                     # NA if no currency
                                   } else if(Currency == "CAN"){
                                     # Canadian dollar (CAN)
                                     Total_Cost * 0.7609645552
                                   } else {
                                     NA
                                   })
  pb$tick()
}

# Check currencies again
unique(import_data$Currency)

# Save -------------------------------------------------------------------------

# Check structure
str(import_data)

# Write
data.table::fwrite(import_data, here::here("data",
                                           "modified-data",
                                           "invoice-data",
                                           "01-import-data-currency.csv"),
                   row.names = F,
                   na = NA)
