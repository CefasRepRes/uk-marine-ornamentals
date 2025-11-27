#### Currency conversion ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to

# Load data --------------------------------------------------------------------

import_data <- readr::read_csv(here::here("data",
                                          "original-data",
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

# Conversions from XE on 10/09/2019 (modal import date)

# Add column
import_data[, Value_USD := as.numeric(NA)]

# USD
import_data[Currency == "USD", Value_USD := Total_Cost]

# SGD
import_data[Currency == "SGD", Value_USD := Total_Cost * 0.7250184458]

# GBP
import_data[Currency == "GBP", Value_USD := Total_Cost * 1.2354212247]

# IDR
import_data[Currency == "IDR", Value_USD := Total_Cost * 0.0000711798]

# JPY
import_data[Currency == "JPY", Value_USD := Total_Cost * 0.0093172594]

# MYR
import_data[Currency == "MYR", Value_USD := Total_Cost * 0.2400292384]

# EUR
import_data[Currency == "EUR", Value_USD := Total_Cost * 1.1044777642]

# AUD
import_data[Currency == "AUD", Value_USD := Total_Cost * 0.6865519982]

# KES
import_data[Currency == "KES", Value_USD := Total_Cost * 0.0096490690]

# CAN
import_data[Currency == "CAN", Value_USD := Total_Cost * 0.7609645552]

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
                   row.names = FALSE,
                   na = NA)
