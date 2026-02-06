#### Other invertebrates ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(data.table) # Better data wrangling
library(magrittr) # Pipes %>%
library(ggplot2) # Plotting
library(patchwork) # Plot arranging

# Get rid of scientific notation
options(scipen = 9999)

# source numValSppTally function
source(here::here("functions", "numValSppTally.R"))

# Load data --------------------------------------------------------------------

import_data <- data.table::fread(here::here("data",
                                            "modified-data",
                                            "import-data-final.csv"))
import_data <- data.table(import_data)

# subset to inverts
inverts_dat <- import_data[Group == "Other invertebrates"]

# Tallies -----------------------------------------------------------------------

## order =====

# by order
inverts_orders <- numValSppTally(data = inverts_dat, grouping = c("Order",
                                                                  "Class"))

# top ten orders by number
inverts_orders_number <- head(inverts_orders[order(-Number)], n = 10)

# top ten orders by value
inverts_orders_value <- head(inverts_orders[order(-Value_USD)], n = 10)

## species =====

# by species
inverts_species <- numValSppTally(data = inverts_dat, grouping = c("Binomial",
                                                                   "Family",
                                                                   "Order",
                                                                   "Class"))

# top ten species by number
inverts_species_number <- head(inverts_species[order(-Number)], n = 10)

# top ten species by value
inverts_species_value <- head(inverts_species[order(-Value_USD)], n = 10)

# Plots ------------------------------------------------------------------------

# Create palette
inverts_order_palette <- c("#332288",
                           "#0077BB",
                           "#33BBEE",
                           "#88CCEE",
                           "#44AA99",
                           "#117733",
                           "#999933",
                           "#DDCC77",
                           "#EE7733",
                           "#CC3311",
                           "#CC6677",
                           "#882255",
                           "#AA4499")

# assign names to palette
names(inverts_order_palette) <- c(unique(inverts_orders_number$Order),
                                  "Cardiida", "Scleralcyonacea") # add for value plot

# plot by number
inverts_number <- ggplot(inverts_orders_number,
                         aes(x = reorder(Order, Number), y = Number, fill = Order)) +
  geom_col() +
  coord_flip() +
  xlab("Family\n") +
  ylab("\nNumber of individuals") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = inverts_order_palette) +
  theme_bw() +
  labs(tag = "A")

# plot by value
inverts_value <- ggplot(inverts_orders_value,
                        aes(x = reorder(Order, Value_USD), y = Value_USD, fill = Order)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  ylab("\n Total value of imports ($ USD)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = inverts_order_palette) +
  theme_bw() +
  labs(tag = "B")

# combines plots
inverts_plot <- inverts_number + inverts_value & theme(legend.position = "none")
inverts_plot

# save
png(filename = here::here("outputs", "plots", "inverts_orders.png"),
    width = 8, height = 4, res = 400, units = "in")
print(inverts_plot)
dev.off()
