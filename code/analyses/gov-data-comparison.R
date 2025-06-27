#### Comparison to government data ####

library(here)
library(tidyverse)

# Load data --------------------------------------------------------------------

invoice_data <- data.table::fread(here::here("data",
                                             "modified-data",
                                             "invoice-data",
                                             "07-import-data-final.csv"))
invoice_data <- data.table(invoice_data)

# Check structure
str(invoice_data)

# Change to date
invoice_data[, Import_date := as.Date(Import_date, format = "%d/%m/%Y")]

# Get totals by month ----------------------------------------------------------

# Reformat dates
invoice_data$Import_Date <- format(invoice_data$Import_date, "%Y-%m")

# Select only fishes
fish_invoice_data <- invoice_data %>% dplyr::filter(Group == "Bony fishes")

# Calculate by date
fish_invoice_summary <- fish_invoice_data %>% group_by(Import_Date) %>%
  summarise(Value_USD = sum(Value_USD, na.rm = T))

# Convert to GBP
fish_invoice_summary$Value_GBP <- fish_invoice_summary$Value_USD * 1.23542122

# Load UK government import data -----------------------------------------------

# Load
gov_imports <- read.csv(here::here("data",
                                   "original-data",
                                   "gov-ornamental-fish-import-data.csv"))

# Select marine only
gov_imports <- gov_imports %>% dplyr::filter(CN8 == "ornamental_marine")

# Reformat dates
gov_imports$Date <- format(as.Date(gov_imports$Date), "%Y-%m")

# Calculate by date
gov_imports_summary <- gov_imports %>% group_by(Date) %>%
  summarise(Value_gov = sum(Value_USD, na.rm = T))

# Match to invoice data
invoice_gov_data <- fish_invoice_summary %>%
  dplyr::left_join(gov_imports_summary, by = c("Import_Date" = "Date"))

# Calculate percentage
invoice_gov_data$percent_in_invoices <- with(invoice_gov_data,
                                             Value_GBP / Value_gov * 100)

# Estimate gov data per week
invoice_gov_data$Value_gov_pw <- invoice_gov_data$Value_gov/4.3

# Calculate percentage - per week
invoice_gov_data$percent_in_invoices_pw <- with(invoice_gov_data,
                                                Value_GBP / Value_gov_pw * 100)

# Plot -------------------------------------------------------------------------

# Format for plotting
gov_data_plot <- na.omit(invoice_gov_data) %>% data.table()
gov_data_plot <- gov_data_plot[!(Import_Date %in% c("2018-02",
                                                    "2018-01"))]
gov_data_plot <- pivot_longer(gov_data_plot,
                              cols = c(Value_gov_pw, Value_GBP))
gov_data_plot$name <- factor(gov_data_plot$name, 
                             levels = c("Value_gov_pw", "Value_GBP"))

# Plot
gov_data_plot <- ggplot(data = gov_data_plot, aes(x = Import_Date, y = value, group = name,
                                                  fill = name, col = name)) +
  geom_col(position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("#225555",
                               "#CCEEFF"),
                    labels = c("Total value",
                               "Value in dataset")) +
  scale_colour_manual(values = c("#225555",
                                 "#CCEEFF"),
                      labels = c("Total value",
                                 "Value in dataset")) +
  scale_alpha_manual() +
  xlab("Month") +
  ylab("Value of imports (GBP)\n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom")
gov_data_plot

# save
png(filename = here::here("outputs", "plots", "gov_data.png"),
    width = 8, height = 6, res = 400, units = "in")
print(gov_data_plot)
dev.off()
