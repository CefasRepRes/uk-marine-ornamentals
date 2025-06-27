#### Time series plots ####

# Load required packages -------------------------------------------------------

library(here) # Easy filepaths
library(readr) # Better file reading
library(data.table) # Better data wrangling
library(dplyr) # Use if you have to
library(tidyr) # Column separation
library(stringr) # String handling
library(ggplot2) # Plotting

# Get rid of scientific notation
options(scipen = 9999)

# Run tables code --------------------------------------------------------------

source(here::here("code", "analyses", "taxa-analysis.R"))

# Plot -------------------------------------------------------------------------

# Create function for plots
timePlot <- function(name){
  data <- import_data[Binomial == name,
                      c("Import_date", "Number")]
  data <- data[, .(sum(Number)),
               by = "Import_date"]
  data <- data[, Import_date := as.Date(Import_date, format = "%d/%m/%Y")]
  plot <- ggplot(data,
                 aes(x = Import_date, y = V1)) +
    geom_smooth(method = "loess") +
    geom_point(size = 3) +
    xlab("Import date") +
    ylab("Number of individuals") +
    ggtitle(name) +
    theme_light()
  return(plot)
}

# Plot time series of top 10 species
time_ten <- timePlot(name = spp_tally$Binomial[1]) +
  timePlot(name = spp_tally$Binomial[2]) + 
  timePlot(name = spp_tally$Binomial[3]) + 
  timePlot(name = spp_tally$Binomial[4]) + 
  timePlot(name = spp_tally$Binomial[5]) +
  timePlot(name = spp_tally$Binomial[6]) + 
  timePlot(name = spp_tally$Binomial[7]) + 
  timePlot(name = spp_tally$Binomial[8]) +
  timePlot(name = spp_tally$Binomial[9]) + 
  timePlot(name = spp_tally$Binomial[10]) + plot_layout(ncol = 2)
time_ten

# Save
png(filename = here::here("outputs", "plots", "time_plots.png"),
    width = 8, height = 12, res = 400, units = "in")
print(time_ten)
dev.off()
