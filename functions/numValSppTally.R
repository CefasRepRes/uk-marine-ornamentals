#' numValSppTally
#' 
#' Creates a tally of number, value and number of species by a given grouping
#' for the UK marine ornamental imports data. Includes cumulative summary and
#' percentages.
#'
#' @param data (class data.frame) data frame to be summarised.
#' @param grouping (class string) field(s) in the data frame by which to group the data.
#'
#' @export
#'
numValSppTally <- function(data, grouping){
  # convert to data table
  data <- data.table(data) 
  # tally by species
  tally <- data[, by = c(grouping),
                .(Number = sum(Number, na.rm = T),
                  Value_USD = sum(Value_USD, na.rm = T),
                  Consignments = uniqueN(AWB_ID))]
  # tally by grouping
  cols <- c("Binomial", grouping)
  tally_spp <- data[, ..cols] %>% unique()
  tally_spp <- tally_spp[, b = c(grouping),
                         .N] 
  total_con <- uniqueN(data$AWB_ID)
  # join
  tally_merge <- merge.data.table(tally, tally_spp, by = grouping)
  # get percentages
  tally_merge$per_num <- round(tally_merge$Number / sum(tally_merge$Number) * 100, digits = 1)
  tally_merge$per_val <- round(tally_merge$Value_USD / sum(tally_merge$Value_USD) * 100, digits = 1)
  tally_merge$per_con <- round((tally_merge$Consignments / total_con) * 100, digits = 1)
  tally_merge$per_spp <- round(tally_merge$N / sum(tally_merge$N) * 100, digits = 1)
  colnames(tally_merge)[colnames(tally_merge) == "N"] <- "Species"
  tally_merge$Value_USD <- round(tally_merge$Value_USD)
  # order by number
  tally_merge <- tally_merge[order(-Number)]
  # get cumulative summary
  tally_merge$cumsum_num <- cumsum(tally_merge$per_num)
  return(tally_merge)
}