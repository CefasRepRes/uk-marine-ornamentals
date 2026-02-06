#' connectionMap
#' 
#' Creates a map of connections from the origin country to the United Kingdom.
#' Points are sized by number of individuals and value.
#'
#' @param group (class string) taxonomic group to be plotted.
#' @param colour (class string) hex code for colour of points.
#' @param tags (class string) tag for the map (e.g. A, B, C).
#'
#' @export
#'
connectionMap <- function(group, colour, tags){
  # get world map
  world <- ggplot2::map_data("world")
  # extract data 
  map_data <- country_group[Group == group, c("Origin_country", "Number", "Value_USD")]
  # extract points
  map_points <- merge.data.table(map_data, countries, by.x = "Origin_country",
                                 by.y = "countries")
  # extract path
  map_path <- merge.data.table(map_data, path, by = "Origin_country")
  # create map by number
  map_number <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "#DDDDDD", col = "white") +
    geom_path(data = map_path, aes(x = lon, y = lat, group = country, linewidth = Number),
              alpha = 0.5, col = colour) +
    geom_point(data = map_points, aes(x = longitude, y = latitude,
                                      size = Number), col = colour) +
    scale_size(labels = scales::comma) +
    scale_linewidth(labels = scales::comma) +
    theme_void() +
    labs(tag = tags[1])
  theme(legend.position = "bottom")
  # create map by value
  map_value <- ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "#DDDDDD", col = "white") +
    geom_path(data = map_path, aes(x = lon, y = lat, group = country, linewidth = Value_USD),
              alpha = 0.5, col = colour) +
    geom_point(data = map_points, aes(x = longitude, y = latitude,
                                      size = Value_USD), col = colour) +
    scale_size(name = "Value ($ USD)", labels = scales::comma) +
    scale_linewidth(name = "Value ($ USD)", labels = scales::comma) +
    theme_void() +
    labs(tag = tags[2])
  theme(legend.position = "bottom")
  return(map_number + map_value)
}
