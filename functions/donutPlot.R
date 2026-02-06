#' donutPlot
#' 
#' Create a donut plot showing proportions of farmed vs. wild caught of a group
#' or taxon.
#'
#' @param data_frame (class data.frame) data frame to be plotted. Must include a 
#' Captive_wild field and a per_num field.
#' @param title (class string) plot title.
#'
#' @export
#'
donutPlot <- function(data_frame, title){
  donut <- data.frame(captive_bred = data_frame[, Captive_wild],
                      ymax = cumsum(data_frame[, per_num]),
                      y = data_frame[, per_num]) %>% data.table()
  donut$ymin <- c(0, head(donut[, ymax], n = -1))
  donut_plot <- ggplot(donut, aes(ymax = ymax, 
                                  ymin = ymin, 
                                  xmax = 4, 
                                  xmin = 3.5, 
                                  fill = captive_bred)) +
    geom_rect() +
    scale_fill_manual(values = captive_palette, name = "Farmed or wild") +
    coord_polar(theta = "y") +
    xlim(c(3, 4)) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    ggtitle(title) 
  return(donut_plot)
}