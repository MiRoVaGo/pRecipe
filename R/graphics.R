precip_colors <- c("#1f78b4", "#a6cee3", "#33a02c", "#b2df8a", "#e31a1c", "#fb9a99", "#ff7f00", "#fdbf6f", "#6a3d9a", "#cab2d6", "#b15928", "#ffff99")

#' Precipitation line plot
#'
#' Function for plotting monthly time-series of area averaged precipitation.
#'
#' @param x  a pRecipe data.table.
#' @return ggplot object

plot_line <- function(x){
  if (length(unique(x$name)) == 1){
    dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
    x <- x[, Z := as.yearmon(Z)][, value := mean(value, na.rm = TRUE), by = .(Z, name)][, .(Z, value, name)] %>% unique()
    x$Z <- as.Date.yearmon(x$Z)
    precip_plot <- ggplot(x, aes(x = Z, y = value, color = name)) + 
      geom_line() +
      geom_point(size = 0.5, show.legend = FALSE) +
      scale_color_manual(values = precip_colors) +
      theme_bw() +
      labs(x = "Time", y = "Precipitation in [mm]", color = "Data set", title = paste0("Monthly Average at (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      scale_x_date(date_breaks = "years", date_labels = "%Y", expand = c(0.01,0.01)) +
      theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    dummie_table <- x[, Z := as.yearmon(Z)][,.(value = mean(value, na.rm = TRUE), name = "average"), by = .(Z)]
    dummie_table$Z <- as.Date.yearmon(dummie_table$Z)
    dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
    x <- x[, Z := as.yearmon(Z)][, value := mean(value, na.rm = TRUE), by = .(Z, name)][, .(Z, value, name)] %>% unique()
    x$Z <- as.Date.yearmon(x$Z)
    precip_plot <- ggplot(x, aes(x = Z, y = value, color = name)) + 
      geom_line(alpha = 0.4, size = 1) +
      geom_line(data = dummie_table) +
      geom_point(data = dummie_table, size = 0.5, show.legend = FALSE) +
      scale_color_manual(values = precip_colors) +
      theme_bw() +
      labs(x = "Time", y = "Precipitation in [mm]", color = "Data set", title = paste0("Monthly Average at (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      scale_x_date(date_breaks = "years", date_labels = "%Y", expand = c(0.01,0.01)) +
      theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(precip_plot)
  
}