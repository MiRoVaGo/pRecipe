precip_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99")

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
      labs(x = "Time", y = "Precipitation in [mm]", color = "Data set", title = paste0("Monthly Average inside (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      scale_x_date(date_breaks = "years", date_labels = "%Y", expand = c(0.01,0.01)) +
      theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(precip_plot)
}

#' Precipitation bar plot
#'
#' Function for plotting (bar plot) monthly area averaged precipitation.
#'
#' @param x  a pRecipe data.table.
#' @return ggplot object

plot_bar <- function(x){
  dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
  dummie_year <- c(min(year(x$Z)), max(year(x$Z)))
  x <- x[, Z := as.yearmon(Z)][, value := mean(value, na.rm = TRUE), by = .(Z, name)][, ':='(mean = mean(value, na.rm = TRUE), std = sd(value, na.rm = TRUE)), by = .(month(Z), name)][, .(Z, mean, std, name)][, Z := month(Z)] %>% unique()
  precip_plot <- ggplot(x, aes(x = Z, y = mean, fill = name)) + 
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(aes(ymin = mean - std, ymax = mean + std), width = 0.4, position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = precip_colors) +
    theme_bw() +
    labs(x = "Month", y = "Precipitation in [mm]", fill = "Data set", title = paste0("Monthly Average between ", dummie_year[1], "-", dummie_year[2], " inside (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
    scale_x_continuous(breaks = seq(1, 12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  return(precip_plot)
}

#' Precipitation box plot
#'
#' Function for plotting (box plot) monthly area averaged precipitation.
#'
#' @param x  a pRecipe data.table.
#' @return ggplot object

plot_box <- function(x){
  dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
  dummie_year <- c(min(year(x$Z)), max(year(x$Z)))
  x <- x[, ':='(month = factor(month(Z)), year = year(Z))][, value := mean(value, na.rm = TRUE), by = .(month, year, name)][, .(month, year, value, name)] %>% unique()
  precip_plot <- ggplot(x, aes(x = month, y = value, fill = name)) +
    geom_boxplot(outlier.size = 0.25) +
    scale_fill_manual(values = precip_colors) +
    theme_bw() +
    labs(x = "Month", y = "Precipitation in [mm]", fill = "Data set", title = paste0("Monthly Average between ", dummie_year[1], "-", dummie_year[2], " inside (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
    scale_x_discrete(breaks = seq(1, 12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  return(precip_plot)
}