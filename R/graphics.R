precip_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99")

#' Precipitation line plot
#'
#' Function for plotting monthly time-series of area averaged precipitation.
#'
#' @param x  a pRecipe data.table.
#' @return ggplot object
#' @export

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
      labs(x = "Time", y = "Precipitation in [mm]", color = "Data set", title = paste0("Monthly Average inside (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
      guides(color = guide_legend(override.aes = list(size = 2))) +
      scale_x_date(date_breaks = "years", date_labels = "%Y", expand = c(0.01,0.01)) +
      theme(panel.grid.minor.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    dummie_table <- copy(x)
    dummie_table <- dummie_table[, Z := as.yearmon(Z)][,.(value = mean(value, na.rm = TRUE), name = "average"), by = .(Z)]
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
#' @export

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
#' @export

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

#' Precipitation map
#'
#' Function for plotting precipitation maps.
#'
#' @param x  a pRecipe data.table.
#' @param monthly logical. If TRUE will generate one plot per month.
#' @return list with ggplot objects
#' @export

plot_map <- function(x, monthly = FALSE){
  dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
  dummie_year <- c(min(year(x$Z)), max(year(x$Z)))
  dummie_res <- abs(unique(x$y)[2] - unique(x$y)[1])
  dummie_min <- copy(x)
  dummie_min <- dummie_min[, value := mean(value, na.rm = TRUE), by = .(x, y, month(Z), name)][, .(value)] %>% min() %>% floor()
  dummie_max <- copy(x)
  dummie_max <- dummie_max[, value := mean(value, na.rm = TRUE), by = .(x, y, month(Z), name)][, .(value)] %>% max() %>% ceiling()
  if (monthly == TRUE){
    x <- split(x, month(x$Z))
    x <- lapply(x, function(month){
      month <- month[, value := mean(value, na.rm = TRUE), by = .(x, y, name)][, .(x, y, month(Z), value, name)] %>% unique()
      precip_plot <- ggplot(month, aes(x = x, y = y)) + 
        geom_raster(aes(fill = value)) +
        borders("world", xlim = c(dummie_box[1], dummie_box[3]), ylim = c(dummie_box[2], dummie_box[4]), colour = "black") +
        scale_fill_viridis(direction = -1, limits = c(dummie_min, dummie_max)) +
        facet_wrap(~name) +
        theme_bw() +
        coord_cartesian(xlim = c(dummie_box[1] - dummie_res/2, dummie_box[3] + dummie_res/2), ylim = c(dummie_box[2] - dummie_res/2, dummie_box[4] + dummie_res/2)) +
        labs(x = "Longitude", y = "Latitude", fill = "Precipitation [mm]", title = paste0(month.abb[month$V3[1]], " Average between ", dummie_year[1], "-", dummie_year[2]))
      return(precip_plot)
    })
    return(x)
    } else {
      x <- x[, year_value := sum(value, na.rm = TRUE), by = .(year(Z), x, y, name)][, ':='(year_mean = mean(year_value, na.rm = TRUE), year_sd = sd(year_value, na.rm = TRUE)), by = .(x, y, name)]
      mean_plot <- ggplot(x, aes(x = x, y = y)) + 
        geom_raster(aes(fill = year_mean)) +
        borders("world", xlim = c(dummie_box[1], dummie_box[3]), ylim = c(dummie_box[2], dummie_box[4]), colour = "black") +
        scale_fill_viridis(direction = -1) + 
        facet_wrap(~name) +
        theme_bw() +
        coord_cartesian(xlim = c(dummie_box[1] - dummie_res/2, dummie_box[3] + dummie_res/2), ylim = c(dummie_box[2] - dummie_res/2, dummie_box[4] + dummie_res/2)) +
        labs(x = "Longitude", y = "Latitude", fill = "Precipitation [mm]", title = paste0("Annual Average between ", dummie_year[1], "-", dummie_year[2]))
      std_plot <- ggplot(x, aes(x = x, y = y)) + 
        geom_raster(aes(fill = year_sd)) +
        borders("world", xlim = c(dummie_box[1], dummie_box[3]), ylim = c(dummie_box[2], dummie_box[4]), colour = "black") +
        scale_fill_viridis(direction = -1) +
        facet_wrap(~name) +
        theme_bw() +
        coord_cartesian(xlim = c(dummie_box[1] - dummie_res/2, dummie_box[3] + dummie_res/2), ylim = c(dummie_box[2] - dummie_res/2, dummie_box[4] + dummie_res/2)) +
        labs(x = "Longitude", y = "Latitude", fill = "Precipitation [mm]", title = paste0("Annual Standard Deviation between ", dummie_year[1], "-", dummie_year[2]))
      precip_plot <- list(mean_plot, std_plot)
      return(precip_plot)
  }
}

#' Precipitation matrix
#'
#' Function for plotting monthly precipitation matrices.
#'
#' @param x  a pRecipe data.table.
#' @return list with ggplot objects
#' @export
 
plot_matrix <- function(x, monthly = FALSE){
  dummie_box <- c(min(x$x), min(x$y), max(x$x), max(x$y))
  dummie_min <- copy(x)
  dummie_min <- dummie_min[, value := mean(value, na.rm = TRUE), by = .(month(Z), year(Z), name)][, .(value)] %>% min() %>% floor()
  dummie_max <- copy(x)
  dummie_max <- dummie_max[, value := mean(value, na.rm = TRUE), by = .(month(Z), year(Z), name)][, .(value)] %>% max() %>% ceiling()
  x <- x[, value := mean(value, na.rm = TRUE), by = .(month(Z), year(Z), name)][, .(Z, value, name)] %>% unique()
  precip_plot <- ggplot(x, aes(x = year(Z), y = month(Z))) + 
    geom_raster(aes(fill = value)) +
    scale_fill_viridis(direction = -1, limits = c(dummie_min, dummie_max)) +
    scale_x_continuous(breaks = seq(min(year(x$Z)), max(year(x$Z))), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(min(month(x$Z)), max(month(x$Z))), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), expand = c(0,0)) +
    facet_wrap(~name) +
    theme_bw() +
    labs(x = NULL, y = NULL, fill = "[mm]", title = paste0("Average Monthly Precipitation inside (", dummie_box[1], ", ", dummie_box[2], ", ", dummie_box[3], ", ", dummie_box[4], ")")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(precip_plot)
}