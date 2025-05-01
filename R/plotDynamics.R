#' Plot barcode dynamics
#'
#' This function plots the dynamics of barcode frequencies over time, using either linear-scale
#' area plots, logarithmic-scale line plots, or both. Only the most frequent barcodes are colored.
#'
#' @param reshaped_df A dataframe produced by `reshapeData()`, containing barcode frequencies over time.
#' @param colored_topFreq_df A dataframe with top barcodes and their assigned color hex codes and max frequencies.
#' @param min_freq_threshold A numeric threshold; barcodes with max frequency below this are colored gray.
#' @param plot_model One of `"linear"`, `"logarithmic"`, or `"both"` to specify the plot type(s).
#'
#' @import ggplot2
#' @importFrom ggnewscale new_scale_fill
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by filter ungroup
#' @export
#' @name plotDynamics


plotDynamics <- function(reshaped_df, colored_topFreq_df, min_freq_threshold, plot_model) {

  # Drop unused column
  colored_topFreq_df$max <- NULL
  
  # Merge dataframes and assign default gray color
  colored_df <- merge(reshaped_df, colored_topFreq_df, by = "ID", all.x = TRUE)
  colored_df$hex[is.na(colored_df$hex)] <- "#cccccc"
  
  # Ensure ID is treated as a factor
  colored_df$ID <- factor(colored_df$ID)
  
  # Order for proper area stacking
  colored_df <- colored_df[order(colored_df$max), ]
  
  # Subset barcodes exceeding threshold
  grouped_df <- colored_df %>%
    dplyr::group_by(hex) %>%
    dplyr::filter(max > min_freq_threshold) %>%
    dplyr::ungroup()
  
  # Color scale
  mycolors <- grouped_df$hex
  names(mycolors) <- grouped_df$ID
  
  # Preserve ordering for stacked areas
  grouped_df$ID <- factor(grouped_df$ID, levels = unique(grouped_df$ID[order(grouped_df$max)]))
  colored_df$ID <- factor(colored_df$ID, levels = unique(colored_df$ID[order(colored_df$max)]))
  
  x_breaks <- sort(unique(reshaped_df$Time))
  
  # ----- LINEAR AREA PLOT -----
  if (plot_model %in% c("linear", "both")) {
    message("Rendering linear-scale area plot. This may take a few minutes...")
    
    g <- ggplot(colored_df) +
      geom_area(aes(x = Time, y = Frequency, group = ID, fill = ID)) +
      scale_fill_manual(values = LONG_COLOR_LIST_RAND, guide = "none") +
      ggnewscale::new_scale_fill() +
      geom_area(data = grouped_df, aes(x = Time, y = Frequency, group = ID, fill = ID)) +
      scale_fill_manual(values = mycolors, name = "Cluster ID", guide = "none") +
      scale_x_continuous(limits = range(x_breaks)) +
      theme_Publication() +
      labs(x = "Time (generations)", y = "Barcode frequency") +
      coord_cartesian(expand = FALSE)
    
    ggsave(
      filename = paste0(output_directory, input_name, "_area.jpg"),
      plot = g,
      width = 8.25,
      height = 6,
      dpi = 300
    )
  }
  
  # ----- LOGARITHMIC LINE PLOT -----
  if (plot_model %in% c("logarithmic", "both")) {
    grDevices::cairo_ps(
      file = paste0(output_directory, input_name, "_line.eps"),
      width = 8.25,
      height = 6
    )
    
    all_line <- ggplot() +
      geom_line(data = colored_df, aes(x = Time, y = Frequency, group = ID), color = "#CCCCCC", alpha = 0.3) +
      geom_line(data = grouped_df, aes(x = Time, y = Frequency, group = ID, color = ID), linewidth = 1) +
      scale_color_manual(values = mycolors, name = "Cluster ID") +
      scale_y_log10(limits = c(min(colored_df$Frequency) + 1e-7, 1)) +
      scale_x_continuous(limits = range(x_breaks)) +
      theme_Publication() +
      labs(x = "Time", y = "Barcode frequency") +
      guides(color = "none", shape = guide_legend(order = 1)) +
      coord_cartesian(expand = FALSE)
    
    print(all_line)
    grDevices::dev.off()
  }

}
