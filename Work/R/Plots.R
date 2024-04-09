# Function to plot density plots for one variable with a legend
plot_density_single_with_legend <- function(original_var, imputed_var) {
  original_density <- ggplot(data = NULL, aes(x = original_var, fill = "Original")) +
    geom_density(alpha = 0.5) +
    labs(title = "Density Plot Comparison") +
    geom_density(data = NULL, aes(x = imputed_var, fill = "Imputed"), alpha = 0.5) +
    labs(title = "Density Plot Comparison") +
    scale_fill_manual(name = "Data", values = c("Original" = "blue", "Imputed" = "red")) +
    theme_minimal()

  print(original_density)
}


plot_boxplots <- function(data, title = "Boxplot of Columns") {
  # Convert data to long format for boxplot
  data_long <- reshape2::melt(data)

  # Plot vertical boxplots
  ggplot(data_long, aes(x = variable, y = value)) +
    geom_boxplot() +
    labs(title = title, x = "Columns", y = "Values") +
    coord_flip() # Rotate the plot to make it vertical
}


distribution_boxplot <- function(dataset_experiment, number = 50, plot_by = c("samples", "metabolites"), factor_name, per_class = TRUE ) {
  # If the plot_by argument is not valid, stop the function
  if (!plot_by %in% c("samples", "metabolites")) {
    stop("Invalid plot_by argument. Must be either 'samples' or 'metabolites'.")
  }
  # If plot_by is samples, make by_sample == TRUE
  if (plot_by == "samples") {
    by_sample <- TRUE
  } else {
    by_sample <- FALSE
  }
  C <- DatasetExperiment_boxplot(factor_name= factor_name, number = number, by_sample = by_sample, per_class = per_class)
  chart_plot(C, dataset_experiment)
}


plot_heatmap <- function(dataset_experiment, na_colour = "#FF00E4") {
  # Create a heatmap of the data
  C <- DatasetExperiment_heatmap(na_colour = na_colour)
  chart_plot(C, dataset_experiment)
}

missing_values_plot <- function(dataset_experiment) {
  VIM::aggr(SummarizedExperiment::assay(dataset_experiment))
}

distribution_boxplot <- function(dataset_experiment, factor_name, per_class = FALSE ) {
  C <- DatasetExperiment_dist(factor_name= factor_name, per_class = per_class)
  chart_plot(C, dataset_experiment)
  }

sample_missing_values_plot <- function(dataset_experiment) {
  
  C = mv_sample_filter() + mv_sample_filter_hist()
  chart_plot(C, dataset_experiment)
}

# PLOT DENSITIES BEFORE AND AFTER
ba_plot <- function(DE_before, DE_after, factor_name = "sample_type") {
  C <- compare_dist(factor_name = factor_name)
  plot <- chart_plot(C, DE_before, DE_after)
  return(plot)
}