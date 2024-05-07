#' Plot Density Comparison
#' 
#' Plot the density comparison of two variables with a legend.
#' 
#' @param original_var The original variable for density plot comparison.
#' @param imputed_var The imputed variable for density plot comparison.
#' 
#' @return A density plot comparing the original and imputed variables with a legend.
#' 
#' @export
#' 
#' @examples
#' plot_density_single_with_legend(original_var, imputed_var)
plot_density_single_with_legend <- function(original_var, imputed_var) {
  original_density <- ggplot(data = NULL, aes(x = original_var, fill = "Original")) +
    geom_density(alpha = 0.5) +
    geom_density(data = NULL, aes(x = imputed_var, fill = "Imputed"), alpha = 0.5) +
    labs(title = "Density Plot Comparison") +
    scale_fill_manual(name = "Data", values = c("Original" = "blue", "Imputed" = "red")) +
    theme_minimal()
  
  print(original_density)
}

#' Plot Boxplots
#' 
#' Plot boxplots for the columns of a dataset.
#' 
#' @param data The dataset to plot boxplots for.
#' @param title The title for the boxplot.
#' 
#' @return A boxplot for the columns of the dataset.
#' 
#' @export
#' 
#' @examples
#' plot_boxplots(data, title = "Boxplot of Columns")
plot_boxplots <- function(data, title = "Boxplot of Columns") {
  # Convert data to long format for boxplot
  data_long <- reshape2::melt(data)
  
  # Plot vertical boxplots
  ggplot(data_long, aes(x = variable, y = value)) +
    geom_boxplot() +
    labs(title = title, x = "Columns", y = "Values") +
    coord_flip() # Rotate the plot to make it vertical
}

#' Plot Heatmap
#' 
#' Plot a heatmap for the dataset experiment.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param na_colour The color for missing values.
#' 
#' @return A heatmap plot for the dataset experiment.
#' 
#' @export
#' 
#' @examples
#' plot_heatmap(dataset_experiment, na_colour = "#FF00E4")
plot_heatmap <- function(dataset_experiment, na_colour = "#FF00E4") {
  # Create a heatmap of the data
  C <- DatasetExperiment_heatmap(na_colour = na_colour)
  chart_plot(C, dataset_experiment)
}

#' Plot PCA
#' 
#' Plot a PCA plot for the dataset experiment.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param factor_name The factor column name.
#' @param nPCs The number of principal components for PCA.
#' 
#' @return A PCA plot for the dataset experiment.
#' 
#' @export
#' 
#' @examples
#' plot_pca(dataset_experiment, factor_name = "sample_type", nPCs = 5)
plot_pca <- function(dataset_experiment, factor_name = "sample_type", nPCs = 5) {
  M <- knn_impute() + mean_centre() + PCA(number_components = nPCs)
  M <- model_apply(M, dataset_experiment)
  C <- pca_scores_plot(factor_name = factor_name, ellipse_type = "norm")
  chart_plot(C, M[3])
}

#' Plot Outliers
#' 
#' Plot outliers for the dataset experiment.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for PCA.
#' @param out_dir The output directory for saving the plot.
#' @param out_name The output name for saving the plot.
#' 
#' @return Outlier plots saved in the specified directory with the specified name.
#' 
#' @export
#' 
#' @examples
#' plot_outliers(dataset_experiment, nPCs = 5, out_dir, out_name)
plot_outliers <- function(dataset_experiment, nPCs = 5, out_dir, out_name) {
  plt1 <- plot_hotelling_pca(dataset_experiment, nPCs = 5)
  plt2 <- plot_hotelling_obs(dataset_experiment, nPCs = 5, nPCs_to_plot = 2)
  plt <- cowplot::plot_grid(plt1, plt2, ncol = 2)
  save_plot(plt, out_dir, out_name)
}

#' Plot Hotelling's T-squared vs. Observations
#' 
#' Plot Hotelling's T-squared vs. Observations for the dataset experiment.
#' 
#' @param dataset_experiment The dataset experiment object.
#' @param nPCs The number of principal components for PCA.
#' @param nPCs_to_plot The number of principal components to plot.
#' 
#' @return A plot of Hotelling's T-squared vs. Observations.
#' 
#' @export
#' 
#' @examples
#' plot_hotelling_obs(dataset_experiment, nPCs = 5, nPCs_to_plot = 2)
plot_hotelling_obs <- function(dataset_experiment, nPCs = 5, nPCs_to_plot = 2) {
  # Perform PCA
  M <- structToolbox::knn_impute() + structToolbox::mean_centre() + structToolbox::PCA(number_components = nPCs)
  M <- structToolbox::model_apply(M, dataset_experiment)
  
  # Extract pca_scores
  pca_scores <- M[3]$scores$data %>% as_tibble()
  
  # Calculate Hotelling's T2 ellipse params
  res_PCs <- ellipseParam(data = pca_scores, k = nPCs_to_plot)
  
  # Plot
  plt <- tibble(
    T2 = purrr::pluck(res_PCs, "Tsquare", "value"),
    obs = rownames(pca_scores)
  ) %>%
    ggplot() +
    geom_point(aes(x = obs, y = T2, fill = T2), shape = 21, size = 3, color = "black") +
    geom_segment(aes(x = obs, y = T2, xend = obs, yend = 0), linewidth = .5) +
    scale_fill_gradient(low = "black", high = "red", guide = "none") +
    geom_hline(yintercept = pluck(res_PCs, "cutoff.99pct"), linetype = "dashed", color = "darkred", linewidth = .5) +
    geom_hline(yintercept = pluck(res_PCs, "cutoff.95pct"), linetype = "dashed", color = "darkblue", linewidth = .5) +
    annotate("text", x = 80, y = 13, label = "99% limit", color = "darkred") +
    annotate("text", x = 80, y = 9, label = "95% limit", color = "darkblue") +
    labs(
      x = "Observations", y = paste0("Hotelling’s T-squared (", nPCs_to_plot, "PCs)"),
      fill = "T2 stats", caption = "Hotelling’s T-squared vs. Observations"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
  return(plt)
}
