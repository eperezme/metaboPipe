#' Plot density plots for one variable with a legend
#' 
#' This function generates density plots for one variable, comparing the original and imputed data, with a legend indicating the data source.
#' 
#' @param original_var The original variable data.
#' @param imputed_var The imputed variable data.
#' @return A ggplot object displaying the density plots.
#' @export
#' @examples
#' plot_density_single_with_legend(original_var = data$original_var, imputed_var = data$imputed_var)
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

#' Plot boxplots for multiple columns
#' 
#' This function generates vertical boxplots for multiple columns of a dataset.
#' 
#' @param data The dataset containing the columns to be plotted.
#' @param title The title of the plot.
#' @return A ggplot object displaying the boxplots.
#' @export
#' @examples
#' plot_boxplots(data = my_data, title = "Boxplot of Columns")
plot_boxplots <- function(data, title = "Boxplot of Columns") {
  # Convert data to long format for boxplot
  data_long <- reshape2::melt(data)
  
  # Plot vertical boxplots
  ggplot(data_long, aes(x = variable, y = value)) +
    geom_boxplot() +
    labs(title = title, x = "Columns", y = "Values") +
    coord_flip() # Rotate the plot to make it vertical
}

#' Plot a heatmap of the data
#' 
#' This function generates a heatmap of the provided dataset.
#' 
#' @param dataset_experiment The dataset for which the heatmap will be generated.
#' @param na_colour The color to represent missing values in the heatmap.
#' @return A ggplot object displaying the heatmap.
#' @export
#' @examples
#' plot_heatmap(dataset_experiment = my_dataset, na_colour = "#FF00E4")
plot_heatmap <- function(dataset_experiment, na_colour = "#FF00E4") {
  # Create a heatmap of the data
  C <- DatasetExperiment_heatmap(na_colour = na_colour)
  chart_plot(C, dataset_experiment)
}

#' Generate a missing values plot
#' 
#' This function generates a missing values plot using the VIM package's aggr function and saves the plot to a specified directory with a given filename.
#' 
#' @param dataset_experiment The dataset for which the missing values plot will be generated.
#' @param out_dir The directory where the plot will be saved.
#' @param out_name The filename for the saved plot.
#'
#' @return NULL
#' @export
#' @examples
#' missing_values_plot(dataset_experiment = my_dataset, out_dir = "output", out_name = "missing_plot.png")
missing_values_plot <- function(dataset_experiment, out_dir, out_name) {
  plt <- VIM::aggr(SummarizedExperiment::assay(dataset_experiment))
  metaboPipe::save_plot(plt, out_dir, out_name)
}


#' Generate a distribution boxplot
#' 
#' This function generates a distribution boxplot for a specified factor in the dataset.
#' 
#' @param dataset_experiment The dataset for which the boxplot will be generated.
#' @param factor_name The name of the factor variable.
#' @param per_class Logical indicating whether to generate separate boxplots for each class of the factor.
#' @return A ggplot object displaying the distribution boxplot.
#' @export
#' @examples
#' distribution_boxplot(dataset_experiment = my_dataset, factor_name = "factor", per_class = TRUE)
distribution_boxplot <- function(dataset_experiment, factor_name, per_class = FALSE) {
  C <- DatasetExperiment_dist(factor_name = factor_name, per_class = per_class)
  chart_plot(C, dataset_experiment)
}

#' Generate a before-after plot
#' 
#' This function generates a before-after plot comparing distributions before and after a certain process or treatment.
#' 
#' @param DE_before The dataset before the process or treatment.
#' @param DE_after The dataset after the process or treatment.
#' @param factor_name The name of the factor variable for stratification.
#' @return A ggplot object displaying the before-after plot.
#' @export
#' @examples
#' ba_plot(DE_before = before_data, DE_after = after_data, factor_name = "sample_type")
ba_plot <- function(DE_before, DE_after, factor_name = "sample_type") {
  C <- compare_dist(factor_name = factor_name)
  plot <- chart_plot(C, DE_before, DE_after)
  return(plot)
}

#' Generate a batch plot
#' 
#' This function generates a batch plot showing the relationship between a feature and run order, stratified by batches and quality control (QC) samples.
#' 
#' @param dataset_experiment The dataset for which the batch plot will be generated.
#' @param order_col The column representing run order.
#' @param batch_col The column representing batches.
#' @param qc_col The column representing QC samples.
#' @param qc_label The label for QC samples.
#' @param colour_by_col The column used for coloring in the plot.
#' @param feature_to_plot The feature to be plotted.
#' @param ylab The label for the y-axis.
#' @param title The title of the plot.
#' @return A ggplot object displaying the batch plot.
#' @export
#' @examples
#' batch_plot(dataset_experiment = my_dataset, order_col = "order", batch_col = "batch", qc_col = "qc", qc_label = "qc_label", colour_by_col = "colour", feature_to_plot = "feature", ylab = "Peak area", title = "Feature vs run_order")
batch_plot <- function(dataset_experiment, order_col, batch_col, qc_col, qc_label, colour_by_col, feature_to_plot, ylab = "Peak area", title = "Feature vs run_order") {
  corrected_feature_name <- make.names(feature_to_plot) # Make sure the feature name is the same as the column name in the dataset
  
  C <- feature_profile(
    run_order = order_col,
    qc_label = qc_label,
    qc_column = qc_col,
    colour_by = colour_by_col,
    feature_to_plot = corrected_feature_name,
    plot_sd = FALSE
  )
  
  chart_plot(C, dataset_experiment) + ylab(ylab) + ggtitle(title)
}

#' Generate a PCA plot
#' 
#' This function generates a PCA plot showing the principal component scores colored by a specified factor.
#' 
#' @param dataset_experiment The dataset for which the PCA plot will be generated.
#' @param factor_name The name of the factor variable used for coloring.
#' @param nPCs The number of principal components to include in the analysis.
#' @return A ggplot object displaying the PCA plot.
#' @export
#' @examples
#' plot_pca(dataset_experiment = my_dataset, factor_name = "sample_type", nPCs = 5)
plot_pca <- function(dataset_experiment, factor_name = "sample_type", nPCs = 5) {
  M <- knn_impute() + mean_centre() + PCA(number_components = nPCs)
  M <- model_apply(M, dataset_experiment)
  C <- pca_scores_plot(factor_name = factor_name, ellipse_type = "norm")
  chart_plot(C, M[3])
}

#' Generate an outliers plot
#' 
#' This function generates a plot showing outliers detected using Hotelling's T-squared statistic in PCA.
#' 
#' @param dataset_experiment The dataset for which the outliers plot will be generated.
#' @param nPCs The number of principal components to include in the analysis.
#' @param out_dir The directory where the plot will be saved.
#' @param out_name The filename for the saved plot.
#' @return NULL
#' @export
#' @examples
#' plot_outliers(dataset_experiment = my_dataset, nPCs = 5, out_dir = "output", out_name = "outliers_plot.png")
plot_outliers <- function(dataset_experiment, nPCs = 5, out_dir, out_name) {
  plt1 <- plot_hotelling_pca(dataset_experiment, nPCs = 5)
  plt2 <- plot_hotelling_obs(dataset_experiment, nPCs = 5, nPCs_to_plot = 2)
  plt <- cowplot::plot_grid(plt1, plt2, ncol = 2)
  metaboPipe::save_plot(plt, out_dir, out_name)
}

#' Generate a PCA Hotelling's T-squared plot
#' 
#' This function generates a PCA Hotelling's T-squared plot showing the principal component scores and confidence ellipses.
#' 
#' @param dataset_experiment The dataset for which the plot will be generated.
#' @param nPCs The number of principal components to include in the analysis.
#' @return A ggplot object displaying the PCA Hotelling's T-squared plot.
#' @export
#' @examples
#' plot_hotelling_pca(dataset_experiment = my_dataset, nPCs = 5)
plot_hotelling_pca <- function(dataset_experiment, nPCs = 5) {
  # Perform PCA
  M <- structToolbox::knn_impute() + structToolbox::mean_centre() + structToolbox::PCA(number_components = nPCs)
  M <- structToolbox::model_apply(M, dataset_experiment)
  
  # Extract pca_scores
  pca_scores <- M[3]$scores$data %>% as_tibble()
  
  # Calculate Hotelling's T2 ellipse params
  res_PCs <- HotellingEllipse::ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
  # Extract Hotelling's T2 values
  T2 <- purrr::pluck(res_PCs, "Tsquare", "value")
  
  # Extract ellipse params for plotting
  a99 <- purrr::pluck(res_PCs, "Ellipse", "a.99pct")
  b99 <- purrr::pluck(res_PCs, "Ellipse", "b.99pct")
  a95 <- purrr::pluck(res_PCs, "Ellipse", "a.95pct")
  b95 <- purrr::pluck(res_PCs, "Ellipse", "b.95pct")
  
  # Plot PCA scores
  plt <- pca_scores %>%
    ggplot(aes(x = PC1, y = PC2)) +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = a99, b = b99, angle = 0), linewidth = .5, linetype = "dotted", fill = "white") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = a95, b = b95, angle = 0), linewidth = .5, linetype = "dashed", fill = "white") +
    geom_point(aes(fill = T2), shape = 21, size = 3, color = "black") +
    scale_fill_viridis_c(option = "viridis") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .1) +
    labs(
      title = "Scatterplot of PCA scores", subtitle = "PC1 vs. PC2", x = "PC1", y = "PC2",
      fill = "T2", caption = "Hotelling's T2 ellipse with 99(exterior line) and 95(interior line) confidence intervals"
    ) +
    theme_bw() +
    theme(panel.grid = element_blank())
  
  return(plt)
}

#' Generate a PCA Hotelling's T-squared observations plot
#' 
#' This function generates a PCA Hotelling's T-squared observations plot showing the T-squared values for each observation.
#' 
#' @param dataset_experiment The dataset for which the plot will be generated.
#' @param nPCs The number of principal components to include in the analysis.
#' @param nPCs_to_plot The number of principal components to plot the ellipses for.
#' @return A ggplot object displaying the PCA Hotelling's T-squared observations plot.
#' @export
#' @examples
#' plot_hotelling_obs(dataset_experiment = my_dataset, nPCs = 5, nPCs_to_plot = 2)
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
