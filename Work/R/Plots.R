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
  library(structToolbox)
  C = mv_sample_filter() + mv_sample_filter_hist()
  chart_plot(C, dataset_experiment)
}

# PLOT DENSITIES BEFORE AND AFTER
ba_plot <- function(DE_before, DE_after, factor_name = "sample_type") {
  C <- compare_dist(factor_name = factor_name)
  plot <- chart_plot(C, DE_before, DE_after)
  return(plot)
}

batch_plot <- function(dataset_experiment, order_col, batch_col, qc_col, qc_label, colour_by_col, feature_to_plot, ylab = 'Peak area', title = 'Feature vs run_order') {
  corrected_feature_name <- make.names(feature_to_plot) # Make sure the feature name is the same as the column name in the dataset
  
  C = feature_profile(
    run_order=order_col,
    qc_label=qc_label,
    qc_column=qc_col,
    colour_by=colour_by_col,
    feature_to_plot=corrected_feature_name,
    plot_sd=FALSE
  )
  
  chart_plot(C, dataset_experiment) + ylab(ylab) + ggtitle(title)
}


#### PLOT PCA ####
plot_pca <- function(dataset_experiment, factor_name = "sample_type", nPCs = 5) {
  M <- knn_impute() + mean_centre() + PCA(number_components = nPCs)
  M <- model_apply(M, dataset_experiment)
  C <- pca_scores_plot(factor_name = factor_name, ellipse_type = "norm")
  chart_plot(C, M[3])
}



library(HotellingEllipse)
library(tidyverse)
library(ggforce)


M <- knn_impute() + mean_centre() + PCA(number_components = 5)
M <- model_apply(M, experiment)
pca_scores <- M@models[[3]]@scores@value@assays@data@listData[[1]] %>% as_tibble()
res_2PCs <- ellipseParam(data = pca_scores, k = 2, pcx = 1, pcy = 2)
T2 <- pluck(res_2PCs, "Tsquare", "value")
a2 <- pluck(res_2PCs, "Ellipse", "a.95pct")
b2 <- pluck(res_2PCs, "Ellipse", "b.95pct")
pca_scores %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed", fill = "white") +
  geom_point(aes(fill = T2), shape = 21, size = 3, color = "black") +
  scale_fill_viridis_c(option = "viridis") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = .2) +
  labs(title = "Scatterplot of PCA scores", subtitle = "PC1 vs. PC2", x = "PC1", y = "PC2", fill = "T2", caption = "Figure 1: Hotelling's T2 ellipse obtained\n using the ellipseParam function") +
  theme_grey()

res_PCs <- ellipseParam(data = pca_scores, k = 2)
tibble(
  T2 = pluck(res_PCs, "Tsquare", "value"), 
  obs = 1:nrow(pca_scores)
) %>%
  ggplot() +
  geom_point(aes(x = obs, y = T2, fill = T2), shape = 21, size = 3, color = "black") +
  geom_segment(aes(x = obs, y = T2, xend = obs, yend = 0), size = .5) +
  scale_fill_gradient(low = "black", high = "red", guide = "none") +
  geom_hline(yintercept = pluck(res_PCs, "cutoff.99pct"), linetype = "dashed", color = "darkred", size = .5) +
  geom_hline(yintercept = pluck(res_PCs, "cutoff.95pct"), linetype = "dashed", color = "darkblue", size = .5) +
  annotate("text", x = 80, y = 13, label = "99% limit", color = "darkred") +
  annotate("text", x = 80, y = 9, label = "95% limit", color = "darkblue") +
  labs(x = "Observations", y = "Hotelling’s T-squared (3 PCs)", fill = "T2 stats", caption = "Figure 3: Hotelling’s T-squared vs. Observations") +
  theme_bw()



cutoff_95 <- pluck(res_PCs, "cutoff.95pct")
cutoff_99 <- pluck(res_PCs, "cutoff.99pct")
# Select the rownames of the observations that are above the 95% limit
outliers_95 <- pca_scores %>% mutate(obs = rownames(pca_scores)) %>%
  filter(T2 > cutoff_95)
# Select the rownames of the observations that are above the 99% limit
outliers_99 <- pca_scores %>% mutate(obs = rownames(pca_scores)) %>%
  filter(T2 > cutoff_99)

experiment$sample_meta[outliers_95$obs, ]

 Filt <- filter_by_name(mode = 'exclude', dimension = 'sample', outliers_95$obs)


pca_hotelling <- funciton(dataset_experiment, nPCs=5){}


