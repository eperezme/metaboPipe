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
    coord_flip()  # Rotate the plot to make it vertical
}