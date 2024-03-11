# load xlsx
# # Original code
data('sacurine',package = 'ropls')
# 
# sacurine$variableMetadata$annotation=rownames(sacurine$variableMetadata)
# rownames(sacurine$variableMetadata)=1:nrow(sacurine$variableMetadata)
# colnames(sacurine$dataMatrix)=1:ncol(sacurine$dataMatrix)
# 
# Bioconductor packages
library(structToolbox)
library(pmp)
library(ropls)
library(BiocFileCache)
library(readr)

# CRAN libraries
library(ggplot2)
library(gridExtra)
library(cowplot)
library(openxlsx)
library(readxl)

file <- "Training/Data/pr5b00354_si_002.xls"

# Load data
data_pos <- read.csv("Training/Data/data_pos.csv", sep = ";", row.names = 1, header = TRUE)
data_neg <- read.csv("Training/Data/data_neg.csv", sep = ";", row.names = 1, header = TRUE)

# Load annotations
sampledata <- read.csv("Training/Data/sampleMetadata.csv", sep = ";", row.names = 1, header = TRUE)
metabolitedata <- read.csv("Training/Data/metabolitedata.csv", sep = ";", row.names = 1, header = TRUE)

generate_experiment <- function(data_neg, sampleMetadata, metaboliteMetadata) {
  # Move the annotations to a new column and rename the features by index to avoid issues
  metaboliteMetadata$annotation <- rownames(metaboliteMetadata)
  rownames(metaboliteMetadata) <- NULL  # Clear existing row names
  
  # Assign row names directly to data frames
  rownames(data_neg) <- seq_len(nrow(data_neg))
  
  # Create DatasetExperiment
  DE <- DatasetExperiment(data = as.data.frame(data_neg),
                          sample_meta = sampleMetadata,
                          variable_meta = metaboliteMetadata,
                          name = 'Test',
                          description = 'Description test')
  return(DE)
}

# Test function
test <- generate_experiment(data_neg, sampledata, metabolitedata)



# 
# PCA <- function(DE, number_components = 5) {
#   # 1. Define the model
#   M = autoscale() + PCA(number_components = 5)
#   # 2. Fit the model
#   M = model_apply(M,DE)
#   
#   # pca scores plots
#   g=list()
#   for (k in colnames(DE$sample_meta)) {
#     C = pca_scores_plot(factor_name = k)
#     g[[k]] = chart_plot(C,M[2])
#   }
#   # plot using cowplot
#   plot_grid(plotlist=g, nrow=1, align='vh', labels=c('A','B','C'))
#   
#   return(M)
# }
# 
# PCA(test, 5)
# 
# PCA <- function(DE, number_components = 5) {
#   # 1. Define the model
#   M = autoscale() + PCA(number_components = 5)
#   # 2. Fit the model
#   M = model_apply(M, DE)
#   
#   # pca scores plots
#   g=list()
#   for (k in colnames(DE$sample_meta)) {
#     C = pca_scores_plot(factor_name = k)
#     g[[k]] = chart_plot(C, M[2])
#   }
#   # plot using cowplot
#   plot_grid(plotlist = g, nrow = 1, align = 'vh', labels = c('A', 'B', 'C'))
#   
#   return(M)
# }
# 
