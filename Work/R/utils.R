# Function to convert DatasetExperiment to a data frame for metaboanalyst

library(MetaboAnalystR)
DE_to_metaboanalyst <- function(dataset_experiment) {
  # Extract the data matrix
  data_matrix <- SummarizedExperiment::assay(dataset_experiment)
  
  # Extract the sample metadata
  sample_metadata <- DataFrame::as.data.frame(colData(dataset_experiment))
  
  # Extract the variable metadata
  variable_metadata <- DataFrame::as.data.frame(rowData(dataset_experiment))
  
  # Convert the data matrix to a data frame
  data_frame <- as.data.frame(data_matrix)
  
  # Add the sample metadata to the data frame
  data_frame <- cbind(sample_metadata, data_frame)
  
  # Add the variable metadata to the data frame
  data_frame <- cbind(variable_metadata, data_frame)
  
  return(data_frame)

}

# Load a dataframe to metaboanalystR data object
# mSet <- InitDataObjects("conc", "stat", FALSE);
# mSet <- Read.TextData(mSet, "")
wd <- getwd()
out_dir <- "analysis"
plot_dir <- "plots"
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

setwd(file.path(wd, out_dir))
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}
mSet <- InitDataObjects("conc", "stat", FALSE);
mSet <- Read.TextData(mSet, filePath = file.path(wd, "data/metaboData.csv"))
mSet<-SanityCheckData(mSet);

# mSet<-ReplaceMin(mSet);
mSet <- RemoveMissingPercent(mSet, percent=0.5) #Remove features containing a user-defined % cut-off of missing values
# mSet <- ImputeMissingVar(mSet, method="exclude") #Remove variables with missing values
mSet <- ImputeMissingVar(mSet, method="knn_smp") #Replace missing values with KNN imputed values   

# Check if the sample size is too small, returns a 0 if the data passes the check
mSet<-IsSmallSmplSize(mSet)

mSet<-PreparePrenormData(mSet);
mSet<-Normalization(mSet, "NULL", "LogNorm", "MeanCenter", "", ratio=FALSE, ratioNum=20);
mSet<-PlotNormSummary(mSet, file.path(plot_dir, "norm_0_"), format ="svg", dpi=72, width=NA);
mSet<-PlotSampleNormSummary(mSet, file.path(plot_dir, "snorm_0_"), format = "svg", dpi=72, width=NA)


