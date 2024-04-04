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




mSet <- InitDataObjects("conc", "stat", FALSE);
mSet <- Read.TextData(mSet, "data/metaboData.csv")
mSet<-SanityCheckData(mSet);

mSet<-ReplaceMin(mSet);
mSet<-PreparePrenormData(mSet);
mSet<-Normalization(mSet, "NULL", "LogNorm", "MeanCenter", "", ratio=FALSE, ratioNum=20);
mSet<-PlotNormSummary(mSet, "norm_0_", format ="svg", dpi=72, width=NA);
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", format = "svg", dpi=72, width=NA)



plot(mSet$snorm_0_)
