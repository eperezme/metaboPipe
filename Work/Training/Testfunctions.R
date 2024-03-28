suppressPackageStartupMessages({
  # Bioconductor packages
  library(SummarizedExperiment)
  library(structToolbox)
  library(pmp)
  library(ropls)
  library(BiocFileCache)
  
  # CRAN libraries
  library(ggplot2)
  library(gridExtra)
  library(cowplot)
  library(openxlsx)
  library(VIM)
  library(dplyr)
})


# use the BiocFileCache
bfc <- BiocFileCache(ask = FALSE)

# path to zip
zipfile = "https://raw.github.com/STATegraData/STATegraData/master/Script_STATegra_Metabolomics.zip"

## retrieve from BiocFileCache
path = bfcrpath(bfc,zipfile)
temp = bfccache(bfc)

## ... or download to temp location
# path = tempfile()
# temp = tempdir()
# download.file(zipfile,path)

# unzip
unzip(zipfile=path, files = "LC_MS_raw_data.xlsx", exdir=temp)

# read samples
data <- as.data.frame(read.xlsx(file.path(temp,"LC_MS_raw_data.xlsx"),sheet = 'Data'))

# Save data as a csv file
# write.csv(data, file = "data.csv")

##### PREPARING DATA FOR EXPERIMENT ######


# Create SampleMetadata dataframe
SM <- data[,1:7]
SM$sample_id = SM$label

# Define QC and blank samples
blanks=c(1,2,33,34,65,66)
QCs=c(3,4,11,18,25,32,35,36,43,50,57,64)

SM$sample_type = "Sample"
SM$sample_type[blanks] = "Blank"
SM$sample_type[QCs] = "QC"


# Create variableMetadata object
VM = data.frame('annotation'=colnames(data)[8:ncol(data)])

# Create the raw data matrix
X = data[,8:ncol(data)]
X$sample_id = SM$sample_id

# Save data as csv files
write.csv(X, file = "data/dataMatrix.csv", row.names = FALSE)
write.csv(SM, file = "data/sampleMetadata.csv", row.names = FALSE)
write.csv(VM, file = "data/variableMetadata.csv", row.names = FALSE)


# Create the experiment
DE <- createExperiment(X, SM, VM, "LCMS Raw test", "Description Test")
DE

####### FILTERING AND IMPUTATION #######

# Filter NA
filtered_experiment <- filter_MV(DE)

filtered_experiment


## impute

QRImputed_experiment <- impute_QRILC(filtered_experiment)
RFImputed_experiment <- impute_RF(filtered_experiment)


## SBCORRECTION (QC-RSC) to correct for signal drift and batch
B <- sb_corr(order_col = "order", batch_col = "biol.batch", qc_col = "sample_type", qc_label = 'QC')
B <- model_apply(B, DE)

test <- predicted(B)
     
