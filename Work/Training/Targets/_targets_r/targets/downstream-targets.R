list(
  tar_file_read(dataMatrix, dataMatrixPath, read.csv(!!.x)),
  tar_file_read(sampleMetadata, sampleMetadataPath, read.csv(!!.x)),
  tar_file_read(variableMetadata, variableMetadataPath, read.csv(!!.x))
)
