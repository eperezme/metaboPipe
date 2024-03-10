# load xlsx
library(openxlsx)

file <- "Training/Data/pr5b00354_si_002.xls"
#load data
data_pos <- read_xls(file, sheet = 1) 
data_neg <- read_xls(file, sheet = 2)
#load annotations
sampledata <- read_xls(file, sheet = 3)
metabolitedata <- read_xls(file, sheet = 4)

