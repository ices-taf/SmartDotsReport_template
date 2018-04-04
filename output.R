

# load libraries
library(icesTAF)

# make output directory
mkdir("output")

# simply copy all files from model to output folder
cp(paste0("model/", dir("model")), "output")
