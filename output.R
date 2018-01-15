

# load libraries
library(icesTAF)

# make output directory
mkdir("output")

# simply copy all files from model to output folder
for (file in dir("model")) {
  cp(paste0("model/", file), paste0("output/", file))
}
