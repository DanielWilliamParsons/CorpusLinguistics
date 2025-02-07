# This script extracts the bundles based on the following criteria
# 10,000 samples are selected.
# A bundle must occur in at least 9500 (95%) of the samples.
# All lexical bundle comparisons can be found in the 
# "frequency threshold sample size decision.xlsx" document in corpusMetadata folder.

library(tidyverse)
library(data.table)
library(doParallel)
library(devtools)
library(ggplot2)
library(ggpubr)

####Initial set up####
# Set the core file path from which all files can be read

core.data.path = "/Volumes/Corpora/BAWE/BAWE_metadata_2022/"
core.source.path = "/Users/danielparsons/Google drive/My Drive/MS OneDrive/01_Research/R Scripts/ZipfianDist/ZipfianDistV2/extractSamples/"

analysis.path = paste(core.source.path, "extractSamples.R", sep = "")
source(analysis.path)

sampling.conditions = data.table::fread(paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep = ""))

items.file = "BAWE_4grams.csv" #bundles
for(j in 301:450){
  
  folder.to.extract = paste(j, 
                            sampling.conditions$comparison[j],
                            sampling.conditions$balance[j],
                            sampling.conditions$purpose[j],
                            sampling.conditions$subCorpusType[j],
                            sampling.conditions$subCorpusLength[j],
                            sampling.conditions$normFreqThresh[j],
                            sampling.conditions$rangeConditions[j],
                            sep = "_")
  print(folder.to.extract)
  if(sampling.conditions$purpose[j] == "bundles"){
    items.file = "BAWE_4grams.csv"
  } else {
    items.file = "BAWE_4frames.csv"
  }
  
  print(items.file)
  
  sample = extractSamples(core.data.path, folder.to.extract, items.file)
  
}


# However, this sampling approach is not the AVERAGE set of bundles.
# For that, we need to know the average frequency of each bundle.
# Then we need to extract those bundles whose average frequency across all 10,000 samples is greater than the threshold frequency.
# To do that I have to run a program similar to lexicalSampler.R
# I can start with the bundles so far extracted. These are the bundles which are definitely above the frequency and range thresholds
# in a certain number of samples.
# So, I can run similar routines as lexicalSampler.R, but this time just for the extracted bundles.
# The purpose is to get their TRUE means of frequencies and ranges. In this way, we can select those bundles
# whose expected values for frequencies and range meets the criteria, leading to the set of items which is the expected set.

# The following will calculate mean, sd, standard error, and upper and lower confidence limits at 95%
# for the frequency, text-range and discipline range.

expected.set.path = paste(core.source.path, "expectedSet.R", sep = "")
source(expected.set.path)

expectedSet(core.data.path)
sessionInfoPath = paste(core.data.path, "SessionInfo/62_expectedSets.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)

# Now extract the expected set based on the confidence interval.
# Use the upper confidence limit to extract the expected set.

extract.expected.set.path = paste(core.source.path, "extractExpectedSet.R", sep ="")
source(extract.expected.set.path)

extractExpectedSet(core.data.path)
sessionInfoPath = paste(core.data.path, "SessionInfo/63_extractExpectedSet.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)







