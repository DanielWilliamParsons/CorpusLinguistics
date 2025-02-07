library(tidyverse)
library(data.table)
library(doParallel)
library(devtools)

####Initial set up####
# Set the core file path from which all files can be read
# Note that Steps 1 and 2 cannot be performed without the original BAWE files
# in csv form.

core.data.path = "/Volumes/Corpora/BAWE/BAWE_metadata_2022/"
core.source.path = "/Users/danielparsons/Google drive/My Drive/MS OneDrive/01_Research/R Scripts/ZipfianDist/ZipfianDistV2/"

####1: Create the bundles and frames files####
# Note that this part cannot be replicated without the csv data frames
# The csv data frames were created using the original BAWE XML files
# by extracting the data from the XML, tagging with CLAWS
# and rendering into a data frame.

####QUESTION####

# Can I make the csv BAWE files available on request
# without distributing the original corpus?

bundle.maker.path = paste(core.source.path, "bundleMaker.R", sep="")
source(bundle.maker.path)
bundleMaker(core.data.path)

# The above function creates the fileByFileData.csv in the corpusMetadata folder
# It also creates the individual file counts of lexical bundles and lexical
# phrase frames in the BAWE_4frames and BAWE_4grams folders

####2. Manage the lexical bundles and frames####
# Again, this cannot be performed without the original files
# The purpose of this step is to filter out the bundles and frames that will
# not be selected due to range criteria.
# The research will examine the impact of range criteria as well as frequency
# threshold criteria.
# However, because I am also interested in how range affects extracted bundles
# and frames, I will not set a frequency threshold for some cases.
# This means that only the range will determine the minimum frequency,
# Since I intend to include a minimum threshold frequency of 20 per million
# for a 300,000 word corpus, we only need bundles or frames that occur with min raw frequency of
# 6 or more.
manage.bundles.path = paste(core.source.path, "bundleManagement.R", sep="")
source(manage.bundles.path)
manageBundles(core.data.path, "BAWE_4grams")
manageBundles(core.data.path, "BAWE_4frames")

####3: Create index files for bundles and frames####
index.files.path = paste(core.source.path, "createIndexFiles.R", sep="")
source(index.files.path)
createIndexFiles(core.data.path, "BAWE_4grams")
createIndexFiles(core.data.path, "BAWE_4frames")

####4: Create the balanced corpora####
corpus.balancer.path = paste(core.source.path, "corpusBalancer.R", sep="")
source(corpus.balancer.path)
# Read in the corpusBalancerConditions.csv file
# This file provides the text-length criteria for the text-length variable values: Long, Medium and Short
# It also contains the names of the baseCorporaMetadata files
balance.criteria.path = paste(core.data.path, "Filepaths/corpusBalancerConditions.csv", sep="")
balance.criteria = data.table::fread(balance.criteria.path)
# Uncomment to inspect the data frame.
# print(balance.criteria)

for(i in 1:nrow(balance.criteria)){
  corpusBalancer(core.data.path, balance.criteria[i])
}
sessionInfoPath = paste(core.data.path, "SessionInfo/04_balanceCorpus.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)


####5: Sample the balanced corpora####

# The next step is to sample sub-corpora at the ten different word count levels.
# The word counts for sub-corpora were decided based on an analysis of how
# normalized frequency thresholds would affect raw frequencies.
# Each sub-corpus size will give a whole number for the raw frequency.
# The analysis can be found in the folder:
# corpusMetadata/frequency threshold sample size decision.xlsx
# An explanation of the analysis is contained in the sampleSizeAnalyzer.R code

####WARNING####
# Running this section of code will probably change the realPropMin and realPropMax
# column values in the baseCorporaMetadata/samplingConditions.csv file
# To see the original conditions and compare with them, make sure to take a copy
# of this file.
sample.size.analyzer.path = paste(core.source.path, "sampleSizeAnalyzer.R", sep="")
source(sample.size.analyzer.path)
analyzeSampleSize(core.data.path)

####6: Pilot taking the samples####

# The function here is a slower function that allows me to record data
# and check things like duplication of samples
# After running the pilot function below, it was determined that the sampling
# approach is accurate, duplicates are not a problem
# and so the fast sampling can be carried out
sampler.path = paste(core.source.path, "SampleTaker.R", sep="")
source(sampler.path)

conds.path = paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
conds = data.table::fread(conds.path)

start = Sys.time()
sequence = seq(from = 1, to = 60, by = 1)

multiCoreBoost = function(i){
  if(conds$balance[i] == "word") {
    takeWordBalancedSamples(core.data.path, conds[i], i)
  } else {
    takePropBalancedSamples(core.data.path, conds[i], i)
  }
}
mclapply(sequence, multiCoreBoost, mc.cores=5)
finish = Sys.time()
totalTime = finish - start
print(totalTime)


####7: Take the samples for real####
####TESTING####
# First I tested 1000 samples on the long corpus with the following allowed discipline variation for the word balanced sub-corpora
# Because the words have to be balanced, the conditions for a sample are strict and the probability that a sample is within the 
# the specified conditions is smaller than a corpus balanced by proportion of files.
# I therefore tested the speed of each condition on the long corpus to make sure the allowed discipline variation allows for
# reasonable speed of sampling with 6 corees used in parallel
# RESULTS
# 1,000,000-word sub corpus; allowedDisciplineVariation: 10,000 (Max: 1,040,000 Min: 960,000); Speed: 21.2 seconds
# 900,000-word sub-corpus;   allowedDisciplineVariation: 9,000  (Max: 936,000 Min: 864,000);   Speed: 21.6 seconds
# 800,000-word sub-corpus;   allowedDisciplineVariation: 8,000  (Max: 832,000 Min: 768,000);   Speed: 33.7 seconds
# 700,000-word sub-corpus;   allowedDisciplineVariation: 7,000  (Max: 728,000 Min: 672,000);   Speed: 58.7 seconds
# 600,000-word sub-corpus;   allowedDisciplineVariation: 6,000  (Max: 624,000 Min: 576,000);   Speed: 1.33 minutes
# 500,000-word sub-corpus;   allowedDisciplineVariation: 5,000  (Max: 520,000 Min: 480,000);   Speed: 2.42 minutes
# 400,000-word sub-corpus;   allowedDisciplineVariation: 4,000  (Max: 416,000 Min: 384,000);   Speed: 4.64 minutes
# 300,000-word sub-corpus;   allowedDisciplineVariation: 3,000  (Max: 312,000 Min: 288,000);   Speed: 8.12 minutes
# 200,000-word sub-corpus;   allowedDisciplineVariation: 2,500  (Max: 210,000 Min: 190,000);   Speed: 5.99 minutes
# 100,000-word sub-corpus;   allowedDisciplineVariation: 2,500  (Max: 110,000 Min: 90,000);    Speed: 2.79 minutes
# We would therefore expect 10,000 samples to take ten times longer.
# Approximately 5 hours for this set of corpora
# Since there are 6 versions of these, that is approximately 30 hours in total for the word balanced sub-corpora

# I also tested the corpus balanced by proportion of files for the long corpus
####RESULTS####
# 1,000,000-word sub corpus; Speed: 10.4 seconds
# 900,000-word sub-corpus;   Speed: 18.6 seconds
# 800,000-word sub-corpus;   Speed: 16.5 seconds
# 700,000-word sub-corpus;   Speed: 16.9 seconds
# 600,000-word sub-corpus;   Speed: 11.1 seconds
# 500,000-word sub-corpus;   Speed: 9.46 seconds
# 400,000-word sub-corpus;   Speed: 12.8 seconds
# 300,000-word sub-corpus;   Speed: 14.2 seconds
# 200,000-word sub-corpus;   Speed: 13.3 seconds
# 100,000-word sub-corpus;   Speed: 17.8 seconds
# From the above results we would expect approximately 30 minutes to collect 10,000 samples for the long corpus
# Therefore, approximately 3 hours in total.

####NOTE####
# After much testing of time and speed in step 9 below, the word-balanced corpus was dropped
# for the first part of this study.
# Therefore, the samplingConditions.csv file only contains the word "prop"
# under the balance column as of April 19th, 2022.
# I might return to this at a future date
# While I have managed to speed up the sampling process through the use of
# a bundles/phrases matrix and data.table functions instead of dplyr
# I have also realised that each frequency and range condition needs its
# own sample (whereas in the pilot project, I took filtered frequency and range
# conditions all on the sample single sample), so it will still take approximately
# 7 days to complete all the sampling.
# The samplingConditions.csv was updated to reflect this new approach
# which requires many, many more corpus file samples to be taken
# and the old version was renamed samplingConditions_old.csv, but remains in the
# baseCorporaMetadata folder.

####SAMPLING CORPUS FILES####
sampler.path = paste(core.source.path, "sampleTakerTestParallel.R", sep="")
source(sampler.path)
library(devtools)

conds.path = paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
conds = data.table::fread(conds.path)


for(w in 38:nrow(conds)) {
  start = Sys.time()
  sequence = seq(from = w, to = w, by = 1)
  print(w)
  multiCoreBoost = function(i){
    if(conds$balance[i] == "word") {
      takeWordBalancedSamplesParallel(core.data.path, conds[i], i)
      
    } else {
      takePropBalancedSamplesParallel(core.data.path, conds[i], i)
    }
  }
  lapply(sequence, multiCoreBoost)
  finish = Sys.time()
  totalTime = finish - start
  print(totalTime)
  gc()
}

####8. Create the bundle matrix####
# The bundle matrix is really a data frame
# Columns are the file names
# Bundles are the row names

matrix.path = paste(core.source.path, "theMatrixMaker.R", sep="")
source(matrix.path)

# These are a problem in frames, but I have since repaired this in bundleMaker.R
# and started again from Step 1.
# filter(grepl(" \\* .*$", frames))
# filter(grepl("^\\* .*$", frames))

makeTheMatrix(core.data.path, "BAWE_4frames")
makeTheMatrix(core.data.path, "BAWE_4grams")


####9. Take samples of the lexical bundles and lexical phrase frames####
library(R.utils)

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path)
f = Sys.time()
print(f-S)
gc()

####10. ERROR CHECKING####
# Given the following message after running the above on 151 - 450 for 1.413049 days
# I have decided to check for errors. The first error check will look to make sure
# that every saved file is not empty. This means looping through 4.5 million files
# and counting the rows
error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker2(core.data.path) #This is much, much faster than errorChecker
f = Sys.time()
print(f-S)
gc()
sessionInfoPath = paste(core.data.path, "SessionInfo/10_errorChecking.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)

# The error checker stopped at folder 187, after which is reported errors in all cores.
# There are 99 files in total, implying is successfully accessed and completed 99 folders worth of samples.
# There were 6 cores in use - and 99 %% 6 is 3, implying that either folders 188, 189 or 190 contain an error
# error_checker 3 will examine these folders and ascertain the nature of the error.

# The error message was:
# Error in rbindlist(sample) : 
# Item 1 of input is not a data.frame, data.table or list
# This implies that one or other of the samples has zero entries.
# This might be the case when a sample has very stringent range and frequency conditions, as occurs in c4_8
# So, I might expect to find empty data frames saved in the individual samples.
error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker3(core.data.path)
f = Sys.time()
print(f-S)
gc()

# I have found a file which cannot be opened properly or which is not read as a table
# It is in folder 188, sample_8229
# errorChecker4 examines the file

error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker4(core.data.path)
f = Sys.time()
print(f-S)
gc()

#### ERROR DISCOVERY####
# I have discovered the problem!
# There do not appear to be 10,000 samples in Results folder starting 188.
# In fact, there are only 8229! Coincidence that this is the file number? NO!
# I set the loop to be from 1 to 10000, so the program thinks there are more files in this folder!
# Now to discover what happened and why only 8229 files were saved and not 10000.
# My guess is that empty data frames were returned which were then not saved
# errorChecker5 will count the number of files per folder to investigate further.

error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker5(core.data.path)
f = Sys.time()
print(f-S)
gc()

# The check confirms that ONLY folder 188 contains a fewer number of files.
# Now to understand the error.
# Examine the samples file from which the error emerged.
# An initial look at the file does not indicate anything unusual in the 1.28 million rows

error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker6(core.data.path)
f = Sys.time()
print(f-S)
gc()

# It therefore seems necessary to run the sampling procedure from Step 9 again just on this
# sample in a non-parallel way so that the error can be found. I have a suspicion that the sample simply gave a
# table with NO results, i.e the count was 0 bundles. If so, I need to update the code to account for 0 hits
####ERROR CORRECTION TESTING####
# A new function was added to lexicalSampler.R, sampleLexicalFeaturesTesting(core.path, sample_start_ sample_end)
# in which the parallel processing is removed and data frames are not saved.
# The user selects which sample. Here, the sample selected is start_start = 188 and sample_end = 188
# so that we are just looking at one condition

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeaturesTesting(core.data.path, 188, 188)
f = Sys.time()
print(f-S)
gc()

# After running the testing feature, no error was found.
# This can be seen in the folder ErrorChecking, in the file error.sampling_188.csv
# 10000 sample were taken without error - all had propr data tables with the number of bundles indicated in the right-hand column
# I am therefore not sure I understand why only 8229 samples were saved.
# It may have been a problem with individual cores or it may have been a memory issue during the sampling process
# The only solution is to run the sampling procedure again and see if the computer saves 10,000 samples this time.
# The folder starting 188_ was moved out of the Results folder and placed into Results_error folder just for reference.
# Step 9 was run again, setting the for loop to 188:188

####ERROR CORRECTION####
library(R.utils)

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path)
f = Sys.time()
print(f-S)
gc()

# The error was successfully corrected. There are now 10,000 samples in folder 188.
# Now errorChecker7 will make sure that there are 450 folders of samples.

error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker7(core.data.path)
f = Sys.time()
print(f-S)
gc()

# Yes, there are 450 folders.
# Then run errorChecker5 again to make sure that there are 10,000 samples within each folder
error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker5(core.data.path)
f = Sys.time()
print(f-S)
gc()
# Yes there are 10,000 samples per folder!

####ERROR CORRECTION SUCCESS####
# The error correction process appears to have been successful.
# 10,000 samples were successfully saved in folder 188 and
# all the sample sets appear present as well as 10,000 samples per set!
# I can now continue with the counting and analysis despite the odd phenomenon that occurred!

####11. Collate the results####
# Next I need to collate the results
# I need to count the number of bundles/frames within each sample.
# In fact, errorChecker2 did this rather nicely, so I can just re-use the code
# and update it a little.
# All collated results can be found in the Results_collated folder.

error.checking.path = paste(core.source.path, "collateResults.R", sep="")
source(error.checking.path)
S = Sys.time()
collateResults(core.data.path)
f = Sys.time()
print(f-S)
sessionInfoPath = paste(core.data.path, "SessionInfo/11_collateResults.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
gc()
#It took 12.61132 hours to complete the counting process.

#### ERROR - MAY 6th ####
# Another error was found when looking through the data for analyses
# and the error is in the lexicalSampler.R function.
# This is going to be updated.

####12. Repeat step 9 - extracting bundle and phrase samples####
# This time two new input variables have been added to lexicalSampler.R
# These are range_start and range_end
# which allow me to set the program to run more than one time for each sample set.
# This simultaneously allows me to make sure that memory issues do not become a problem.
# i.e., I can use gc() each time.
library(R.utils)

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 1, 50)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 51, 100)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 101, 150)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 151, 200)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 201, 250)
f = Sys.time()
print(f-S)
gc()


lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 251, 300)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 301, 350)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 351, 400)
f = Sys.time()
print(f-S)
gc()

lexical.sampler.path = paste(core.source.path, "lexicalSampler.R", sep="")
source(lexical.sampler.path)
S = Sys.time()
sampleLexicalFeatures(core.data.path, 401, 450)
f = Sys.time()
print(f-S)
gc()

####13. Run errorChecker5.R####
# This is to ensure that there are 10,000 samples per set.
error.checking.path = paste(core.source.path, "errorChecking.R", sep="")
source(error.checking.path)
S = Sys.time()
errorChecker5(core.data.path)
f = Sys.time()
print(f-S)
gc()
# No problem - 10,000 samples have been taken.
# Obviously, the technique of breaking up the sampling into smaller sets of 50
# and clearing the memory every 50 prevented the loss of samples that occurred
# with the first run in step 9.

####14. Collate the results again####
# The results collated before May 6th are stored in Results_error folder now
# under Results_collated
# All newly collated results can be found in the Results_collated folder.
# These results were collated on May 9th.
collate.results.path = paste(core.source.path, "collateResults.R", sep="")
source(collate.results.path)
S = Sys.time()
collateResults(core.data.path)
f = Sys.time()
print(f-S)
sessionInfoPath = paste(core.data.path, "SessionInfo/11_collateResults_May9th.txt", sep="")
writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
gc()

# It took 12.31912 hours to reach this point.
# But, I'm sure it is all completed now at last!

