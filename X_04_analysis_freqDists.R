# This analysis plots curves to examine the distribution of types across the samples
#

library(tidyverse)
library(data.table)
library(doParallel)
library(devtools)
library(ggplot2)
library(ggpubr)

####Initial set up####
# Set the core file path from which all files can be read
# Note that Steps 1 and 2 cannot be performed without the original BAWE files
# in csv form.

core.data.path = "/Volumes/Corpora/BAWE/BAWE_metadata_2022/"
core.source.path = "/Users/danielparsons/Google drive/My Drive/MS OneDrive/01_Research/R Scripts/ZipfianDist/ZipfianDistV2/analysis/"
sampling.conditions = paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
sampling.conditions = data.table::fread(sampling.conditions)

####1. Prepare data ####
analysis.path = paste(core.source.path, "summarizeDists.R", sep = "")
source(analysis.path)

purpose = c("bundles")
subCorpusType = c("long", "mid", "short")
subCorpusLength = c(1000000)
comparison = c("freq")
rangeConditions = c("c2_2")

sample.data = summarizeDistributions(core.data.path,
                                     sampling.conditions,
                                     purpose,
                                     subCorpusType,
                                     subCorpusLength,
                                     comparison,
                                     rangeConditions)

print(sample.data)
sample.data = rbindlist(sample.data)

#### OUTLIER IDENTIFICATION ####
sample.data = sample.data %>% group_by(type) %>% arrange(-nrows) %>% ungroup()
View(sample.data)
# It seems that there are one or two huge outliers in the data.
# For example, c2_2 at 1,000,000 words, there is one major outlier in the mid-length text samples
# Note that to find exactly which sample it comes from, uncomment the above commented out code - items at the top
# of the sample.data view are the outliers - this will be obvious when looking at the column "nrows"
# The reason for this is not clear, but it may be best to remove those outliers as they clearly skew the data.
# I will carry out an investigation to try to understand what the outliers are
# For example: sample 8415 in mid length texts, c2_2 for 1,000,000 words (row 26 on sampling.conditions csv)

#### REMOVE OUTLIERS ####
sample.data = sample.data %>% group_by(type) %>% mutate(iqr = IQR(nrows), 
                                                        q1 = quantile(nrows, probs=c(.25), na.rm=FALSE),
                                                        q3 = quantile(nrows, probs=c(.75), na.rm=FALSE)) %>%
  filter(!nrows > q3 + 5*iqr) %>% 
  filter(!nrows < q1 - 5*iqr) %>% ungroup()

print("NUMBER OF SAMPLES INCLUDED AFTER REMOVING OUTLIERS")
print(nrow(sample.data))

####2. Summarize data ####
sample.data.summary = sample.data %>% group_by(type) %>% summarize(mean = mean(nrows),
                                                                   max = max(nrows),
                                                                   min = min(nrows),
                                                                   range = max(nrows) - min(nrows))

####3. Prepare plot ####
this.plot <- sample.data %>% ggplot(aes(x=nrows, color=type)) + 
  geom_histogram(binwidth=1, alpha=0.5, fill="white", position="dodge") +
  labs(x = "Type frequency (1,000,000 words)", y = "Number of samples") +
  geom_vline(data=sample.data.summary, aes(xintercept=mean, color=type), linetype="dashed") +
  theme(legend.position="top")

sample.data.summary
this.plot

#### INVESTIGATE OUTLIER ####
# Why is there an outlier in the sample?
# What precisely has caused it?
# Is it a particular type of lexical bundle or phrase frame?
# Find the sample that contains 503 types of bundle, which should be file 8415 in the list of files

dt.path = paste(core.data.path, "Results/26_freq_prop_bundles_mid_1000000_20_c2_2/", sep="")
samples.path = list.files(dt.path, full.names=TRUE)
samples.path[8413]
dt = data.table::fread(samples.path[8413])
num.bundles = nrow(dt)
View(dt)
# The incredible thing is that these appear to be legitimate at first glance!
# Let's call in the index and see WHICH bundles they actually are.

bundles.path = paste(core.data.path, "indexFiles/BAWE_4grams.csv", sep ="")
bundles.index = data.table::fread(bundles.path)
dt = dt %>% left_join(bundles.index, by = c("frames" = "index"))
View(dt)
# Holy moly!
# There is a big problem somewhere!
# The frequency of the bundles in the sample appears to be larger than the total
# frequency of the bundles in the whole corpus!
# That simply cannot be correct, which means something has gone terribly wrong somewhere
# I will have to go back over ALL my calculations and try to find out what has gone wrong.
# Though I should try to keep in mind to check whether the frequency in the sample is the total
# raw frequency or the normalized frequency. If it's the normalized frequency, then it would
# be okay for them to be larger sometimes than the overall frequency in the whole corpus
# I can check this for other samples, too.

dt_filter = dt %>% filter(freq.x > freq.y)
print(dt_filter)

# Take a look at the sample of files
samples.path = paste(core.data.path, "Samples/freq_prop_bundles_mid_1000000_20_c2_2.gz", sep = "")
samples.to.examine = data.table::fread(samples.path)
samples.to.examine[, grouping:=rleid(sample)]
samples.to.examine = samples.to.examine %>% group_by(grouping) %>% summarize(total.word.count = sum(wordCount))
View(samples.to.examine)

# I have found the problem.
# Look at row 8571. It has a total word count in the sample that is
# double the word count that it should be
# The likely reason for this is line 38 in lexicalSampler.R which
# is replicated above:
# samples.to.examine[, grouping:=rleid(sample)]
# If the sample variable has two consecutive samples that are labelled the same,
# which is improbable but possible, then this phenomenon will occur
# In fact, this is likely to happen if the first sample in the batch 
# is the only one accepted on TWO or more occasions.

# See where this happens specifically - it happens with sample 1.
samples.path = paste(core.data.path, "Samples/freq_prop_bundles_mid_1000000_20_c2_2.gz", sep = "")
samples.to.examine = data.table::fread(samples.path)
samples.to.examine[, grouping:=rleid(sample)]
samples.to.examine = samples.to.examine %>% group_by(grouping) %>% mutate(total.word.count = sum(wordCount))
View(samples.to.examine)

# Try an alternative grouping system to solve the problem
# This alternative solves the problem, though potentially introduces a very minor
# delay on performance.
# It groups according to discipline and not sample
samples.path = paste(core.data.path, "Samples/freq_prop_bundles_mid_1000000_20_c2_2.gz", sep = "")
samples.to.examine = data.table::fread(samples.path)
samples.to.examine[, grouping_disc:=rleid(discipline)]
samples.to.examine = samples.to.examine %>% mutate(grouping = if_else(grouping_disc%%4 == 0,
                                                                      grouping_disc/4,
                                                                      (1 + ((grouping_disc - (grouping_disc%%4))/4)))) 
samples.to.examine.new = samples.to.examine %>% group_by(grouping) %>% mutate(total.word.count = sum(wordCount))
View(samples.to.examine.new)

# It seems that the outlier problem has been fixed.
####SUMMARY OF THE PROBLEM AND SOLUTION####
# Written up on May 10th.
# The name of this file was changed to X_04_analysis_freqDists.R on May 10th
# to reflect the problems that were identified.
# In generating samples, sets of 250 were were taken.
# Within those sets, the number of words was counted.
# Samples outside a given word-count range (see samplingConditions.csv in baweCorporaMetadata folder)
# were rejected to ensure the corpus size is consistent.
# During this process, each sample of 250 was enumerated with an id starting at 1 up to 250.
# The id was the SAME for each sample set (to reduce programmatic looping and so to save processing time)
# Those sample sets that met the word-count criteria were saved to a file in the Samples folder
# with the id given to them.
# However, this led to a problem.
# Some sample sets kept only the first sample, which was given id = 1.
# If this occurred in two consecutive sample sets, then when they were saved, there would be TWO
# sample sets with the same id next to each other.
# This was a problem for the rleid function used in lexicalSampler.R program to give new ids to each sample set
# (The new ids were necessary because the approximately 10,000 saved sample sets would contain duplicate ids out of 1-250.)
# Because rleid only attaches a new id when the sample id changes, if two consecutive samples contain the same
# id, rleid would consider the two samples as one single sample.
# This led to the creation of samples that were doubled in size, which meant that there were significant outliers
# in the counts of lexical bundles and lexical phrase frames extracted.
# The solution to this problem was to have rleid run on the disciplinary label and then create a grouping id
# that updated every 4 rleid labels because there are 4 disciplines. This works because the disciplines were sampled
# one at a time meaning the samples are organized by discipline. This solution would not work if the samples were
# not organized by discipline and it might be difficult to organize the data by discipline.








