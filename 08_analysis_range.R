####RANGE ANALYSIS####
# This analysis file is concerned with analyzing the expected type frequency
# of both lexical bundles and lexical phrase frames extracted
# from varying length corpora under varying RANGE conditions.

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


####1. Analyze Average Type Frequency: Long Texts####
# Lexical bundles from long texts are in conditions 151 - 200
# Lexical phrase frames from long texts are in conditions 301 - 350
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 151, end = 200)
#counts.summary.bundles = counts.summary.bundles %>% filter(rangeConditions != "c4_1")
bundles.long = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 301, end = 350)
#counts.summary.frames = counts.summary.frames %>% filter(rangeConditions != "c4_1")
frames.long = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.long
frames.long

####COMMENTARY####


####2. Analyze Average Type Frequency: Mid Texts####
# Lexical bundles from mid texts are in conditions 201 - 250
# Lexical phrase frames from mid texts are in conditions 351 - 400
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 201, end = 250)
#counts.summary.bundles = counts.summary.bundles %>% filter(rangeConditions != "c4_1")
bundles.mid = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start =351, end = 400)
#counts.summary.frames = counts.summary.frames %>% filter(rangeConditions != "c4_1")
frames.mid = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.mid
frames.mid

####COMMENTARY####
# We find the same pattern in the mid-length texts as in the long-length texts


####3. Analyze Average Type Frequency: Short Texts####
# Lexical bundles from short texts are in conditions 251 - 300
# Lexical phrase frames from short texts are in conditions 401 - 450
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 251, end = 300)
#counts.summary.bundles = counts.summary.bundles %>% filter(rangeConditions != "c4_1")
bundles.short = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 401, end = 450)
#counts.summary.frames = counts.summary.frames %>% filter(rangeConditions != "c4_1")
frames.short = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.short
frames.short

####COMMENTARY####
# We find the same pattern in the short-length texts as in the mid-length and long-length texts

####4. Compare charts####
fig1 <- ggarrange(bundles.long, bundles.mid, bundles.short, frames.long, frames.mid, frames.short,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 3, nrow = 2)
fig1 <- annotate_figure(fig1, top = text_grob("Effects of varying corpus size on bundle/frame expected type frequencies across varying range conditions",
                                              color = "red", face = "bold", size = 16))
fig1

####FINAL COMMENTARY####
# The effects are the same as for frequency threshold comparisons.
# There is a little fluctuation/noise, more so maybe that in the frequency threshold comparisons.
# It looks as though there is a lot of noise in the short-length texts corpus
# but that is a result of the y-axis range being smaller due to an effect of no range conditions (c4_1)
# Removing this range condition from the data frames for each chart re-scales the y-axis so that they are similar in all charts
# and shows that the noisy pattern is still apparent.
# In sum, range conditions appear to add noise to the average set across corpora of different sizes.
# It may be that we need many, many, many more samples in order for the noise to reduce.
# It could be similar to the effect of a biased die roll compared with an unbiased die roll.
# In the central limit theorem, an unbiased die roll tends to a normal distribution in only a few rolls
# However, an unbiased die roll appears to need many, many more rolls than before it starts to tend towards the normal distribution.
# As a result, it may be advisable to not include stringent range conditions if making comparisons across corpora.

####METHOD OF EXTRACTING THE EXPECTED SET####
#### Expected Set Per Sub-corpus ####
# This is basically a comparison of lexical bundles WITHIN a corpus
# Range = minimum of 3 texts is recommended, but it's probably not necessary to include this
# since we are using a sampling technique which will filter out anything unexpected due to being in up to three documents too much.
# Frequency threshold of 40 per million is recommended for stable comparisons
# Determine the components (sub-corpora) of the corpus you wish to compare, e.g., speech vs writing, disciplines, first language groupings
# We probably want to make sure that each sub-corpus is at least 500,000 words long
# Sample each component 10,000 times.
# Make sure each sample is at least 300,000 words
# Count bundles in each corpus
# Filter bundles according to criteria
# Take this intermediary set and count again in the samples
# Take the mean and 95% confidence intervals for these means and then filter again on the upper confidence level
# This final set is the expected set according to the central limit theorem and law of large numbers



