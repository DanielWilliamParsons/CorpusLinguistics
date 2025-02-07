####FREQUENCY ANALYSIS####
# This analysis file is concerned with analyzing the average type frequency
# of both lexical bundles and lexical phrase frames extracted
# from varying length corpora under varying FREQUENCY conditions.

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
# Lexical bundles from long texts are in conditions 1 - 25
# Lexical phrase frames from long texts are in conditions 76 - 100
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 1, end = 25)
bundles.long = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start = 76, end = 100)
frames.long = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
#bundles.long
#frames.long

####COMMENTARY####
# As can be seen in the charts, increasing the size of the corpus (from 300000 to 1000000) has the effect of
# reducing the number of lexical bundles and lexical phrase frames extracted from the corpus
# In other words, this research supports the findings of Bestgen (2019) that a smaller corpus will 
# result in a larger number of lexical bundles, and extends those findings to the case of
# lexical phrase frames.
# Bestgen (2019) only examined normalized frequency thresholds of 20 per million and 40 per million
# whereas in this research, five frequency thresholds have been investigated.
# The effect is most pronounced for both lexical bundles and lexical phrase frames when the threshold
# frequency is lower, and becomes less pronounced as the threshold frequency increases.
# This can be observed by noticing that the steepness of each line in the charts is much less prominent
# for a normalized frequency threshold of 60 per million (c6_2) and much more pronounced at 20 per million (c2_2)
# This is true for both lexical bundles and lexical phrase frames.

####2. Analyze Average Type Frequency: Mid Texts####
# Lexical bundles from mid texts are in conditions 26 - 50
# Lexical phrase frames from mid texts are in conditions 101 - 125
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 26, end = 50)
bundles.mid = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start = 101, end = 125)
frames.mid = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
#bundles.mid
#frames.mid

####COMMENTARY####
# We find the same pattern in the mid-length texts as in the long-length texts


####3. Analyze Average Type Frequency: Short Texts####
# Lexical bundles from short texts are in conditions 51 - 75
# Lexical phrase frames from short texts are in conditions 126 - 150
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 51, end = 75)
bundles.short = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start = 126, end = 150)
frames.short = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
#bundles.short
#frames.short

####COMMENTARY####
# We find the same pattern in the short-length texts as in the mid-length and long-length texts

####4. Compare charts####
fig1 <- ggarrange(bundles.long, bundles.mid, bundles.short, frames.long, frames.mid, frames.short,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 3, nrow = 2)
fig1 <- annotate_figure(fig1, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                      color = "red", face = "bold", size = 16))
fig1



