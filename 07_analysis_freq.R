####FREQUENCY ANALYSIS####
# This analysis file is concerned with analyzing the type frequency of the expected sets
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
core.source.path = "/Users/danielparsons/dparsonsiuj@gmail.com - Google Drive/My Drive/01_Research/R Scripts/ZipfianDist/ZipfianDistV2/analysis/"
sampling.conditions = paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
sampling.conditions = data.table::fread(sampling.conditions)


####1. Analyze Average Type Frequency: Long Texts####
# Lexical bundles from long texts are in conditions 1 - 25
# Lexical phrase frames from long texts are in conditions 76 - 100
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 1, end = 25)
bundles.long = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 76, end = 100)
frames.long = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.long
frames.long

####COMMENTARY####
# At around c4_4, the expected number of bundles extracted from any sized corpus becomes stable at around 28 bundles
# At c3_2 and c2_2, there is an increase in the expected number of bundles extracted as the corpus size increases, and this is
# is most significant as the corpus drops from 500,000 to 300,000 at the c2_2 level.
# This implies that if we wish to compare corpora, the expected set will be consistent for any corpus size with a c4_2 set of conditions.
# That is VERY interesting!


####2. Analyze Average Type Frequency: Mid Texts####
# Lexical bundles from mid texts are in conditions 26 - 50
# Lexical phrase frames from mid texts are in conditions 101 - 125
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 26, end = 50)
bundles.mid = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 101, end = 125)
frames.mid = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.mid
frames.mid

####COMMENTARY####
# We find the same pattern in the mid-length texts as in the long-length texts


####3. Analyze Average Type Frequency: Short Texts####
# Lexical bundles from short texts are in conditions 51 - 75
# Lexical phrase frames short long texts are in conditions 126 - 150
analysis.path = paste(core.source.path, "analysis_2.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 51, end = 75)
bundles.short = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeExpectedTypeFreqs(core.data.path, sampling.conditions, start = 126, end = 150)
frames.short = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = expected.type.freq, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
bundles.short
frames.short

####COMMENTARY####
# We find the same pattern in the short-length texts as in the mid-length and long-length texts
# The other interesting finding is that text-size effects appear to disappear, too.
# In other words, increasing or decreasing the length of the texts in the corpus appears to be insignificant in the expected sets.

####4. Compare charts####
fig1 <- ggarrange(bundles.long, bundles.mid, bundles.short, frames.long, frames.mid, frames.short,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 3, nrow = 2)
fig1 <- annotate_figure(fig1, top = text_grob("Effects of varying corpus size on bundle/frame expected set counts",
                                              color = "red", face = "bold", size = 16))
fig1