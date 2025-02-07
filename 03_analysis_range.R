####RANGE ANALYSIS####
# This analysis file is concerned with analyzing the average type frequency
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
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 151, end = 200)
bundles.long = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start = 301, end = 350)
frames.long = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
#bundles.long
#frames.long

####COMMENTARY####
# The effect of range is extremely significant in smaller corpora for both bundles and lexical phrase frames.
# The more stringent the range conditions (least stringent is c4_1, most stringent is c4_10)
# the fewer the bundles/frames that are extracted.
# For smaller corpora, the gap between the extreme range condition are the widest.
# This gap converges as the corpus size increases, but there is still a gap at the one-million word-corpus.
# It should be noted that the gap converges the most for the one-dimensional range condition of minimum number of texts only.
# However, once the second condition of minimum number of texts per discipline are included, this has the greatest effect.
# Another notable effect is due to the inclusion of two dimensions of range criteria as the corpus size decreases.
# Two dimensions of range appear to have the effect of reducing the number of bundles and phrase frames extracted from samples
# as the corpus size decreases. This can be seen most prominently at c_10, c_9 and c_8. However, even at c_7, c_6 and c_5, while the
# number of bundles appears to rise slightly as corpus size decreases, eventually a smaller corpus leads to a large reduction in the
# average number of bundles/frames extracted.
# This implies that a second dimension of range has the potential to overcome the Zipfian effect described by Bestgen (2019).
# It essentially constrains the potential for new bundles or frames to emerge by virtue of corpus size and instead requires
# that the emergence of a frame or bundle is dependent more strongly on an overlap between, in this case, definitions of genre.
# While this adds a constraint to the total number of types that can be extracted, it makes the extracted bundles more comparable
# across studies. The occurrence of a bundle extracted under frequency and one-dimensional range conditions only cannot be attributed
# to a Zipfian effect or a genre effect, thus making it difficult to conclude that a bundle is indeed a feature of a particular genre. 
# However, if the addition of a second dimension of range appears to suppress a Zipfian effect
# then we can make stronger claims about the generic nature of a lexical bundle or frame.

####2. Analyze Average Type Frequency: Mid Texts####
# Lexical bundles from mid texts are in conditions 201 - 250
# Lexical phrase frames from mid texts are in conditions 351 - 400
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 201, end = 250)
bundles.mid = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start =351, end = 400)
frames.mid = counts.summary.frames %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Print the plots - uncomment to print
#bundles.mid
#frames.mid

####COMMENTARY####
# We find the same pattern in the mid-length texts as in the long-length texts


####3. Analyze Average Type Frequency: Short Texts####
# Lexical bundles from short texts are in conditions 251 - 300
# Lexical phrase frames from short texts are in conditions 401 - 450
analysis.path = paste(core.source.path, "analysis_1.R", sep = "")
source(analysis.path)

# Bundles
counts.summary.bundles = analyzeCounts(core.data.path, sampling.conditions, start = 251, end = 300)
bundles.short = counts.summary.bundles %>% 
  ggplot( aes(x = subCorpusLength, y = mean.num.types, group = rangeConditions, color = rangeConditions)) +
  geom_line()

# Frames
counts.summary.frames = analyzeCounts(core.data.path, sampling.conditions, start = 401, end = 450)
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
fig1 <- annotate_figure(fig1, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates across varying range conditions",
                                              color = "red", face = "bold", size = 16))
fig1

####FINAL COMMENTARY####
# Comparing long, medium and short length sample types: (This needs its own analysis section and different charts for clarity)
# Bundles: least restrictive range conditions: number of bundles extracted decreases as text length decreases.
# Bundles: mid restrictive range conditions (c_5): number of bundles extracted increases as text length decreases.
# Frames follow the same pattern as bundles.
# This is just preliminary observation and needs proper analysis.



