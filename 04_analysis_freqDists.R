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

core.data.path = "/Volumes/Corpora/BAWE/BAWE_metadata_2022/"
core.source.path = "/Users/danielparsons/Google drive/My Drive/MS OneDrive/01_Research/R Scripts/ZipfianDist/ZipfianDistV2/analysis/"
sampling.conditions = paste(core.data.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
sampling.conditions = data.table::fread(sampling.conditions)

####1. Prepare data for bundles ####
analysis.path = paste(core.source.path, "summarizeDists.R", sep = "")
source(analysis.path)

purpose = c("bundles")
subCorpusType = c("long", "mid", "short")
subCorpusLength = c(1000000, 900000, 700000, 500000, 300000)
comparison = c("freq")
rangeConditions = c("c2_2", "c3_2", "c4_2", "c5_2", "c6_2")
chart.label = c("Type frequency (1,000,000)", 
                "Type frequency (900,000)", 
                "Type frequency (700,000)", 
                "Type frequency (500,000)", 
                "Type frequency (300,000)")
print(chart.label)

####2. Create the plots for frequency distributions####
# There will be 25 plots for bundles and 25 plots for frames
# Each plot contains distributions for short, mid and long-length texts
# For each sub-corpus length there are 5 ranges.
# There are 5 sub-corpus lengths in total.

sample.data.summaries = list()
all.plots = list()
k = 1
for(i in 1:length(subCorpusLength)){
  
  for(j in 1:length(rangeConditions)){
    sample.data = summarizeDistributions(core.data.path,
                                         sampling.conditions,
                                         purpose,
                                         subCorpusType,
                                         subCorpusLength[i],
                                         comparison,
                                         rangeConditions[j])
    sample.data = rbindlist(sample.data)
    sample.data.summary = sample.data %>% group_by(type) %>% summarize(mean = mean(nrows),
                                                                       max = max(nrows),
                                                                       min = min(nrows),
                                                                       range = max(nrows) - min(nrows))
    final.chart.label = paste(chart.label[i], rangeConditions[j], sep = " ")
    this.plot <- sample.data %>% ggplot(aes(x=nrows, color=type)) + 
      geom_histogram(binwidth=1, alpha=0.5, fill="white", position="dodge") +
      labs(x = final.chart.label, y = "Number of samples") +
      geom_vline(data=sample.data.summary, aes(xintercept=mean, color=type), linetype="dashed") #+
      #theme(legend.position="top")
    
    sample.data.summaries[[k]] = sample.data.summary
    all.plots[[k]] = this.plot
    k = k + 1
    
  }
  
}



fig1 <- ggarrange(all.plots[[1]], all.plots[[2]], all.plots[[3]], all.plots[[4]], all.plots[[5]],
                  all.plots[[6]], all.plots[[7]], all.plots[[8]], all.plots[[9]], all.plots[[10]],
                  all.plots[[11]], all.plots[[12]], all.plots[[13]], all.plots[[14]], all.plots[[15]],
                  all.plots[[16]], all.plots[[17]], all.plots[[18]], all.plots[[19]], all.plots[[20]],
                  all.plots[[21]], all.plots[[22]], all.plots[[23]], all.plots[[24]], all.plots[[25]],
                  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                             "Q", "R", "S", "T", "U", "V", "W", "X", "Y"),
                  ncol = 5, nrow = 5)
fig1 <- annotate_figure(fig1, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                              color = "red", face = "bold", size = 16))
fig1
#Note that blue is for short, green is for medium and red is for long-length texts.

####3. Comments on bundles####
# The frequency distribution of types in bundles seems to depend slightly on text length.
# This appears to be most prominent for larger corpora.
# For 1,000,000-word corpora, the distribution of bundle type-frequencies for SHORT length texts 
# shifts from the left of the distribution for LONG and MEDIUM length texts to the right of the distribution
# as the threshold frequency increases (from c2_2 to c6_2, left to right on the top row of the chart.)
# For a smaller corpus (bottom row), this effect is not so prominent.
# Observe the average lines in the middle column (c4_2) from the top row to the bottom row.
# It can be seen that they converge.
# This implies that the effect of text length becomes an important factor for larger corpora, but less so for smaller
# corpora.


####4. Prepare data for frames ####
analysis.path = paste(core.source.path, "summarizeDists.R", sep = "")
source(analysis.path)

purpose = c("frames")
subCorpusType = c("long", "mid", "short")
subCorpusLength = c(1000000, 900000, 700000, 500000, 300000)
comparison = c("freq")
rangeConditions = c("c2_2", "c3_2", "c4_2", "c5_2", "c6_2")
chart.label = c("Type frequency (1,000,000)", 
                "Type frequency (900,000)", 
                "Type frequency (700,000)", 
                "Type frequency (500,000)", 
                "Type frequency (300,000)")
print(chart.label)

####5. Create the plots for frequency distributions####
# There will be 25 plots for bundles and 25 plots for frames
# Each plot contains distributions for short, mid and long-length texts
# For each sub-corpus length there are 5 ranges.
# There are 5 sub-corpus lengths in total.

sample.data.summaries = list()
all.plots = list()
k = 1
for(i in 1:length(subCorpusLength)){
  
  for(j in 1:length(rangeConditions)){
    sample.data = summarizeDistributions(core.data.path,
                                         sampling.conditions,
                                         purpose,
                                         subCorpusType,
                                         subCorpusLength[i],
                                         comparison,
                                         rangeConditions[j])
    sample.data = rbindlist(sample.data)
    sample.data.summary = sample.data %>% group_by(type) %>% summarize(mean = mean(nrows),
                                                                       max = max(nrows),
                                                                       min = min(nrows),
                                                                       range = max(nrows) - min(nrows))
    final.chart.label = paste(chart.label[i], rangeConditions[j], sep = " ")
    this.plot <- sample.data %>% ggplot(aes(x=nrows, color=type)) + 
      geom_histogram(binwidth=1, alpha=0.5, fill="white", position="dodge") +
      labs(x = final.chart.label, y = "Number of samples") +
      geom_vline(data=sample.data.summary, aes(xintercept=mean, color=type), linetype="dashed") #+
    #theme(legend.position="top")
    
    sample.data.summaries[[k]] = sample.data.summary
    all.plots[[k]] = this.plot
    k = k + 1
    
  }
  
}



fig2 <- ggarrange(all.plots[[1]], all.plots[[2]], all.plots[[3]], all.plots[[4]], all.plots[[5]],
                  all.plots[[6]], all.plots[[7]], all.plots[[8]], all.plots[[9]], all.plots[[10]],
                  all.plots[[11]], all.plots[[12]], all.plots[[13]], all.plots[[14]], all.plots[[15]],
                  all.plots[[16]], all.plots[[17]], all.plots[[18]], all.plots[[19]], all.plots[[20]],
                  all.plots[[21]], all.plots[[22]], all.plots[[23]], all.plots[[24]], all.plots[[25]],
                  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                             "Q", "R", "S", "T", "U", "V", "W", "X", "Y"),
                  ncol = 5, nrow = 5)
fig2 <- annotate_figure(fig2, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                              color = "red", face = "bold", size = 16))
fig2

####6. Comments on frames####
# It seems that the pattern is a little different with frames.
# There is convergence of average lines in both direction, left to right (constant corpus size, increasing frequency threshold)
# and top and bottom (constant threshold frequency, decreasing corpus size).
# This implies that frequency thresholds and corpus size are interacting in some systematic way with text size.
# Text size is allowed to have a large effect as the corpus grows and the threshold frequency decreases.
# As the threshold frequency increases and the corpus size decreases, the text size effect is suppressed.

# The distribution in the frequency is wider for long texts compared to short texts as the corpus size increases (bottom to top)
# and the same thing happens as the threshold frequency decreases (right to left).










