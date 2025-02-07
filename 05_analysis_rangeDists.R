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
comparison = c("range")
rangeConditions = c("c4_1", "c4_2", "c4_3", "c4_4", "c4_5", "c4_6", "c4_7", "c4_8", "c4_9", "c4_10")
chart.label = c("Type frequency (1,000,000)", 
                "Type frequency (900,000)", 
                "Type frequency (700,000)", 
                "Type frequency (500,000)", 
                "Type frequency (300,000)")
print(chart.label)

####2. Create the plots for type frequency distributions####
# There will be 50 plots for bundles and 50 plots for frames
# Each plot contains distributions for short, mid and long-length texts
# For each sub-corpus length there are 10 ranges.
# There are 5 sub-corpus lengths in total.
# For one-dimensional range condition (number of texts only) there will be 20 plots
# For two-dimensional range condition (number of texts AND number of disciplines) there will be 30 plots

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



fig3.1d.range <- ggarrange(all.plots[[1]], all.plots[[2]], all.plots[[3]], all.plots[[4]], all.plots[[11]],
                  all.plots[[12]], all.plots[[13]], all.plots[[14]], all.plots[[21]], all.plots[[22]],
                  all.plots[[23]], all.plots[[24]], all.plots[[31]], all.plots[[32]], all.plots[[33]],
                  all.plots[[34]], all.plots[[41]], all.plots[[42]], all.plots[[43]], all.plots[[44]],
                  labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                             "Q", "R", "S", "T"),
                  ncol = 4, nrow = 5)
fig3.1d.range <- annotate_figure(fig3.1d.range, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                              color = "red", face = "bold", size = 16))
fig3.1d.range

fig4.2d.range <- ggarrange(all.plots[[5]], all.plots[[6]], all.plots[[7]], all.plots[[8]], all.plots[[9]],
                           all.plots[[10]], all.plots[[15]], all.plots[[16]], all.plots[[17]], all.plots[[18]],
                           all.plots[[19]], all.plots[[20]], all.plots[[25]], all.plots[[26]], all.plots[[27]],
                           all.plots[[28]], all.plots[[29]], all.plots[[30]], all.plots[[35]], all.plots[[36]],
                           all.plots[[37]], all.plots[[38]], all.plots[[39]], all.plots[[40]], all.plots[[45]],
                           all.plots[[46]], all.plots[[47]], all.plots[[48]], all.plots[[49]], all.plots[[50]],
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                                      "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "A1", "B1", "C1", "D1"),
                           ncol = 6, nrow = 5)
fig4.2d.range <- annotate_figure(fig4.2d.range, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                                       color = "red", face = "bold", size = 16))
fig4.2d.range

####3. Comments on bundles ####


####4. Prepare data for frames ####
analysis.path = paste(core.source.path, "summarizeDists.R", sep = "")
source(analysis.path)

purpose = c("frames")
subCorpusType = c("long", "mid", "short")
subCorpusLength = c(1000000, 900000, 700000, 500000, 300000)
comparison = c("range")
rangeConditions = c("c4_1", "c4_2", "c4_3", "c4_4", "c4_5", "c4_6", "c4_7", "c4_8", "c4_9", "c4_10")
chart.label = c("Type frequency (1,000,000)", 
                "Type frequency (900,000)", 
                "Type frequency (700,000)", 
                "Type frequency (500,000)", 
                "Type frequency (300,000)")
print(chart.label)

####5. Create the plots for type frequency distributions####
# There will be 50 plots for bundles and 50 plots for frames
# Each plot contains distributions for short, mid and long-length texts
# For each sub-corpus length there are 10 ranges.
# There are 5 sub-corpus lengths in total.
# For one-dimensional range condition (number of texts only) there will be 20 plots
# For two-dimensional range condition (number of texts AND number of disciplines) there will be 30 plots

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



fig5.1d.range <- ggarrange(all.plots[[1]], all.plots[[2]], all.plots[[3]], all.plots[[4]], all.plots[[11]],
                           all.plots[[12]], all.plots[[13]], all.plots[[14]], all.plots[[21]], all.plots[[22]],
                           all.plots[[23]], all.plots[[24]], all.plots[[31]], all.plots[[32]], all.plots[[33]],
                           all.plots[[34]], all.plots[[41]], all.plots[[42]], all.plots[[43]], all.plots[[44]],
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                                      "Q", "R", "S", "T"),
                           ncol = 4, nrow = 5)
fig5.1d.range <- annotate_figure(fig5.1d.range, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                                                color = "red", face = "bold", size = 16))
fig5.1d.range

fig6.2d.range <- ggarrange(all.plots[[5]], all.plots[[6]], all.plots[[7]], all.plots[[8]], all.plots[[9]],
                           all.plots[[10]], all.plots[[15]], all.plots[[16]], all.plots[[17]], all.plots[[18]],
                           all.plots[[19]], all.plots[[20]], all.plots[[25]], all.plots[[26]], all.plots[[27]],
                           all.plots[[28]], all.plots[[29]], all.plots[[30]], all.plots[[35]], all.plots[[36]],
                           all.plots[[37]], all.plots[[38]], all.plots[[39]], all.plots[[40]], all.plots[[45]],
                           all.plots[[46]], all.plots[[47]], all.plots[[48]], all.plots[[49]], all.plots[[50]],
                           labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
                                      "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "A1", "B1", "C1", "D1"),
                           ncol = 6, nrow = 5)
fig6.2d.range <- annotate_figure(fig6.2d.range, top = text_grob("Effects of varying corpus size on bundle/frame extraction rates",
                                                                color = "red", face = "bold", size = 16))
fig6.2d.range


# The pattern for two-dimensional range criteria appears to be roughly the same at all range conditions and corpus sizes
# There are more lexical phrase frames extracted from corpora made up of short-length texts, than medium and long-length texts.
# This implies that text length may be a more important factor than range.

