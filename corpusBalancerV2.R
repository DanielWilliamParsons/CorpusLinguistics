#Corpus re-balancing. This program allows the selection of specific files in building a corpus
#Input: fileByFileData_Zipfian.csv
#Output: a reduced file with the same structure as fileByFileData_Zipfian.csv

library(R6)
library(stringr)
library(car)
library(tidyverse)
library(data.table)
library(stopwords)
library(textstem)

corpusBalancer = R6Class("corpusBalancer", list(
  
  cd = "", #corpus data
  initialize = function(input){
    self$cd = data.table::fread(input, stringsAsFactors=FALSE)
    self$cd = self$cd %>% filter(subject!="other")
  },
  
  #This function makes three subcorpora that maintain a word count balance of approximately 300,000yen per discipline
  wordBalancedBAWECorpora = function(minWCLONG, #Minimum word count in each file required to be part of the long-text sub-corpus (>2800 seems optimal)
                                     minWCMID, #Minimum word count in each file required to be part of the mid-length text sub-corpus (>1800 seems optimal)
                                     maxWCMID, #Maximum word count in each file required to be part of the mid-length text sub-corpus (<2800 seems optimal)
                                     maxWCSHORT, #Maximum word count in each file required to be part of the short-length text sub-corpus (<1800 seems optimal)
                                     maxFileNoLONG, #Maximum number of files contributed by any one author within the long-text sub-corpus
                                     maxFileNoSHORT, #Maximum number of files contributed by any one author within the short-text sub-corpus
                                     maxSubCorpWC, #Maximum number of words that all files should total up to in each disciplinary group (300000 seems about right)
                                     corpusDataPathLONG, #Path to csv file that stores the data for the long-length text sub-corpora
                                     corpusDataPathMID, #Path to csv file that stores the data for the mid-length text sub-corpora
                                     corpusDataPathSHORT, #Path to csv file that stores the data for the short-length text sub-corpora
                                     shortSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the short-length text sub-corpora
                                     midSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the mid-length text sub-corpus
                                     longSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the long-length text sub-corpus
                                     shortAUTHSummary, #Path to csv file that stores data summarizing how many authors contributed how many files in the short-length text sub-corpus
                                     midAUTHSummary, #Path to csv file that stores data summarizing how many authors contributed how many files in the mid-length text sub-corpus
                                     longAUTHSummary){ #Path to csv file that stores data summarizing how many authors contributed how many files in the long-length text sub-corpus
    
    
    corpusTextsLONG = self$cd %>%
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>% #Calculate how many texts each author contributes
      arrange(numberOfTextsContributed) %>% #Probably unnecessary to do this as it is done later
      slice_max(wordCount, n = maxFileNoLONG) %>% #Take only a the top n files that have the maximum word counts per file for each author
      filter(wordCount >= minWCLONG) %>% #Make sure that these maximum-word-count files are above a minimum count, e.g. 2,800 words
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, -wordCount) %>% #For each discipline, arrange the resulting files according to the number of texts contributed and then according to word count in descending order (highest word count first for least number of contributions in the discipline)
      mutate("cumSumBackwards"=cumsum(wordCount)) %>% #Calculate a cumulative word count sum for each discipline based on the above ordering of files.
      group_by(discipline) %>% filter(cumSumBackwards <= maxSubCorpWC) %>% ungroup() #Select all authors until the cumulative sum is at or slightly less than the maximum value of the disciplinary group's allowed word count, e.g., 300000
    
    corpusTextsLONGSummary = corpusTextsLONG %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% print(n=4) %>% ungroup() #Calculate the number of files and  word count per discipline given the above arrangement
    corpusAUTHORSummaryLONG = corpusTextsLONG %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryLONG = corpusAUTHORSummaryLONG %>% #Summarize how many authors have contributed how many files to the corpus, e.g. how many authors contributed 2 files, how many contributed 1 file
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    corpusTextsLONG$contain = "Y"
    checkLONG = corpusTextsLONG %>% select(fn, contain)
    
    corpusTextsSHORT = self$cd %>% left_join(checkLONG, by=c("fn"="fn")) %>% filter(is.na(contain)) %>% select(-contain)
    corpusTextsLONG = corpusTextsLONG %>% select(-contain) #Remove all files from the original corpus which have been selected for the long-length texts subcorpus
    
    corpusTextsSHORT = corpusTextsSHORT %>% #Perform a similar set of operations on the reduced original corpus, but this time focusing on short files.
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>%
      arrange(numberOfTextsContributed) %>%
      slice_min(wordCount, n = maxFileNoSHORT) %>% #Since the files are shorter by definition, it is best to allow many contributions from individual authors due to the limitations of the corpus design
      filter(wordCount < maxWCSHORT) %>%
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, wordCount) %>%
      mutate("cumSumUpwards"=cumsum(wordCount)) %>%
      group_by(discipline) %>% filter(cumSumUpwards <= maxSubCorpWC) %>% ungroup()
    
    corpusTextsSHORTSummary = corpusTextsSHORT %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% print(n=4) %>% ungroup()
    corpusAUTHORSummarySHORT = corpusTextsSHORT %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummarySHORT = corpusAUTHORSummarySHORT %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    corpusTextsSHORT$contain = "Y"
    checkSHORT = corpusTextsSHORT %>% select(fn, contain)
    
    corpusTextsLONG$contain = "Y"
    checkLONG = corpusTextsLONG %>% select(fn, contain)
    
    corpusTextMID = self$cd %>% left_join(checkLONG, by=c("fn"="fn")) %>% filter(is.na(contain)) %>% select(-contain)
    corpusTextMID = corpusTextMID %>% left_join(checkSHORT) %>% filter(is.na(contain)) %>% select(-contain) #Remove from the original corpus both the long-length and short-length texts selected above. Mid-range texts will be left
    
    corpusTextsMID = corpusTextMID %>% #Perform a similar operation to extract a balanced number of texts per disciplinary group for the mid-length texts sub-corpus
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>%
      arrange(numberOfTextsContributed) %>%
      filter(wordCount >= minWCMID & wordCount < maxWCMID) %>% #Due to the small number of PS files, no limit was made on the number of files that authors can contribute; however under the optimal conditions found, the max number of contributed texts is 5 by only 2 authors with the majority contributing just 1 or 2 texts
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, wordCount) %>%
      mutate("cumSumUpwards"=cumsum(wordCount)) %>%
      group_by(discipline) %>% filter(cumSumUpwards <= maxSubCorpWC) %>% ungroup()
    
    corpusTextsMIDSummary = corpusTextsMID %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% print(n=4) %>% ungroup()
    corpusAUTHORSummaryMID = corpusTextsMID %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryMID = corpusAUTHORSummaryMID %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    corpusTextsLONG = corpusTextsLONG %>% select(-contain, -numberOfTextsContributed, -cumSumBackwards)
    corpusTextsSHORT = corpusTextsSHORT %>% select(-contain,-numberOfTextsContributed, -cumSumUpwards)
    corpusTextsMID = corpusTextsMID %>% select(-numberOfTextsContributed, -cumSumUpwards)
    
    data.table::fwrite(corpusTextsLONG, corpusDataPathLONG)
    data.table::fwrite(corpusTextsMID, corpusDataPathMID)
    data.table::fwrite(corpusTextsSHORT, corpusDataPathSHORT)
    
    data.table::fwrite(corpusTextsLONGSummary, longSUMMARY)
    data.table::fwrite(contributedTextsSummaryLONG, longAUTHSummary)
    
    data.table::fwrite(corpusTextsMIDSummary, midSUMMARY)
    data.table::fwrite(contributedTextsSummaryMID, midAUTHSummary)
    
    data.table::fwrite(corpusTextsSHORTSummary, shortSUMMARY)
    data.table::fwrite(contributedTextsSummarySHORT, shortAUTHSummary)
  },
  
  #This functions makes three sub-corpora that are balanced with respect to the number of files per discipline in the original corpus
  originalBalanceSelection = function(corpusDataPathLONG, #Path to csv file that stores the data for the long-length text sub-corpora
                                      corpusDataPathMID, #Path to csv file that stores the data for the mid-length text sub-corpora
                                      corpusDataPathSHORT, #Path to csv file that stores the data for the short-length text sub-corpora
                                      shortSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the short-length text sub-corpora
                                      midSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the mid-length text sub-corpus
                                      longSUMMARY, #Path to csv file that stores data summarizing the number of files and total word count in the long-length text sub-corpus
                                      shortAUTHSummary, #Path to csv file that stores data summarizing how many authors contributed how many files in the short-length text sub-corpus
                                      midAUTHSummary, #Path to csv file that stores data summarizing how many authors contributed how many files in the mid-length text sub-corpus
                                      longAUTHSummary){
    original = self$cd
    
    summary = self$cd %>% group_by(discipline) %>%
      summarize("numFiles"=n(), "totalWords"=sum(wordCount)) %>%
      mutate("proportionOfFiles" = numFiles/(sum(numFiles))) %>%
      mutate("proportionWordCount" = totalWords/(sum(totalWords))) %>%
      print(n=5)
    
    #Create the long-length texts sub-corpus - approx 20% of the original corpus, all maximum word counts from each discipline
    corpusTextsLONG = original %>% group_by(discipline) %>% 
      slice_max(wordCount, prop=0.20) %>% ungroup
    
    corpusTextsLONGSummary = corpusTextsLONG %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% 
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>%
      print(n=4) %>% ungroup() #Calculate the number of files and  word count per discipline given the above arrangement
    corpusAUTHORSummaryLONG = corpusTextsLONG %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryLONG = corpusAUTHORSummaryLONG %>% #Summarize how many authors have contributed how many files to the corpus, e.g. how many authors contributed 2 files, how many contributed 1 file
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    #Create the short-length texts sub-corpus, approximately 50% of the minimum word counts from each discipline
    corpusTextsSHORT = original %>% group_by(discipline) %>% 
      slice_min(wordCount, prop=0.50) %>% ungroup
    
    corpusTextsSHORTSummary = corpusTextsSHORT %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>%  
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>%
      print(n=4) %>% ungroup()
    corpusAUTHORSummarySHORT = corpusTextsSHORT %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummarySHORT = corpusAUTHORSummarySHORT %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    #Filter out the above created two sub-corpora from the original corpus
    #The remainder is the mid-length texts sub-corpus.
    filesInCorpus = corpusTextsLONG %>% select(fn)
    filesInCorpus$hasText = "Y"
    original = original %>% left_join(filesInCorpus, by=c("fn"="fn")) %>%
      filter(is.na(hasText)) %>% select(-hasText)
    
    filesInCorpus = corpusTextsSHORT %>% select(fn)
    filesInCorpus$hasText = "Y"
    corpusTextsMID = original %>% left_join(filesInCorpus, by=c("fn"="fn")) %>%
      filter(is.na(hasText)) %>% select(-hasText)
    
    corpusTextsMIDSummary = corpusTextsMID %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>%  
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>%
      print(n=4) %>% ungroup()
    corpusAUTHORSummaryMID = corpusTextsMID %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryMID = corpusAUTHORSummaryMID %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup() %>%
      print(n=10)
    
    data.table::fwrite(corpusTextsLONG, corpusDataPathLONG)
    data.table::fwrite(corpusTextsMID, corpusDataPathMID)
    data.table::fwrite(corpusTextsSHORT, corpusDataPathSHORT)
    
    data.table::fwrite(corpusTextsLONGSummary, longSUMMARY)
    data.table::fwrite(contributedTextsSummaryLONG, longAUTHSummary)
    
    data.table::fwrite(corpusTextsMIDSummary, midSUMMARY)
    data.table::fwrite(contributedTextsSummaryMID, midAUTHSummary)
    
    data.table::fwrite(corpusTextsSHORTSummary, shortSUMMARY)
    data.table::fwrite(contributedTextsSummarySHORT, shortAUTHSummary)
    
  },
  
  makeTextBAWECorpusOLD = function(minFileWordCount, maxFileWordCount,
                                            minNumberOfTextsPerAuthor, minNumberOfTextsPerSubject,
                                            corpusDataPath,
                                            checkDataPath,
                                            compareWithFullCorpusPath){
    
    originalSummary = self$cd %>% group_by(subject) %>% summarize(numberOfTexts=n(), totalWords=sum(wordCount))
    originalSummary$propOrig = originalSummary$numberOfTexts / sum(originalSummary$numberOfTexts)
    
    
    textsIncluded = self$cd %>%
      filter(wordCount>minFileWordCount & wordCount<maxFileWordCount) %>%
      group_by(author) %>%
      slice_min(wordCount, n=minNumberOfTextsPerAuthor) %>%
      ungroup() %>%
      group_by(subject) %>%
      mutate("numberOfTexts" = n()) %>%
      filter(numberOfTexts >= minNumberOfTextsPerSubject) %>%
      select(-numberOfTexts)
    data.table::fwrite(textsIncluded, corpusDataPath)
    
    check = textsIncluded %>% group_by(subject) %>% summarize(numberOfTexts=n(), totalWords=sum(wordCount))
    check$prop = check$numberOfTexts / sum(check$numberOfTexts)
    print(sum(check$totalWords))
    data.table::fwrite(check, checkDataPath)
    
    compareWithOriginal = check %>% left_join(originalSummary, by=c("subject"="subject"))
    data.table::fwrite(compareWithOriginal, compareWithFullCorpusPath)
    
    #Fraction of rows between 0.65 and 0.7 should work here. THIS IS JUST PRACTICE!
    #sample = textsIncluded %>% group_by(subject) %>% sample_frac(0.14, replace=FALSE) #0.52-0.54 for one million words; #0.39-0.41 for 750,000 words; 0.26-0.28 for 500,000 words; 0.13-0.14 for 250,000 words.
    #check = sample %>% group_by(subject) %>% summarize(numberOfTexts=n(), totalWords=sum(wordCount))
    #sampleWC = sum(sample$wordCount)
    #print(sampleWC)
  }
  
))

df = data.table::fread("/Volumes/Corpora/BAWE/BAWE_metadata/corpusBalancerConditionsBalancedByFiles.csv", stringsAsFactors=FALSE)
#The conditions in the file above provide a set of paths to save summary data for the partitioned corpus
#The corpus is partitioned into three main parts, a long-length texts section, a mid-length texts section
#and a short-length texts section. The proportion of files between disciplines is kept approximately the same
#as the original corpus.
for(i in 1:nrow(df)){
  print(df$input[i])
  x<-corpusBalancer$new(df$input[i])
  x$originalBalanceSelection(df$corpusDataPathLONG[i],
                            df$corpusDataPathMID[i],
                            df$corpusDataPathSHORT[i],
                            df$shortSUMMARY[i],
                            df$midSUMMARY[i],
                            df$longSUMMARY[i],
                            df$shortAUTHSummary[i],
                            df$midAUTHSummary[i],
                            df$longAUTHSummary[i])
}

#df = data.table::fread("/Volumes/Corpora/BAWE/BAWE_metadata/corpusBalancerConditions.csv", stringsAsFactors=FALSE)
#The conditions in the file above provide a balanced corpus of approximately 300,000 words per disciplinary group
#for a total of 1.2 million words in three main sub-corpora: long-length texts (greater than 2800 words), 
#mid-length texts (1800 < length < 2800 words) and short-length texts (less than 1800 words)
#The number of files in the long-length texts sub-corpus is 282
#The number of files in the mid-length texts sub-corpus is 533
#The number of files in the short-length texts is 992
#for(i in 1:nrow(df)){
#  print(df$input[i])
#  x<-corpusBalancer$new(df$input[i])
#  x$wordBalancedBAWECorpora(df$minWCLONG[i],
#                            df$minWCMID[i],
#                            df$maxWCMID[i],
#                            df$maxWCSHORT[i],
#                            df$maxFileNoLONG[i],
#                            df$maxFileNoSHORT[i],
#                            df$maxSubCorpWC[i],
#                            df$corpusDataPathLONG[i],
#                            df$corpusDataPathMID[i],
#                            df$corpusDataPathSHORT[i],
#                            df$shortSUMMARY[i],
#                            df$midSUMMARY[i],
#                            df$longSUMMARY[i],
#                            df$shortAUTHSummary[i],
#                            df$midAUTHSummary[i],
#                            df$longAUTHSummary[i])
#}

