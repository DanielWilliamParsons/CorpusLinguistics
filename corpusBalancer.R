
#This function accesses the
corpusBalancer = function(core.path, balancer.data){
  
  input.path = paste(core.path, balancer.data$input[1], sep="")
  fileByFileData = data.table::fread(input.path)
  fileByFileData = fileByFileData %>% filter(subject != "other") #Remove "other" subjects from the corpus
  
  if(balancer.data$balance == "word"){
    
    # Copy the file by file metadata so that it can be used again.
    fileByFileDataAnalysis = fileByFileData
    
    ####Word-Balanced####
    # The files are selected such that there are approximately 300,000 words
    # per disciplinary group in BAWE
    
    ####LONG TEXT LENGTH CORPUS####
    #Create the long-text base corpus
    corpusTextsLONG = fileByFileDataAnalysis %>%
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>% #Calculate how many texts each author contributes
      arrange(numberOfTextsContributed) %>%
      slice_max(wordCount, n = balancer.data$maxFileNoLONG) %>% #Take only the top n files that have the maximum word counts per file for each author
      filter(wordCount >= balancer.data$minWCLONG) %>% #Make sure that these maximum-word-count files are above the min word count of 2,800 words
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, -wordCount) %>% #For each discipline, arrange the resulting files according to the number of texts contributed and then according to word count in descending order (highest word count first for least number of contributions in the discipline)
      mutate("cumSumBackwards"=cumsum(wordCount)) %>% #Calculate a cumulative word count sum for each discipline based on the above ordering of files.
      group_by(discipline) %>% filter(cumSumBackwards <= balancer.data$maxSubCorpWC) %>% ungroup() #Select all authors until the cumulative sum is at or slightly less than the maximum value of the disciplinary group's allowed word count, e.g., 300000
    
    # Calculate the number of files and  word count per discipline in the long-text corpora
    corpusTextsLONGSummary = corpusTextsLONG %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% print(n=4) %>% ungroup() 
    
    # Summarize the authors contribution to the long-text corpus
    corpusAUTHORSummaryLONG = corpusTextsLONG %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryLONG = corpusAUTHORSummaryLONG %>% #Summarize how many authors have contributed how many files to the corpus, e.g. how many authors contributed 2 files, how many contributed 1 file
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    # Remove the selected long texts from the main file by file metadata
    long.text.files = corpusTextsLONG$file
    fileByFileDataAnalysis = fileByFileDataAnalysis %>% filter(!file %in% long.text.files)
    
    ####SHORT TEXT LENGTH CORPUS####
    # Extract the short text files
    corpusTextsSHORT = fileByFileDataAnalysis %>%
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>%
      arrange(numberOfTextsContributed) %>%
      slice_min(wordCount, n = balancer.data$maxFileNoSHORT) %>% #Since the files are shorter by definition, it is best to allow many contributions from individual authors due to the limitations of the corpus design
      filter(wordCount < balancer.data$maxWCSHORT) %>%
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, wordCount) %>%
      mutate("cumSumUpwards"=cumsum(wordCount)) %>%
      group_by(discipline) %>% filter(cumSumUpwards <= balancer.data$maxSubCorpWC) %>% ungroup()
    
    # Calculate the number of files and  word count per discipline in the short-text corpora
    corpusTextsSHORTSummary = corpusTextsSHORT %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>%  ungroup()
    
    # Examine the author contributions to the short-text corpora
    corpusAUTHORSummarySHORT = corpusTextsSHORT %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummarySHORT = corpusAUTHORSummarySHORT %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    # Remove the selected texts
    short.text.files = corpusTextsSHORT$file
    fileByFileDataAnalysis = fileByFileDataAnalysis %>% filter(!file %in% short.text.files)
    
    ####MID TEXT LENGTH CORPUS####
    corpusTextsMID = fileByFileDataAnalysis %>% 
      group_by(author) %>%
      mutate("numberOfTextsContributed" = n()) %>%
      arrange(numberOfTextsContributed) %>%
      filter(wordCount >= balancer.data$minWCMID & wordCount < balancer.data$maxWCMID) %>% #Due to the small number of PS files, no limit was made on the number of files that authors can contribute; however under the optimal conditions found, the max number of contributed texts is 5 by only 2 authors with the majority contributing just 1 or 2 texts
      ungroup() %>%
      group_by(discipline) %>% arrange(numberOfTextsContributed, wordCount) %>%
      mutate("cumSumUpwards"=cumsum(wordCount)) %>%
      group_by(discipline) %>% filter(cumSumUpwards <= balancer.data$maxSubCorpWC) %>% ungroup()
    
    # Calculate the number of files and  word count per discipline in the mid-text-length corpora
    corpusTextsMIDSummary = corpusTextsMID %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    
    # Examine the author contributions to the mid-text-length corpora
    corpusAUTHORSummaryMID = corpusTextsMID %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryMID = corpusAUTHORSummaryMID %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    ####Tidy up####
    corpusTextsLONG = corpusTextsLONG %>% select(-numberOfTextsContributed, -cumSumBackwards)
    corpusTextsSHORT = corpusTextsSHORT %>% select(-numberOfTextsContributed, -cumSumUpwards)
    corpusTextsMID = corpusTextsMID %>% select(-numberOfTextsContributed, -cumSumUpwards)
    
    ####Save all the data####
    data.table::fwrite(corpusTextsLONG, paste(core.path, balancer.data$corpusDataPathLONG, sep =""))
    data.table::fwrite(corpusTextsMID, paste(core.path, balancer.data$corpusDataPathMID, sep =""))
    data.table::fwrite(corpusTextsSHORT, paste(core.path, balancer.data$corpusDataPathSHORT, sep =""))
    
    data.table::fwrite(corpusTextsLONGSummary, paste(core.path, balancer.data$longSUMMARY, sep =""))
    data.table::fwrite(contributedTextsSummaryLONG, paste(core.path, balancer.data$longAUTHSummary, sep =""))
    
    data.table::fwrite(corpusTextsMIDSummary, paste(core.path, balancer.data$midSUMMARY, sep =""))
    data.table::fwrite(contributedTextsSummaryMID, paste(core.path, balancer.data$midAUTHSummary, sep =""))
    
    data.table::fwrite(corpusTextsSHORTSummary, paste(core.path, balancer.data$shortSUMMARY, sep =""))
    data.table::fwrite(contributedTextsSummarySHORT, paste(core.path, balancer.data$shortAUTHSummary, sep ="")) 
    
  } else {
    
    ####Proportionally-Balanced####
    # The files are selectes such that the proportion of files per discipline
    # is equivalent to the proportion of files per discipline in the original
    # BAWE corpus
    
    # Copy the file by file metadata so that it can be used again.
    fileByFileDataAnalysis = fileByFileData
    
    # Step 1: Understand the proportions first
    summary = fileByFileData %>% group_by(discipline) %>%
      summarize("numFiles" = n(), "totalWords" = sum(wordCount)) %>%
      mutate("proportionOfFiles" = numFiles/(sum(numFiles))) %>%
      mutate("proportionWordCount" = totalWords/(sum(totalWords)))
    print(summary)
    
    #### Long-length texts####
    corpusTextsLONG = fileByFileData %>% group_by(discipline) %>%
      slice_max(wordCount, prop=0.20) %>% ungroup()
    
    # Summary of the long texts
    corpusTextsLONGSummary = corpusTextsLONG %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>% 
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>% 
      ungroup()
    
    # Summary of the author contributions
    corpusAUTHORSummaryLONG = corpusTextsLONG %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryLONG = corpusAUTHORSummaryLONG %>% #Summarize how many authors have contributed how many files to the corpus, e.g. how many authors contributed 2 files, how many contributed 1 file
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    # Remove the selected long texts from the main file by file metadata
    long.text.files = corpusTextsLONG$file
    fileByFileDataAnalysis = fileByFileDataAnalysis %>% filter(!file %in% long.text.files)
    
    ####Short-length texts####
    # Create the short-length texts sub-corpus, approximately 50% of the minimum word counts from each discipline
    corpusTextsSHORT = fileByFileDataAnalysis %>% group_by(discipline) %>% 
      slice_min(wordCount, prop=0.50) %>% ungroup()
    
    # Summary of the proportion of files per discipline
    corpusTextsSHORTSummary = corpusTextsSHORT %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>%  
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>% ungroup()
    
    # Summary of the author contributions
    corpusAUTHORSummarySHORT = corpusTextsSHORT %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummarySHORT = corpusAUTHORSummarySHORT %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    ####Mid-length texts####
    # Remove the selected texts
    # This filtering process should leave only mid-length texts which can then be summarized
    short.text.files = corpusTextsSHORT$file
    corpusTextsMID = fileByFileDataAnalysis %>% filter(!file %in% short.text.files)
    
    # Summarize the proportion of texts in each discipline
    corpusTextsMIDSummary = corpusTextsMID %>% ungroup() %>%
      group_by(discipline) %>% summarize(numberOfFiles=n(), totalWordCount=sum(wordCount)) %>%  
      mutate("proportionOfFiles"=numberOfFiles/(sum(numberOfFiles))) %>% ungroup()
    
    # Summarize author contributions
    corpusAUTHORSummaryMID = corpusTextsMID %>% group_by(author) %>% 
      summarize(numberOfContributedTexts=n(), totalWordCount=sum(wordCount)) %>% ungroup()
    contributedTextsSummaryMID = corpusAUTHORSummaryMID %>% 
      group_by(numberOfContributedTexts) %>% 
      summarize(numberOfAuthors=n()) %>% ungroup()
    
    ####Save all the data####
    data.table::fwrite(corpusTextsLONG, paste(core.path, balancer.data$corpusDataPathLONG, sep=""))
    data.table::fwrite(corpusTextsMID, paste(core.path, balancer.data$corpusDataPathMID, sep=""))
    data.table::fwrite(corpusTextsSHORT, paste(core.path, balancer.data$corpusDataPathSHORT, sep=""))
    
    data.table::fwrite(corpusTextsLONGSummary, paste(core.path, balancer.data$longSUMMARY, sep=""))
    data.table::fwrite(contributedTextsSummaryLONG, paste(core.path, balancer.data$longAUTHSummary, sep=""))
    
    data.table::fwrite(corpusTextsMIDSummary, paste(core.path, balancer.data$midSUMMARY, sep=""))
    data.table::fwrite(contributedTextsSummaryMID, paste(core.path, balancer.data$midAUTHSummary, sep=""))
    
    data.table::fwrite(corpusTextsSHORTSummary, paste(core.path, balancer.data$shortSUMMARY, sep=""))
    data.table::fwrite(contributedTextsSummarySHORT, paste(core.path, balancer.data$shortAUTHSummary, sep=""))
    
  }
  
  
  
  
}