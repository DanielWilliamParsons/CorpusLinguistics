# The following two functions take the samples from each corpus in two ways
# 1. Keeping the word count balanced between disciplines
# 2. Keeping the proportion of files in each discipline the same

# The seed variable in both function ensures that the samples from the same baseCorpora will be
# different between frames and bundles since the same base corpora is being used

# Three main variables are output after running this function
# samplingFrame: a data frame containing all the file names sampled
# disciplineSummaryFrame: a data frame containing summaries of the disciplines in each sample
# raTracker: a data frame which tracks every sample and records whether it was accepted or rejected,
#            and also records the reason for acceptance or rejection.
#            The codes for accept/reject and reasons are written above the definition of the
#            variable

takeWordBalancedSamples <- function(core.path, conds, seed){
  ####PREPARE INPUT VARIABLES####
  # Retrieve the sample size and base corpus from which to sample
  sampleSize = 200#conds$sampleSize
  baseCorpus.path = paste(core.path, "baseCorporaMetadata/", conds$baseCorpora, sep="")
  baseCorpus = data.table::fread(baseCorpus.path)
  
  # Prepare the minimum and maximum proportions of each base corpus to sample from
  minSampleProp = conds$realPropMin
  maxSampleProp = conds$realPropMax
  
  # Prepare the minimum and maximum total word counts for each discipline in the sampled base corpus
  minWordCount.perDiscipline = conds$minSampleSize / 4 #Divide by 4 because this will be the sample size for those 4 disciplinary groups
  maxWordCount.perDiscipline = conds$maxSampleSize / 4
  minWordCount = conds$minSampleSize
  maxWordCount = conds$maxSampleSize
  
  ####PREPARE OUTPUT VARIABLES####
  # This is the sampling frame which stores ALL the samples.
  samplingFrame = list()
  
  # This is the frame in which a summary of each discipline sampled is added
  disciplineSummaryFrame = list()
  
  # Duplicate checker. This checks that the files in one sample are not the same combination as in another sample
  duplicateChecker = list()
  
  # The raTracker: r = reject; a = accept.
  # This tracker keeps track at whether a sample is rejected or accepted an provides the reason.
  # ra = 1 means accept, ra = 0 means reject
  # reason = 1 means not duplicated and within word count rangess (i.e., accepted)
  # reason = 2 means total word count is out of range
  # reason = 3 means disciplinary word count is out of range
  # reason = 4 means a sample was duplicated
  raTracker = list()
  
  ####PREPARE THE SAMPLING PROCEDURE####
  set.seed(seed)
  k = 1 # For accepted samples, up to sampleSize
  l = 1 # For tracking both accepted and rejected samples
  while(length(samplingFrame) < sampleSize) {
    if(k %% 100 == 0){
      print(k)
    }
    # Randomly select the sample proportion
    sampleProp <- sample(minSampleProp:maxSampleProp, 1)
    sampleProp = sampleProp/100
    
    # Sample the corpus from each disciplinary group without replacement
    this.sample = baseCorpus %>% group_by(discipline) %>% slice_sample(prop = sampleProp, replace=FALSE) %>% ungroup()
    
    # Get the total word count of the sample
    total.word.count = sum(this.sample$wordCount)
    
    if(total.word.count > minWordCount & total.word.count < maxWordCount) {
      
      discipline.summary = this.sample %>% group_by(discipline) %>% 
        summarize(num.files.per.discipline = n(), total.discipline.word.count = sum(wordCount)) %>% ungroup()
      
      AH.word.count = discipline.summary$total.discipline.word.count[discipline.summary$discipline == "AH"]
      LS.word.count = discipline.summary$total.discipline.word.count[discipline.summary$discipline == "LS"]
      PS.word.count = discipline.summary$total.discipline.word.count[discipline.summary$discipline == "PS"]
      SS.word.count = discipline.summary$total.discipline.word.count[discipline.summary$discipline == "SS"]
      
      
      # If the following conditions are met, then add the sample to the samplingFrame
      allowedDisciplinaryMin = (conds$minSampleSize/4)-conds$allowedDisciplineVariation
      allowedDisciplinaryMax = (conds$maxSampleSize/4)+conds$allowedDisciplineVariation
      
      
      if((AH.word.count > allowedDisciplinaryMin & AH.word.count < allowedDisciplinaryMax) &
         (LS.word.count > allowedDisciplinaryMin & LS.word.count < allowedDisciplinaryMax) &
         (PS.word.count > allowedDisciplinaryMin & PS.word.count < allowedDisciplinaryMax) &
         (SS.word.count > allowedDisciplinaryMin & SS.word.count < allowedDisciplinaryMax)) {
        
        #Now check for duplicates
        if(length(duplicateChecker) >= 1){
          # This is the case when k >= 1: after the first sample
          
          # Get the files in the current sample
          files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
          
          # Call the function that loops through all the files in the duplicate checker list
          # The checks vector contains TRUE or FALSE values, TRUE meaning that the current vector of files
          # is the same as a previous sample, and should therefore be discarded.
          checks = checkForDuplicates(duplicateChecker, files)
          
          if(TRUE %in% checks){
            
            tracker = data.table(ra = 0, reason = 4)
            raTracker[[l]] = tracker
            
            l = l + 1
            
          } else{
            # Record the sample
            print("It's not true")
            this.sample$sampleNumber = k
            this.sample$totalWordCount = total.word.count
            discipline.summary$sampleNumber = k
            
            # Add just the file names to the duplicate checker
            files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
            duplicateChecker[[k]] = files
            
            samplingFrame[[k]] = this.sample
            disciplineSummaryFrame[[k]] = discipline.summary
            
            tracker = data.table(ra = 1, reason = 1)
            raTracker[[l]] = tracker
            
            k = k + 1
            l = l + 1
          }
          
        } else {
          # Record the first sample
          # This is the case when k = 1: the first sample, so this piece of code is used just once at the start
          this.sample$sampleNumber = 1
          this.sample$totalWordCount = total.word.count
          discipline.summary$sampleNumber = 1
          
          # Add just the file names to the duplicate checker
          files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
          duplicateChecker[[1]] = files
          
          samplingFrame[[1]] = this.sample
          disciplineSummaryFrame[[1]] = discipline.summary
          
          tracker = data.table(ra = 1, reason = 1)
          raTracker[[l]] = tracker
          
          l = l + 1
          k = 2
        }
        
      } else {
        
        tracker = data.table(ra = 0, reason = 3)
        raTracker[[l]] = tracker
        
        l = l + 1
      }
      
    } else {
      
      tracker = data.table(ra = 0, reason = 2)
      raTracker[[l]] = tracker
      
      l = l + 1
      
    } #END OF CONDITION CHECKS
  } #END OF SAMPLING PROCEDURE IN THE WHILE LOOP
  
  ####PREPARE TO READ TO FILE####
  
  samplingFrame = rbindlist(samplingFrame)
  disciplineSummaryFrame = rbindlist(disciplineSummaryFrame)
  raTracker = rbindlist(raTracker)
  
  samplingFrame.path = paste(core.path, "SamplesPilot/", conds$subCorpusLength, "_", conds$saveSamplingFrame, sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  disciplineSummaryFrame.path = paste(core.path, "SamplesSummariesPilot/", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".csv", sep ="")
  data.table::fwrite(disciplineSummaryFrame, disciplineSummaryFrame.path)
  
  raTracker.path = paste(core.path, "TrackingPilot/", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".csv", sep ="")
  data.table::fwrite(raTracker, raTracker.path)
  
  sessionInfoPath = paste(core.path, "SessionInfo/06_takeSamples_", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}

takePropBalancedSamples = function(core.path, conds, seed) {
  
  ####PREPARE INPUT VARIABLES####
  # Retrieve the sample size and base corpus from which to sample
  sampleSize = 200#conds$sampleSize
  baseCorpus.path = paste(core.path, "baseCorporaMetadata/", conds$baseCorpora, sep="")
  baseCorpus = data.table::fread(baseCorpus.path)
  
  # Prepare the minimum and maximum proportions of each base corpus to sample from
  minSampleProp = conds$realPropMin
  maxSampleProp = conds$realPropMax
  
  # Prepare the minimum and maximum total word counts for each discipline in the sampled base corpus
  minWordCount = conds$minSampleSize
  maxWordCount = conds$maxSampleSize
  
  ####PREPARE OUTPUT VARIABLES####
  # This is the sampling frame which stores ALL the samples.
  samplingFrame = list()
  
  # This is the frame in which a summary of each discipline sampled is added
  disciplineSummaryFrame = list()
  
  # Duplicate checker. This checks that the files in one sample are not the same combination as in another sample
  duplicateChecker = list()
  
  # The raTracker: r = reject; a = accept.
  # This tracker keeps track at whether a sample is rejected or accepted an provides the reason.
  # ra = 1 means accept, ra = 0 means reject
  # reason = 1 means not duplicated and within word count rangess (i.e., accepted)
  # reason = 2 means total word count is out of range
  # reason = 3 means disciplinary word count is out of range
  # reason = 4 means a sample was duplicated
  raTracker = list()
  
  ####PREPARE THE SAMPLING PROCEDURE####
  set.seed(seed)
  k = 1 # For accepted samples, up to sampleSize
  l = 1 # For tracking both accepted and rejected samples
  while(length(samplingFrame) < sampleSize) {
    if(k %% 100 == 0){
      print(k)
    }
    # Randomly select the sample proportion
    sampleProp <- sample(minSampleProp:maxSampleProp, 1)
    sampleProp = sampleProp/100
    
    # Sample the corpus from each disciplinary group without replacement
    this.sample = baseCorpus %>% group_by(discipline) %>% slice_sample(prop = sampleProp, replace=FALSE) %>% ungroup()
    
    # Get the total word count of the sample
    total.word.count = sum(this.sample$wordCount)
    
    if(total.word.count > minWordCount & total.word.count < maxWordCount) {
      
      discipline.summary = this.sample %>% group_by(discipline) %>% 
        summarize(num.files.per.discipline = n(), total.discipline.word.count = sum(wordCount)) %>% ungroup()
      
      #Now check for duplicates
      if(length(duplicateChecker) >= 1){
        # This is the case when k >= 1: after the first sample
        
        # Get the files in the current sample
        files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
        
        # Call the function that loops through all the files in the duplicate checker list
        # The checks vector contains TRUE or FALSE values, TRUE meaning that the current vector of files
        # is the same as a previous sample, and should therefore be discarded.
        checks = checkForDuplicates(duplicateChecker, files)
        
        if(TRUE %in% checks){
          
          tracker = data.table(ra = 0, reason = 4)
          raTracker[[l]] = tracker
          
          l = l + 1
          
        } else{
          # Record the sample
          print("It's not true")
          this.sample$sampleNumber = k
          this.sample$totalWordCount = total.word.count
          discipline.summary$sampleNumber = k
          
          # Add just the file names to the duplicate checker
          files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
          duplicateChecker[[k]] = files
          
          samplingFrame[[k]] = this.sample
          disciplineSummaryFrame[[k]] = discipline.summary
          
          tracker = data.table(ra = 1, reason = 1)
          raTracker[[l]] = tracker
          
          k = k + 1
          l = l + 1
        }
        
      } else {
        # Record the first sample
        # This is the case when k = 1: the first sample, so this piece of code is used just once at the start
        this.sample$sampleNumber = 1
        this.sample$totalWordCount = total.word.count
        discipline.summary$sampleNumber = 1
        
        # Add just the file names to the duplicate checker
        files = this.sample %>% select(file) %>% arrange(file) %>% pull(file)
        duplicateChecker[[1]] = files
        
        samplingFrame[[1]] = this.sample
        disciplineSummaryFrame[[1]] = discipline.summary
        
        tracker = data.table(ra = 1, reason = 1)
        raTracker[[l]] = tracker
        
        l = l + 1
        k = 2
      }
    } #END OF CONDITION CHECKS
  } #END OF SAMPLING PROCEDURE IN THE WHILE LOOP
  
  ####PREPARE TO READ TO FILE####
  samplingFrame = rbindlist(samplingFrame)
  disciplineSummaryFrame = rbindlist(disciplineSummaryFrame)
  raTracker = rbindlist(raTracker)
  
  samplingFrame.path = paste(core.path, "SamplesPilot/", conds$subCorpusLength, "_", conds$saveSamplingFrame, sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  disciplineSummaryFrame.path = paste(core.path, "SamplesSummariesPilot/", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".csv", sep ="")
  data.table::fwrite(disciplineSummaryFrame, disciplineSummaryFrame.path)
  
  raTracker.path = paste(core.path, "TrackingPilot/", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".csv", sep ="")
  data.table::fwrite(raTracker, raTracker.path)
  
  sessionInfoPath = paste(core.path, "SessionInfo/06_takeSamples_", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}



# This function accepts the duplicateChecker list
# The duplicateChecker is a list of files from each sample.
# The files variable is the current sample of files.
# This function loops through the list of files in the duplicateChecker list
# and checks if the new sample is the same as any in the list
# It returns a vector of TRUE or FALSE statements, TRUE meaning duplicateChecker does
# contain a duplicate, FALSE meaning that there are no duplicates.
checkForDuplicates = function(duplicateChecker, files){
  
  # This function will loop through all the files sampled so far and make a comparison
  checks = lapply(duplicateChecker, function(x){
    if(all(x %in% files)) {
      TRUE
    } else {
      FALSE
    }
  })
  
  checks = unlist(checks)
  return(checks)
  
}








