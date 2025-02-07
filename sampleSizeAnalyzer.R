# The purpose of this function is to determine an ideal fraction of files to sample
# such that they extract a corpus of a specified size.

analyzeSampleSize = function(core.path){
  
  conds.path = paste(core.path, "baseCorporaMetadata/samplingConditions.csv", sep="")
  conds = data.table::fread(conds.path)
  # The file read in above contains the following information
  # balance: whether the corpus is balanced by word count per disciplinary group or by proportion of files in each disciplinary group
  # purpose: whether we are sampling bundles or phrase frames
  # subCorpusType: long-length texts (long), medium-length texts (mid), short-length texts (short)
  # subCorpusLength: from 1000000 to 100000 in decrements of 100000
  # baseCorporaToSample: the file name where the base corpora files are stored
  # saveSamplingFrame: the file where all the samples taken are saved
  # sampleSize: how many samples to take
  
  #In running this function, the following columns are used below
  # testPropMin: sets a minimum value for the proportion of files to sample
  #              this is divided by 100 to get the proportion
  # testPropMax: sets a maximum value for the proportion of files to sample
  # minSampleSize: when all samples are taken, this is the minimum allowed word count
  # maxSampleSize: when all samples are taken, this is the maximum allowed word count
  # Note that the difference between minSampleSize and maxSampleSize represents an
  # acceptable error in the size of the sub-corpus.
  # For ezxample, an error of 5000 words less and 5000 words more than the value set
  # should allow for the fact that document sizes differ and so combinations of them
  # cannot easily add up to precisely the value of the sub-corpus
  # However, note that this range was widened in ONE case: mid-length texts in the
  # sub-corpora of 100,000 words balanced by the proportion of files. This is because no suitable proportion
  # of each disciplinary group could be found that created corpora within +/- 5000 words range
  # the range for this is one was +/- 10,000 words
  
  # Updating the following columns is the GOAL of this function
  # realPropMin: this is the minimum proportion of files to sample that will likely sample between the minimum and maximum word counts
  # realPropMax: this is the maximum proportion of files to sample that will likely sample between the minimum and maximum word counts
  # The purpose of this function is to calculate realPropMin and realPropMax and update the samplingCondition.csv file
  # The realPropMin and realPropMax are then used later for the actual sampling.
  # The condition for realPropMin and realPropMax is that after 1000 samples are taken, of those samples whose total word counts are between
  # the minimum and maximum word counts, the proportions that occur the most frequently (5% or more) are kept and the minimum and maximum values
  # of these are chosen
  
  for(i in 1:nrow(conds)){
    
    toSample = paste(core.path, "baseCorporaMetadata/", conds$baseCorporaToSample[i], sep="")
    toSample = data.table::fread(toSample)
    print.to.screen = paste("Analyzing sub corpus size of ", conds$subCorpusLength[i], "for a sub-corpus type of ", conds$subCorpusType[i])
    print(print.to.screen)
    sample.size = list()
    
    # Focus on sampling the corpus balanced by the word count per discipline
    if(conds$balance[i] == "word"){
      
      for(j in 1:1000){
        
        propSize = sample(conds$testPropMin[i]:conds$testPropMax[i], 1)
        propSize = propSize/100
        sampledFiles = toSample %>% group_by(discipline) %>% 
          slice_sample(prop = propSize, replace=FALSE) %>% 
          ungroup() %>%
          group_by(discipline) %>%
          summarize(word.count.per.discipline = sum(wordCount)) %>%
          ungroup() %>%
          mutate(total.word.count = sum(word.count.per.discipline),
                 propSize = propSize)
        
        # Because we are sampling based on number of words per discipline, it's important that the word count
        # in each disciplinary group does not vary too much.
        # On the other hand, there is a need to balance sample sizes.
        # Experimentation led to the decision about the size of "allowedDisciplineVariation" - see the column in the samplingConditions.csv file
        if(sampledFiles$total.word.count[1] < conds$maxSampleSize[i] & sampledFiles$total.word.count[1] > conds$minSampleSize[i]) {
          
          AH.word.count = sampledFiles$word.count.per.discipline[sampledFiles$discipline == "AH"]
          LS.word.count = sampledFiles$word.count.per.discipline[sampledFiles$discipline == "LS"]
          PS.word.count = sampledFiles$word.count.per.discipline[sampledFiles$discipline == "PS"]
          SS.word.count = sampledFiles$word.count.per.discipline[sampledFiles$discipline == "SS"]
          
          allowedDisciplinaryMin = (conds$minSampleSize[i]/4)-conds$allowedDisciplineVariation[i]
          allowedDisciplinaryMax = (conds$maxSampleSize[i]/4)+conds$allowedDisciplineVariation[i]
          
          
          if((AH.word.count > allowedDisciplinaryMin & AH.word.count < allowedDisciplinaryMax) &
             (LS.word.count > allowedDisciplinaryMin & LS.word.count < allowedDisciplinaryMax) &
             (PS.word.count > allowedDisciplinaryMin & PS.word.count < allowedDisciplinaryMax) &
             (SS.word.count > allowedDisciplinaryMin & SS.word.count < allowedDisciplinaryMax)) {
            
            this.sample.size = data.table(sampleSize = sampledFiles$total.word.count[1], propSize = propSize)
            sample.size[[j]] = this.sample.size
            
          }
        }
      }
      sample.size = rbindlist(sample.size)
      sample.size = sample.size %>% arrange(propSize)
      print(sample.size)
      
      minPropSize = sample.size$propSize[1] * 100
      maxPropSize = sample.size$propSize[nrow(sample.size)] * 100
      if(minPropSize == maxPropSize){
        maxPropSize = minPropSize + 1
      }
      conds$realPropMin[i] = minPropSize
      conds$realPropMax[i] = maxPropSize
      data.table::fwrite(conds, conds.path)
      
    } else {
      # Now focus on the sub-corpora which are balanced by the proportion of files per discipline
      
      for(j in 1:1000){
        propSize = sample(conds$testPropMin[i]:conds$testPropMax[i], 1)
        propSize = propSize/100
        sampledFiles = toSample %>% group_by(discipline) %>% 
          slice_sample(prop = propSize, replace=FALSE) %>% 
          ungroup()
        word.count = sum(sampledFiles$wordCount)
        this.sample.size = data.table(sampleSize=word.count, propSize=propSize)
        sample.size[[j]] = this.sample.size
      }
      sample.size = rbindlist(sample.size)
      
      sample.size = sample.size %>% filter(sampleSize < conds$maxSampleSize[i] & sampleSize > conds$minSampleSize[i]) %>% 
        group_by(propSize) %>% summarize(freq = n()) %>% mutate(freq.prop = freq/(sum(freq))) %>%
        filter(freq.prop >= 0.05)
      
      print(sample.size)
      minPropSize = sample.size$propSize[1] * 100
      maxPropSize = sample.size$propSize[nrow(sample.size)] * 100
      if(minPropSize == maxPropSize){
        maxPropSize = minPropSize + 1
      }
      conds$realPropMin[i] = minPropSize
      conds$realPropMax[i] = maxPropSize
      data.table::fwrite(conds, conds.path)
    }
    
    
  }
  
  
  sessionInfoPath = paste(core.path, "SessionInfo/05_sampleSizeAnalyzer.txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}