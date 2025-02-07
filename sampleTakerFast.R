# This approach is much faster since it uses multiple sampling through the map function
# from purrr, which significantly speeds up the processing.

takeWordBalancedSamplesFast <- function(core.path, conds, seed){
  ####PREPARE INPUT VARIABLES####
  # Retrieve the sample size and base corpus from which to sample
  sampleSize = 200#conds$sampleSize
  baseCorpus.path = paste(core.path, "baseCorporaMetadata/", conds$baseCorpora, sep="")
  baseCorpus = data.table::fread(baseCorpus.path)
  baseCorpus = baseCorpus %>% select(discipline, file, wordCount)
  
  # Prepare the minimum and maximum proportions of each base corpus to sample from
  minSampleProp = conds$realPropMin
  maxSampleProp = conds$realPropMax
  
  # Prepare the minimum and maximum total word counts for each discipline in the sampled base corpus
  minWordCount.perDiscipline = (conds$minSampleSize / 4) - conds$allowedDisciplineVariation
  maxWordCount.perDiscipline = (conds$maxSampleSize / 4) + conds$allowedDisciplineVariation
  minWordCount = conds$minSampleSize
  maxWordCount = conds$maxSampleSize
  
  ####PREPARE OUTPUT VARIABLES####
  # This is the sampling frame which stores ALL the samples.
  samplingFrame = data.table(sample=c(), data = c(), total.word.count = c())
  samplingFrameRows = ""
  
  ####PREPARE THE SAMPLING PROCEDURE####
  set.seed(seed)
  
  if(nrow(samplingFrame) == 0){
    samplingFrameRows = 0
  } else {
    samplingFrameRows = nrow(samplingFrame)
  }
  
  j = 1
  while(samplingFrameRows < sampleSize) {
    k = j + 249
    baseCorpusList = list()
    for(i in j:k){
      baseCorpus$sample = i
      baseCorpusList[[i]] = baseCorpus
    }
    
    sampleProp <- sample(minSampleProp:maxSampleProp, 1)
    sampleProp = sampleProp/100
  
    baseCorpusList = rbindlist(baseCorpusList)
    
    #baseCorpusList = baseCorpusList %>% group_by(sample) %>% nest() %>% 
    #  mutate(data = map(data, ~ .x %>% 
    #                      dplyr::group_by(discipline) %>%
    #                      dplyr::slice_sample(prop=sampleProp, replace=FALSE) %>%
    #                      dplyr::mutate(disc.word.count = sum(wordCount)) %>%
    #                      dplyr::ungroup() )) %>%
    #  mutate(totalWordCount = map(data, ~ .x %>% 
    #                                dplyr::summarize(total.word.count = sum(wordCount)))) %>%
    #  unnest(cols = c(totalWordCount)) %>%
    #  filter(total.word.count %in% c(minWordCount:maxWordCount))
    
    baseCorpusList = baseCorpusList %>% group_by(sample) %>% nest() %>% 
      mutate(data = map(data, ~ .x %>% 
                          dplyr::group_by(discipline) %>%
                          dplyr::slice_sample(prop=sampleProp, replace=FALSE) %>%
                          dplyr::mutate(min.disc.word.count = sum(wordCount)) %>%
                          dplyr::ungroup() %>%
                          dplyr::mutate(max.disc.word.count = min.disc.word.count))) %>%
      mutate(min.disc.word.count = map(data, ~ .x %>% 
                                         dplyr::slice_min(min.disc.word.count, with_ties=FALSE) %>%
                                         dplyr::select(min.disc.word.count)),
             max.disc.word.count = map(data, ~ .x %>%
                                         dplyr::slice_max(max.disc.word.count, with_ties=FALSE) %>%
                                         dplyr::select(max.disc.word.count))) %>%
      unnest(cols = c(min.disc.word.count, max.disc.word.count)) %>%
      filter(min.disc.word.count %in% c(minWordCount.perDiscipline:maxWordCount.perDiscipline) &
               max.disc.word.count %in% c(minWordCount.perDiscipline:maxWordCount.perDiscipline))
    
    #if(nrow(baseCorpusList) > 0){
     # baseCorpusList = baseCorpusList 
    #}
      
    
    #print(baseCorpusList)
    baseCorpusList = baseCorpusList %>% select(sample, data)
    
    if(nrow(samplingFrame) == 0){
      samplingFrame = baseCorpusList
      samplingFrameRows = nrow(samplingFrame)
    } else {
      samplingFrame = rbind(samplingFrame, baseCorpusList)
      samplingFrameRows = nrow(samplingFrame)
    }
    
    j = j + 250
    
  } #End of sampling procedure
  
  
 
  
  ####PREPARE TO READ TO FILE####
  
  samplingFrame = samplingFrame %>% unnest(cols = c(data))
  
  samplingFrame.path = paste(core.path, "Samples/", conds$subCorpusLength, "_", conds$saveSamplingFrame, sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  sessionInfoPath = paste(core.path, "SessionInfo/07_takeSamples_", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}


takePropBalancedSamplesFast <- function(core.path, conds, seed){
  ####PREPARE INPUT VARIABLES####
  # Retrieve the sample size and base corpus from which to sample
  sampleSize = 200#conds$sampleSize
  baseCorpus.path = paste(core.path, "baseCorporaMetadata/", conds$baseCorpora, sep="")
  baseCorpus = data.table::fread(baseCorpus.path)
  baseCorpus = baseCorpus %>% select(discipline, file, wordCount)
  
  # Prepare the minimum and maximum proportions of each base corpus to sample from
  minSampleProp = conds$realPropMin
  maxSampleProp = conds$realPropMax
  
  # Prepare the minimum and maximum total word counts for each discipline in the sampled base corpus
  minWordCount = conds$minSampleSize
  maxWordCount = conds$maxSampleSize
  
  ####PREPARE OUTPUT VARIABLES####
  # This is the sampling frame which stores ALL the samples.
  samplingFrame = data.table(sample=c(), data = c(), total.word.count = c())
  samplingFrameRows = ""
  
  ####PREPARE THE SAMPLING PROCEDURE####
  set.seed(seed)
  
  if(nrow(samplingFrame) == 0){
    samplingFrameRows = 0
  } else {
    samplingFrameRows = nrow(samplingFrame)
  }
  
  j = 1
  while(samplingFrameRows < sampleSize) {
    k = j + 99
    baseCorpusList = list()
    for(i in j:k){
      baseCorpus$sample = i
      baseCorpusList[[i]] = baseCorpus
    }
    
    sampleProp <- sample(minSampleProp:maxSampleProp, 1)
    sampleProp = sampleProp/100
    
    baseCorpusList = rbindlist(baseCorpusList)
    
    baseCorpusList = baseCorpusList %>% group_by(sample) %>% nest() %>% 
      mutate(data = map(data, ~ .x %>% 
                          dplyr::group_by(discipline) %>%
                          dplyr::slice_sample(prop=sampleProp, replace=FALSE) %>%
                          dplyr::ungroup() )) %>%
      mutate(total.word.count = map(data, ~ .x %>% 
                                    dplyr::summarize(total.word.count = sum(wordCount)))) %>%
      unnest(cols = c(total.word.count)) %>%
      filter(total.word.count %in% c(minWordCount:maxWordCount))
    
    
    if(nrow(samplingFrame) == 0){
      samplingFrame = baseCorpusList
      samplingFrameRows = nrow(samplingFrame)
    } else {
      samplingFrame = rbind(samplingFrame, baseCorpusList)
      samplingFrameRows = nrow(samplingFrame)
    }
    
    j = j + 100
    
  } #End of sampling procedure
  
  
  
  
  ####PREPARE TO READ TO FILE####
  
  samplingFrame = samplingFrame %>% unnest(cols = c(data))
  
  samplingFrame.path = paste(core.path, "Samples/", conds$subCorpusLength, "_", conds$saveSamplingFrame, sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  sessionInfoPath = paste(core.path, "SessionInfo/07_takeSamples_", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}
