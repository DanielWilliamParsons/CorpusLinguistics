# This approach is much faster since it uses multiple sampling through the map function
# from purrr, which significantly speeds up the processing.
# The parallel part take place over a set of samples within the function
# instead of parallelizing the function intself.
# Parallelizing the function is, in fact, slower because
# while one core might be working on sampling a short corpus which is fast
# another core might be working on sampling a long corpus which is time consuming.
# Therefore, parallelizing within the function allows all cores to work on
# one sample at a time which speeds everything up overall regardless if whether the
# sample is long-length text small corpus or short-length text large corpus.
# See note about parallel processing below

takeWordBalancedSamplesParallel <- function(core.path, conds, seed){
  ####PREPARE INPUT VARIABLES####
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
  
  ####PREPARE THE SAMPLING PROCEDURE####
  baseCorpusList = list()
  for(i in 1:250){
    baseCorpus$sample = i
    baseCorpusList[[i]] = baseCorpus
  }
  baseCorpusList = rbindlist(baseCorpusList)
  
  sequence = seq(from = 1, to = 6, by = 1)
  
  multiCoreSampling = function(i){
    seed.me = i + ((seed-1)*6)
    set.seed(seed.me)
    
    ####PREPARE THE OUTPUT VARIABLES####
    samplingFrame = data.table(sample=c(), data = c(), total.word.count = c())
    samplingFrameRows = ""
    
    sampleSize = ceiling(conds$sampleSize / 60) #Rounds up
    while(samplingFrameRows < sampleSize) {
      
      if(nrow(samplingFrame) == 0){
        samplingFrameRows = 0
      } else {
        samplingFrameRows = nrow(samplingFrame)
      }
      
      sampleProp <- sample(minSampleProp:maxSampleProp, 1)
      sampleProp = sampleProp/100
      
      baseCorpusSample = baseCorpusList %>% group_by(sample) %>% nest() %>% 
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
      
      
      baseCorpusSample = baseCorpusSample %>% select(sample, data)
      
      if(nrow(samplingFrame) == 0){
        samplingFrame = baseCorpusSample
        samplingFrameRows = nrow(samplingFrame)
      } else {
        samplingFrame = rbind(samplingFrame, baseCorpusSample)
        samplingFrameRows = nrow(samplingFrame)
      }
    } #END OF SAMPLING PROCEDURE
    
    return(samplingFrame)
  }
  samplingFrame = mclapply(sequence, multiCoreSampling, mc.cores=6)
  samplingFrame = rbindlist(samplingFrame)
  print(samplingFrame)
  
  
  ####PREPARE TO READ TO FILE####
  
  samplingFrame = samplingFrame %>% unnest(cols = c(data))
  samplingFrame = samplingFrame %>% select(sample, discipline, file, wordCount)
  
  samplingFrame.path = paste(core.path, "Samples/", conds$subCorpusLength, "_", conds$saveSamplingFrame, sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  sessionInfoPath = paste(core.path, "SessionInfo/07_takeSamples_", conds$balance, "_", conds$purpose, "_", conds$subCorpusType, "_", conds$subCorpusLength, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}


takePropBalancedSamplesParallel <- function(core.path, conds, seed){
  ####PREPARE INPUT VARIABLES####
  baseCorpus.path = paste(core.path, "baseCorporaMetadata/", conds$baseCorpora, sep="")
  baseCorpus = data.table::fread(baseCorpus.path)
  baseCorpus = baseCorpus %>% select(discipline, file, wordCount)
  
  
  # Prepare the minimum and maximum proportions of each base corpus to sample from
  minSampleProp = conds$realPropMin
  maxSampleProp = conds$realPropMax
  
  # Prepare the minimum and maximum total word counts for each discipline in the sampled base corpus
  minWordCount = conds$minSampleSize
  maxWordCount = conds$maxSampleSize
  
  
  ####PREPARE THE SAMPLING PROCEDURE####
  # This part creates a list of the 250 base corpora - all the same corpus
  # Why?
  # In order to speed up the sampling process, the whole list is sampled from
  # In other words, 250 samples can be taken at the same time.
  # A few other values were tested for speed, including 500 and 1000
  # and it was found that 250 appears to get a good efficiency.
  baseCorpusList = list()
  for(i in 1:250){
    baseCorpus$sample = i
    baseCorpusList[[i]] = baseCorpus
  }
  baseCorpusList = rbindlist(baseCorpusList)
  
  ####NOTE ABOUT PARALLEL PROCESSING####
  # The purpose of the sequence is to run 6 cores at the same time
  # with each core working on one set of 250 base corpora.
  # in other words, it is possible to take 250 * 6 samples in parallel: 1500
  # at any one time.
  # However, after filtering is done to ensure the sample is within the word count
  # criteria, it is unlikely that 1,500 samples will remain, especially for the 
  # lower word count corpora with long-length texts.
  # The while loop in this function checks the overall sample size
  # and continues the sampling until the total number of samples taken is larger
  # than the sample size.
  sequence = seq(from = 1, to = 6, by = 1)
  
  multiCoreSampling = function(i){
    
    # This approach to seeding ensures that each sample is different.
    # The second term is the seed for the base corpus being sampled, input to
    # this function.
    # The first term is an increment based on parallel processing
    seed.me = i + ((seed-1)*6)
    set.seed(seed.me)
    
    ####PREPARE THE OUTPUT VARIABLES####
    samplingFrame = data.table(sample=c(), data = c(), total.word.count = c())
    samplingFrameRows = ""
    
    # While the sample size is supposed to be 10,000
    # we divide here by 6
    # This is because we are using 6 cores and we wish each core to
    # sample 1/6 of the 10,000
    # They will be combined after all the cores have finished working
    # and there will likely be slightly more than 10,000 samples
    # The excess samples can be filtered when collecting the lexical bundles
    sampleSize = ceiling(conds$sampleSize / 6) #Rounds up
    
    while(samplingFrameRows < sampleSize) {
      
      if(nrow(samplingFrame) == 0){
        samplingFrameRows = 0
      } else {
        samplingFrameRows = nrow(samplingFrame)
      }
      
      sampleProp <- sample(minSampleProp:maxSampleProp, 1)
      sampleProp = sampleProp/100
      
      baseCorpusSample = baseCorpusList %>% group_by(sample) %>% nest() %>% 
        mutate(data = map(data, ~ .x %>% 
                            dplyr::group_by(discipline) %>%
                            dplyr::slice_sample(prop=sampleProp, replace=FALSE) %>%
                            dplyr::ungroup() )) %>%
        mutate(total.word.count = map(data, ~ .x %>% 
                                        dplyr::summarize(total.word.count = sum(wordCount)))) %>%
        unnest(cols = c(total.word.count)) %>%
        filter(total.word.count %in% c(minWordCount:maxWordCount))
      
      baseCorpusSample = baseCorpusSample %>% select(sample, data)
      
      if(nrow(samplingFrame) == 0){
        samplingFrame = baseCorpusSample
        samplingFrameRows = nrow(samplingFrame)
      } else {
        samplingFrame = rbind(samplingFrame, baseCorpusSample)
        samplingFrameRows = nrow(samplingFrame)
      }
      
    }
    return(samplingFrame)
    
  }
  samplingFrame = mclapply(sequence, multiCoreSampling, mc.cores=6)
  samplingFrame = rbindlist(samplingFrame)
  print(samplingFrame)
  
  
  ####PREPARE TO READ TO FILE####
  
  samplingFrame = samplingFrame %>% unnest(cols = c(data))
  samplingFrame = samplingFrame %>% select(sample, discipline, file, wordCount)
  
  samplingFrame.path = paste(core.path, 
                             "Samples/", 
                             conds$comparison, "_",
                             conds$balance, "_", 
                             conds$purpose, "_", 
                             conds$subCorpusType, "_", 
                             conds$subCorpusLength, "_", 
                             conds$normFreqThresh, "_", 
                             conds$rangeConditions, ".gz", sep = "")
  data.table::fwrite(samplingFrame, samplingFrame.path)
  
  sessionInfoPath = paste(core.path,
                          "SessionInfo/07_takeSamples_",
                          conds$comparison, "_",
                          conds$balance, "_", 
                          conds$purpose, "_", 
                          conds$subCorpusType, "_", 
                          conds$subCorpusLength, "_", 
                          conds$normFreqThresh, "_", 
                          conds$rangeConditions, ".txt", sep = "")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}
