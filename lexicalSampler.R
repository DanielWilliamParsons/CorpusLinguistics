
# The inputs range_start and range_end were added on May 6th so that the program could be called more than once
# without needing me to come in and check the computer.
sampleLexicalFeatures = function(core.path, range_start, range_end){
  
  sampling.conditions = data.table::fread(paste(core.path, "baseCorporaMetadata/samplingConditions.csv", sep=""))
  
  ####Set up the range table####
  # This will be used later when determining which bundles/phrase frames should be kept or discarded.
  range.conditions.table = data.table(code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                      text.num = c(0, 3, 4, 5, 3, 4, 3, 5, 4, 5),
                                      disc.num = c(0, 0, 0, 0, 2, 2, 3, 2, 3, 3))
  # This range table can be found in "frequency threshold sample size decisions" spreadsheet 
  # in the corpusMetadata folder, range worksheet.
  
  ####Read in the BAWE_4grams matrix####
  # The bundles (BAWE_4grams) matrix is introduced here because this is the first in the
  # sampling conditions
  type = "bundles"
  matrix = getMatrix(core.path, type)
  
  # This range should be i in 1:length(sampling.conditions)
  for(i in range_start:range_end){ #Set to 188:188 on April 28th after discovering an error. Updated again on May 6th to adjust for a second error.
    ####Read in the corpus samples####
    # Update this path depending on where the sample is saved
    sample.condition.path = paste(core.path, 
                               "Samples/", 
                               sampling.conditions$comparison[i], "_",
                               sampling.conditions$balance[i], "_", 
                               sampling.conditions$purpose[i], "_", 
                               sampling.conditions$subCorpusType[i], "_", 
                               sampling.conditions$subCorpusLength[i], "_", 
                               sampling.conditions$normFreqThresh[i], "_", 
                               sampling.conditions$rangeConditions[i], ".gz", sep = "")
    print(sample.condition.path)
    
    # Get the corpus sample
    corpus.sample = data.table::fread(sample.condition.path)
    # corpus.sample[, grouping:=rleid(sample)] # On May 6th an error was disovered here - if two consecutive sample sets have
                                             # accept only the first sample, they will be given the same rleid code
                                             # meaning that a double-sized sample will be created.
                                             # The following TWO lines ensure that this doesn't happen
    corpus.sample[, grouping_disc:=rleid(discipline)]
    corpus.sample = corpus.sample %>% mutate(grouping = if_else(grouping_disc%%4 == 0,
                                                                          grouping_disc/4,
                                                                          (1 + ((grouping_disc - (grouping_disc%%4))/4)))) 
    corpus.sample = corpus.sample %>% select(-sample) %>% group_split(grouping)
    
    
    ####Check the lexical bundles/phrases matrix####
    # We need to change the matrix when we change to sampling from bundles to
    # frames and vice versa.
    # The following lines ensure that we only read in the matrix files
    # when it is absolutely necessary.
    # When not necessary, we just continue to use the matrix file
    if(grepl("bundles", sample.condition.path) == TRUE){
      new_type = "bundles"
    } else {
      new_type = "frames"
    }
    
    if(new_type != type){
      type = new_type
      rm(matrix)
      matrix = getMatrix(core.path, type)
    }
    
    
    ####Set Threshold Conditions####
    # Get the frequency threshold and range conditions for this sample group
    raw.freq.threshold = sampling.conditions$rawFreqThresh[i] # The raw frequency threshold are hard coded in the samplingConditions spreadsheet.
                                                              # The calculations of these can be found in "frequency threshold sample size decisions" spreadsheet in corpusMetadata folder
    
    range.conditions = sampling.conditions$rangeConditions[i]
    range.conditions = stringr::str_sub(range.conditions, 4, stringr::str_length(range.conditions)) # This extracts the range part of the code
    range.conditions.text.num = range.conditions.table$text.num[range.conditions.table$code==range.conditions] # Get the range text dimension from the table
    range.conditions.disc.num = range.conditions.table$disc.num[range.conditions.table$code==range.conditions] # Get the range discipline dimension from the table
    
    # Filter the matrix depending on the conditions
    # Do this for each corpus sample (10,000 times!)
    sample.size = sampling.conditions$sampleSize[i]
    sequence = seq(from = 1, to = 10000, by = 1)
    s = Sys.time()
    multiCoreLexisSampling = function(j){
      
      
      # Get one sample of the corpus sets
      cs = corpus.sample[[j]]
      
      # Get the list of file names from the sample in order to filter the
      # data frame columns which are named after the file names in the matrix
      cs.files = cs %>% pull(file)
      
      # Make a vector containing the term "frames" so that we can filter
      # this column in the data table
      cs.files.frame = c("frames", cs.files)
      
      # Filter the relevant files for this sample from the matrix
      lexis = matrix[, ..cs.files.frame]
      
      #### FILTER THRESHOLD FREQUENCY ####
      # Using the threshold frequency, filter only those bundles which
      # are equal to or above the threshold frequency
      lexis = lexis[, freq:=rowSums(.SD, na.rm=T), .SDcols=cs.files
                    ][freq>=raw.freq.threshold
                      ]
      
      #### FILTER RANGE CRITERIA ####
      # The type of filtering that takes place next depend on the range
      # conditions
      if(range.conditions == "1"){
        
        lexis = lexis[, range.all:=""
                      ][, range.disc:=""
                        ]
        
      } else if (range.conditions == "2" | range.conditions == "3" | range.conditions == "4"){
        
        lexis = lexis[, range.all:=rowSums(!is.na(.SD)), .SDcols=cs.files
                      ][range.all>=range.conditions.text.num
                        ][, range.disc:=""
                          ]
        
      } else {
        
        AH.files = cs %>% filter(discipline == "AH") %>% pull(file)
        LS.files = cs %>% filter(discipline == "LS") %>% pull(file)
        PS.files = cs %>% filter(discipline == "PS") %>% pull(file)
        SS.files = cs %>% filter(discipline == "SS") %>% pull(file)
        
        lexis = lexis[, range.AH:=rowSums(!is.na(.SD)), .SDcols=AH.files
                      ][, range.LS:=rowSums(!is.na(.SD)), .SDcols=LS.files
                        ][, range.PS:=rowSums(!is.na(.SD)), .SDcols=PS.files
                          ][, range.SS:=rowSums(!is.na(.SD)), .SDcols=SS.files
                            ][, range.all:=rowSums(!is.na(.SD)), .SDcols=cs.files
                              ][, range.disc:=rowSums(.SD>=range.conditions.text.num), .SDcols=c("range.AH", "range.LS", "range.PS", "range.SS")
                                ][range.all>=range.conditions.text.num
                                  ][range.disc>=range.conditions.disc.num
                                    ]
      }
      
      select_cols = c("frames", "freq", "range.all", "range.disc")
      lexis = lexis[, ..select_cols]
      
      # Prepare to save the sample of extracted lexical bundles/frames
      extracted.bundles.dir.path = paste(core.path, "Results/", i, "_",
                                  sampling.conditions$comparison[i], "_",
                                  sampling.conditions$balance[i], "_", 
                                  sampling.conditions$purpose[i], "_", 
                                  sampling.conditions$subCorpusType[i], "_", 
                                  sampling.conditions$subCorpusLength[i], "_", 
                                  sampling.conditions$normFreqThresh[i], "_", 
                                  sampling.conditions$rangeConditions[i], sep="")
      extracted.bundles.dir = file.path(extracted.bundles.dir.path)
      if(!dir.exists(extracted.bundles.dir)){
        dir.create(extracted.bundles.dir)
      }
      
      extracted.bundles.save.path = paste(extracted.bundles.dir.path, "/sample_", j, ".gz", sep="")
      data.table::fwrite(lexis, extracted.bundles.save.path)
      
      return(lexis)
      
    }
    lexis = mclapply(sequence, multiCoreLexisSampling, mc.cores=4, mc.silent=TRUE)
    rm(corpus.sample)
    
    # Write the session info for the given set of 10,000 samples
    session.path = paste(core.path, "SessionInfo/9_", i, "_",
                         sampling.conditions$comparison[i], "_",
                         sampling.conditions$balance[i], "_", 
                         sampling.conditions$purpose[i], "_", 
                         sampling.conditions$subCorpusType[i], "_", 
                         sampling.conditions$subCorpusLength[i], "_", 
                         sampling.conditions$normFreqThresh[i], "_", 
                         sampling.conditions$rangeConditions[i], ".txt", sep="")
    writeLines(capture.output(devtools::session_info(include_base=TRUE)), session.path)
    f = Sys.time()
    print(f-s)
    #print(lexis)
  }
  
}


getMatrix = function(core.path, type){
  
  if(type == "bundles"){
    type = "BAWE_4grams"
  } else {
    type = "BAWE_4frames"
  }
  matrix.path = paste(core.path, "Matrix/", type, ".csv", sep="")
  matrix = data.table::fread(matrix.path)
  
  print(ncol(matrix))
  print(nrow(matrix))
  return(matrix)
  
}

sampleLexicalFeaturesTesting = function(core.path, sample_start, sample_end){
  
  sampling.conditions = data.table::fread(paste(core.path, "baseCorporaMetadata/samplingConditions.csv", sep=""))
  
  ####Set up the range table####
  # This will be used later when determining which bundles/phrase frames should be kept or discarded.
  range.conditions.table = data.table(code = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                                      text.num = c(0, 3, 4, 5, 3, 4, 3, 5, 4, 5),
                                      disc.num = c(0, 0, 0, 0, 2, 2, 3, 2, 3, 3))
  # This range table can be found in "frequency threshold sample size decisions" spreadsheet 
  # in the corpusMetadata folder, range worksheet.
  
  ####Read in the BAWE_4grams matrix####
  # The bundles (BAWE_4grams) matrix is introduced here because this is the first in the
  # sampling conditions
  type = "bundles"
  matrix = getMatrix(core.path, type)
  
  
  for(i in sample_start:sample_end){
    ####Read in the corpus samples####
    # Update this path depending on where the sample is saved
    sample.condition.path = paste(core.path, 
                                  "Samples/", 
                                  sampling.conditions$comparison[i], "_",
                                  sampling.conditions$balance[i], "_", 
                                  sampling.conditions$purpose[i], "_", 
                                  sampling.conditions$subCorpusType[i], "_", 
                                  sampling.conditions$subCorpusLength[i], "_", 
                                  sampling.conditions$normFreqThresh[i], "_", 
                                  sampling.conditions$rangeConditions[i], ".gz", sep = "")
    print(sample.condition.path)
    
    # Get the corpus sample
    corpus.sample = data.table::fread(sample.condition.path)
    corpus.sample[, grouping:=rleid(sample)]
    corpus.sample = corpus.sample %>% select(-sample) %>% group_split(grouping)
    
    
    ####Check the lexical bundles/phrases matrix####
    # We need to change the matrix when we change to sampling from bundles to
    # frames and vice versa.
    # The following lines ensure that we only read in the matrix files
    # when it is absolutely necessary.
    # When not necessary, we just continue to use the matrix file
    if(grepl("bundles", sample.condition.path) == TRUE){
      new_type = "bundles"
    } else {
      new_type = "frames"
    }
    
    if(new_type != type){
      type = new_type
      rm(matrix)
      matrix = getMatrix(core.path, type)
    }
    
    
    ####Set Threshold Conditions####
    # Get the frequency threshold and range conditions for this sample group
    raw.freq.threshold = sampling.conditions$rawFreqThresh[i] # The raw frequency threshold are hard coded in the samplingConditions spreadsheet.
    # The calculations of these can be found in "frequency threshold sample size decisions" spreadsheet in corpusMetadata folder
    
    range.conditions = sampling.conditions$rangeConditions[i]
    range.conditions = stringr::str_sub(range.conditions, 4, stringr::str_length(range.conditions)) # This extracts the range part of the code
    range.conditions.text.num = range.conditions.table$text.num[range.conditions.table$code==range.conditions] # Get the range text dimension from the table
    range.conditions.disc.num = range.conditions.table$disc.num[range.conditions.table$code==range.conditions] # Get the range discipline dimension from the table
    
    # Filter the matrix depending on the conditions
    # Do this for each corpus sample (10,000 times!)
    sample.size = sampling.conditions$sampleSize[i]
    sequence = seq(from = 1, to = 10000, by = 1)
    s = Sys.time()
    
    multiCoreLexisSampling = function(j){
      
      
      # Get one sample of the corpus sets
      cs = corpus.sample[[j]]
      
      # Get the list of file names from the sample in order to filter the
      # data frame columns which are named after the file names in the matrix
      cs.files = cs %>% pull(file)
      
      # Make a vector containing the term "frames" so that we can filter
      # this column in the data table
      cs.files.frame = c("frames", cs.files)
      
      # Filter the relevant files for this sample from the matrix
      lexis = matrix[, ..cs.files.frame]
      
      #### FILTER THRESHOLD FREQUENCY ####
      # Using the threshold frequency, filter only those bundles which
      # are equal to or above the threshold frequency
      lexis = lexis[, freq:=rowSums(.SD, na.rm=T), .SDcols=cs.files
      ][freq>=raw.freq.threshold
      ]
      
      #### FILTER RANGE CRITERIA ####
      # The type of filtering that takes place next depend on the range
      # conditions
      if(range.conditions == "1"){
        
        lexis = lexis[, range.all:=""
        ][, range.disc:=""
        ]
        
      } else if (range.conditions == "2" | range.conditions == "3" | range.conditions == "4"){
        
        lexis = lexis[, range.all:=rowSums(!is.na(.SD)), .SDcols=cs.files
        ][range.all>=range.conditions.text.num
        ][, range.disc:=""
        ]
        
      } else {
        
        AH.files = cs %>% filter(discipline == "AH") %>% pull(file)
        LS.files = cs %>% filter(discipline == "LS") %>% pull(file)
        PS.files = cs %>% filter(discipline == "PS") %>% pull(file)
        SS.files = cs %>% filter(discipline == "SS") %>% pull(file)
        
        lexis = lexis[, range.AH:=rowSums(!is.na(.SD)), .SDcols=AH.files
        ][, range.LS:=rowSums(!is.na(.SD)), .SDcols=LS.files
        ][, range.PS:=rowSums(!is.na(.SD)), .SDcols=PS.files
        ][, range.SS:=rowSums(!is.na(.SD)), .SDcols=SS.files
        ][, range.all:=rowSums(!is.na(.SD)), .SDcols=cs.files
        ][, range.disc:=rowSums(.SD>=range.conditions.text.num), .SDcols=c("range.AH", "range.LS", "range.PS", "range.SS")
        ][range.all>=range.conditions.text.num
        ][range.disc>=range.conditions.disc.num
        ]
      }
      
      select_cols = c("frames", "freq", "range.all", "range.disc")
      lexis = lexis[, ..select_cols]
      
      if(nrow(lexis) > 0){
        #print(j)
        nrows.sample = as.character(nrow(lexis))
        error.sampling = data.table(sample.num=j, nrows = nrows.sample)
      } else {
        #print(j)
        error.sampling = data.table(sample.num=j, nrows = "No bundles in the table!")
      }
      
      return(error.sampling)
    }
    error.sampling = lapply(sequence, multiCoreLexisSampling)#, mc.cores=4, mc.silent=TRUE)
    error.sampling = rbindlist(error.sampling)
    data.table::fwrite(error.sampling, paste(core.path, "ErrorChecking/error.sampling_188.csv", sep=""))
    rm(corpus.sample)
    f = Sys.time()
    print(f-s)
  }
}

