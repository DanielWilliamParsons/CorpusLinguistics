

extractExpectedSet = function(core.path){
  
  # Get an index file (update as needed later)
  index.file.type = "BAWE_4grams.csv"
  indexes = data.table::fread(paste(core.path, "indexFiles/", index.file.type, sep = ""))
  
  sampling.conditions = data.table::fread(paste(core.path, "baseCorporaMetadata/samplingConditions.csv", sep=""))
  # Create a reference table for the range conditions that will allow filtering.
  range.conditions = data.table(condition = c("c2_2", "c3_2", "c4_2", "c5_2", "c6_2",
                                              "c4_1", "c4_3", "c4_4", "c4_5",
                                              "c4_6", "c4_7", "c4_8", "c4_9", "c4_10"),
                                text.range = c(3, 3, 3, 3, 3,
                                               0, 4, 5, 3, 
                                               4, 3, 5, 4, 5),
                                disc.range = c(3, 3, 3, 3, 3,
                                               0, 0, 0, 2,
                                               2, 3, 2, 3, 3))
  
  for(i in 1:nrow(sampling.conditions)){
    # Get the file name
    expected.set = paste(sampling.conditions$comparison[i],
                            sampling.conditions$balance[i],
                            sampling.conditions$purpose[i],
                            sampling.conditions$subCorpusType[i],
                            sampling.conditions$subCorpusLength[i],
                            sampling.conditions$normFreqThresh[i],
                            sampling.conditions$rangeConditions[i],
                            sep = "_")
    expected.set.file = paste(core.path, "expectedSets/", expected.set, ".csv", sep = "")
    expected.set.items = data.table::fread(expected.set.file)
    
    # Check if we need to load in a different index file.
    if(sampling.conditions$purpose[i] == "bundles"){
      new.index.file.type = "BAWE_4grams.csv"
    } else {
      new.index.file.type = "BAWE_4frames.csv"
    }
    
    if(new.index.file.type != index.file.type){
      index.file.type = new.index.file.type
      indexes = data.table::fread(paste(core.path, "indexFiles/", index.file.type, sep = ""))
    }
    
    # Get the filtering criteria
    min.text.range = range.conditions$text.range[range.conditions$condition == sampling.conditions$rangeConditions[i]]
    min.disc.range = range.conditions$disc.range[range.conditions$condition == sampling.conditions$rangeConditions[i]]
    raw.freq.thresh = sampling.conditions$rawFreqThres[i]
    
    # Perform the filter and join the index to the expected set
    expected.set.items = expected.set.items %>% 
      filter(CI_upper.mean.freq >= raw.freq.thresh) %>%
      filter(CI_upper.mean.range.text >= min.text.range) %>%
      filter(CI_upper.mean.range.disc >= min.disc.range) %>%
      left_join(indexes, by = c("frames" = "index"))
    
    
    save.file.name = paste(core.path, "extractedItemsExpected/", expected.set, ".csv", sep = "")
    data.table::fwrite(expected.set.items, save.file.name)
    
  }
  
}