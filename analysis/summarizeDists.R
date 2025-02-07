
summarizeDistributions = function(core.path, samp.conds, purp, subCorpType, subCorpLength, comp, rangeConds){
  
  #samp.conds = samp.conds %>% mutate(subCorpusLength = as.character(subCorpusLength))
  
  to.retrieve = samp.conds %>% filter(rangeConditions %in% rangeConds) %>%
    filter(subCorpusType %in% subCorpType) %>%
    filter(subCorpusLength %in% subCorpLength) %>%
    filter(comparison %in% comp) %>%
    filter(purpose %in% purp)
  
  print(to.retrieve)
  
  
  sequence = seq(from = 1, to = nrow(to.retrieve), by = 1)
  
  readData = function(i) {
    
    folder_number = ""
    if(comp == "range"){
      folder_number = to.retrieve$sampleFolder_2[i] + 150
    } else {
      folder_number = to.retrieve$sampleFolder_2[i]
    }
    
    file_name = paste(folder_number,
                         to.retrieve$comparison[i],
                         to.retrieve$balance[i],
                         to.retrieve$purpose[i],
                         to.retrieve$subCorpusType[i],
                         to.retrieve$subCorpusLength[i],
                         to.retrieve$normFreqThresh[i],
                         to.retrieve$rangeConditions[i],
                         "counts.csv",
                         sep = "_")
    path.to.file = paste(core.path, "Results_collated/", file_name, sep ="")
    dt = data.table::fread(path.to.file)
    dt = dt %>% mutate(type = to.retrieve$subCorpusType[i])
    return(dt)
    
  }
  sample.data = lapply(sequence, readData)
  
  return(sample.data)
  
  
}