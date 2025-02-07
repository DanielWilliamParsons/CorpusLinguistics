
# The function analyzeCounts reads in sample conditions results files.
# The results files contain the number of bundles/frames extracted in each of 10000 samples.
# samp.conds refers to the sample conditions file which contains the information for the sample conditions
# start refers to the starting row in the sample conditions file
# end refers to the ending row in the sample conditions file

# Output is a data table containing the mean number of bundles/frames
# across each set of 10000 samples for all the conditions from start to end
# The data frame output columns are
# mean.num.types
# purpose (bundles or frames)
# subCorpusType (long / mid / short)
# subCorpusLength (1000000, 900000, 700000, 500000, 300000)
# rangeConditions (from c2_2 to c6_10) - see the Excel sheet "frequency threshold and sample size decision"
# Final Sampling Decision tab in the corpusMetadata folder
# to see the definitions of the frequency and range criteria for rangeConditions

analyzeExpectedTypeFreqs = function(core.path, samp.conds, start, end){
  
  sequence = seq(from = start, to = end, by = 1)
  
  readFiles = function(i){
    file.num = as.character(i)
    file_name = paste(samp.conds$comparison[i],
                      samp.conds$balance[i],
                      samp.conds$purpose[i],
                      samp.conds$subCorpusType[i],
                      as.character(samp.conds$subCorpusLength[i]),
                      as.character(samp.conds$normFreqThresh[i]),
                      samp.conds$rangeConditions[i], sep = "_")
    file.path = paste(core.path, "extractedItemsExpected/", file_name, ".csv", sep="")
    items = data.table::fread(file.path)
    
    items = items %>% summarize(expected.type.freq = n())
    items$comparison = samp.conds$comparison[i]
    items$purpose = samp.conds$purpose[i]
    items$subCorpusType = samp.conds$subCorpusType[i]
    items$subCorpusLength = samp.conds$subCorpusLength[i]
    items$rangeConditions = samp.conds$rangeConditions[i]
    
    return(items)
    
  }
  items = lapply(sequence, readFiles)
  items = rbindlist(items)
  print(items)
  return(items)
  
  
  
}






