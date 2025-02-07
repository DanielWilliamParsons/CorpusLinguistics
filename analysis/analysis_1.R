
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

analyzeCounts = function(core.path, samp.conds, start, end){
  
  sequence = seq(from = start, to = end, by = 1)
  
  readFiles = function(i){
    file.num = as.character(i)
    file_name = paste(i, 
                      samp.conds$comparison[i],
                      samp.conds$balance[i],
                      samp.conds$purpose[i],
                      samp.conds$subCorpusType[i],
                      as.character(samp.conds$subCorpusLength[i]),
                      as.character(samp.conds$normFreqThresh[i]),
                      samp.conds$rangeConditions[i],
                      "counts.csv", sep = "_")
    file.path = paste(core.path, "Results_collated/", file_name, sep="")
    count.data = data.table::fread(file.path)
    return(count.data)
  }
  count.data = lapply(sequence, readFiles)
  
  count.data.summary = count.data %>% map(. %>% summarize(mean.num.types = mean(nrows)))
  count.data.summary = rbindlist(count.data.summary)
  
  prepMetaData = function(i){
    df = data.table(purpose = samp.conds$purpose[i],
                    subCorpusType = samp.conds$subCorpusType[i],
                    subCorpusLength = as.character(samp.conds$subCorpusLength[i]),
                    rangeConditions = samp.conds$rangeConditions[i])
    return(df)
  }
  metadata = lapply(sequence, prepMetaData)
  metadata = rbindlist(metadata)
  
  count.data.summary = cbind(count.data.summary, metadata)
  count.data.summary = count.data.summary %>% mutate(subCorpusLength = as.numeric(subCorpusLength))
  return(count.data.summary)
  
}






