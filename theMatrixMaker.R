

makeTheMatrix = function(core.path, matrixType){
  
  index.files.path = paste(core.path, "indexFiles/", matrixType, ".csv", sep="")
  index.files = data.table::fread(index.files.path)
  index.files = index.files %>% select(index) %>% rename(frames=index)

  corpus.files.path = paste(core.path, matrixType, sep ="")
  corpus.files = list.files(corpus.files.path, full.names=TRUE)
  corpus.files.names = list.files(corpus.files.path)
  #print(length(corpus.files.names)) #THere are 2761 files in BAWE
  
  # Join the columns of each individual file containing bundle/frame information to
  # the main index file which contains all the bundle/frame information for the full corpus
  # This will essentially become a matrix that contains 2761 columns, each column for one file of BAWE
  
  # Because there are many more phrase frames than bundles, the phrase frames have to be processed in batches
  # Hence the if statement and the for loops for BAWE_4frames. The process is only necessary one time for 4grams
  # and takes about one hour to run.
  
  final.matrix = ""
  for(j in 1:8){ #START OF BATCH PROCESSING
    print(j)
    if(j == 8){
      length = 345
    } else {
      length = 344
    }
    
    start.file = ((j*345) - 344)
    end.file = start.file + length
    
    sequence = seq(from = start.file, to = end.file, by = 1)
    getListOfDFs = function(i){
      df = data.table::fread(corpus.files[i])
      names(df)[names(df) == "freq"] <- corpus.files.names[i]
      return(df)
    }
    df.list = lapply(sequence, getListOfDFs)
    df.list = append(list(index.files), df.list)
    
    the.matrix = df.list %>% reduce(full_join, by = "frames") %>% distinct()
    #the.matrix = the.matrix %>% replace(is.na(.), 0) - using 0 instead of NA makes data.table's rowSums function count all the files in lexicalSampler.R 
    #so this can be ignored.
    #However, if using dplyr instead of data.table to take the lexical samples, then this is useful as it speed up dplyr.
    
    if(final.matrix == ""){
      final.matrix = the.matrix
    } else {
      the.matrix = the.matrix %>% select(-frames)
      final.matrix = cbind(final.matrix, the.matrix)
    }
  }
  print(nrow(final.matrix))
  save.matrix.path = paste(core.path, "Matrix/", matrixType, ".csv", sep="")
  data.table::fwrite(final.matrix, save.matrix.path)
  
  
  sessionInfoPath = paste(core.path, "SessionInfo/08_matrix_", matrixType, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  gc()
  
  
}