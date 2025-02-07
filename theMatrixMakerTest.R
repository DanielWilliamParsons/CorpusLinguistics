if(matrixType == "BAWE_4frames"){
  
  for(j in 1:8){ #START OF BATCH PROCESSING
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
    
    the.matrix = df.list %>% reduce(left_join, by = "frames")
    the.matrix = the.matrix %>% replace(is.na(.), 0)
    View(the.matrix)
    
    save.matrix.path = paste(core.path, "Matrix/", matrixType, "_", j, ".csv", sep="")
    data.table::fwrite(the.matrix, save.matrix.path)
  } ### END OF BATCH PROCESSING
  
  
} else {
  sequence = seq(from = 1, to = length(corpus.files), by = 1)
  
  getListOfDFs = function(i){
    df = data.table::fread(corpus.files[i])
    names(df)[names(df) == "freq"] <- corpus.files.names[i]
    return(df)
  }
  df.list = lapply(sequence, getListOfDFs)
  df.list = append(list(index.files), df.list)
  
  the.matrix = df.list %>% reduce(left_join, by = "frames")
  the.matrix = the.matrix %>% replace(is.na(.), 0)
  View(the.matrix)
  
  save.matrix.path = paste(core.path, "Matrix/", matrixType, ".csv", sep="")
  data.table::fwrite(the.matrix, save.matrix.path)
}