
expectedSet = function(core.path){
  
  type = "bundles"
  matrix = getMatrix(core.path, type) # The matrix is needed to get the list of lexical bundles per file
  
  sampling.conditions = data.table::fread(paste(core.path, "baseCorporaMetadata/samplingConditions.csv", sep=""))
  
  for(i in 1:450){
    
    extracted.items = paste(i, 
                                 sampling.conditions$comparison[i],
                                 sampling.conditions$balance[i],
                                 sampling.conditions$purpose[i],
                                 sampling.conditions$subCorpusType[i],
                                 sampling.conditions$subCorpusLength[i],
                                 sampling.conditions$normFreqThresh[i],
                                 sampling.conditions$rangeConditions[i],
                                 sep = "_")
    extracted.items.files = paste(core.path, "extractedItems/", extracted.items, ".csv", sep = "")
    extracted.items.files.names.only = paste(extracted.items, ".gz", sep ="")
    extracted.items.files.names.only = sub(".*?_", "", extracted.items.files.names.only)
    
    print(i)
    Start = Sys.time()
    # Get the extracted bundles/frames into a vector.
    extracted.items = data.table::fread(extracted.items.files)
    extracted.items = extracted.items %>% pull(frames)
    
    # Check whether we need a new bundle/frame matrix
    if(grepl("bundles", extracted.items.files) == TRUE){
      new_type = "bundles"
    } else {
      new_type = "frames"
    }
    
    if(new_type != type){
      type = new_type
      rm(matrix)
      matrix = getMatrix(core.path, type)
    }
    
    
    
    # FReduce the size of the matrix so that only the bundles extracted are present in the matrix
    # This should significantly reduce the size of the matrix to make it more easy to work with.
    this.matrix = matrix %>% filter(frames %in% extracted.items)
    
    # Make the matrix into a long data frame so that file names are along side bundles and their 
    # freqs per file
    this.matrix = this.matrix %>% 
      pivot_longer(!frames, names_to = "file.name", values_to = "freq") %>%
      filter(!is.na(freq))
    files.in.matrix = this.matrix %>% pull(file.name)
    
    # Prepare the path to get the sample
    sample.condition.path = paste(core.path, "Samples/", extracted.items.files.names.only, sep ="")
    print(sample.condition.path)
    
    # Get the corpus sample and label it.
    corpus.sample = data.table::fread(sample.condition.path)
    corpus.sample[, grouping_disc:=rleid(discipline)]
    corpus.sample = corpus.sample %>% mutate(grouping = if_else(grouping_disc%%4 == 0,
                                                                grouping_disc/4,
                                                                (1 + ((grouping_disc - (grouping_disc%%4))/4)))) 
    corpus.sample = corpus.sample %>% select(-sample)
    
    # Filter the matrix further using the files that were selected from this set of corpus samples
    corpus.sample.files = corpus.sample %>% select(file) %>% distinct(file) %>% pull(file)
    this.matrix  = this.matrix %>% filter(file.name %in% corpus.sample.files)
    
    # Split the corpus samples into individual samples in a list
    corpus.sample = corpus.sample %>% group_split(grouping)
    
    # Now process each corpus sample
    sequence = seq(from = 1, to = 10000)
    multiCoreProcess = function(i){
      sample = corpus.sample[[i]] %>%
        left_join(this.matrix, by = c("file" = "file.name")) %>%
        group_by(grouping, frames) %>%
        summarize(bundle.freq = sum(freq), # Then sum the bundle as a mutate, then ungroup. This gives the frequency per sample.
               bundle.range.text = n(), # Then count the bundle as a mutate, then ungroup. This gives the text range.
               bundle.range.disc = n_distinct(discipline)) %>% # Count distinct disciplines for each frame, This gets the discipline range
        ungroup()
      return(sample)
    }
    samples = mclapply(sequence, multiCoreProcess, mc.cores = 5)
    samples = rbindlist(samples)
    
    # Summarize the data with means in order to get expected values for each bundle/frame
    samples = samples %>% group_by(frames) %>%
      summarize(mean.freq = mean(bundle.freq),
                sd.freq = sd(bundle.freq),
                sd.error.freq = sd(bundle.freq)/sqrt(n()),
                mean.range.text = mean(bundle.range.text),
                sd.range.text = sd(bundle.range.text),
                sd.range.text.error = sd(bundle.range.text)/sqrt(n()),
                mean.range.disc = mean(bundle.range.disc),
                sd.range.disc = sd(bundle.range.disc),
                sd.range.disc.error = sd(bundle.range.disc)/sqrt(n())) %>%
      mutate(CI_lower.mean.freq = mean.freq - (1.96*sd.error.freq),
             CI_upper.mean.freq = mean.freq + (1.96*sd.error.freq),
             CI_lower.mean.range.text = mean.range.text - (1.96*sd.range.text.error),
             CI_upper.mean.range.text = mean.range.text + (1.96*sd.range.text.error),
             CI_lower.mean.range.disc = mean.range.disc + (1.96*sd.range.disc.error),
             CI_upper.mean.range.disc = mean.range.disc - (1.96*sd.range.disc.error))
    
    
    # Finally, save the expected set file
    save.name = sub(".gz", ".csv", extracted.items.files.names.only)
    save.path = paste(core.path, "expectedSets/", save.name, sep ="")
    data.table::fwrite(samples, save.path)
    
    End = Sys.time()
    total = End - Start
    print(total)
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





