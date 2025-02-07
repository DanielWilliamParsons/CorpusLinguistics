# This function checks whether or not each sample contains rows of data
# It essentially counts the rows in each file and stores the number
# Note that this functions is only run on the second batch of files: 151 - 450
# which does the range comparison
# This is because this is where one of the cores reported an error.
# When parallelized, it is expected to take around 5 hours to run

errorChecker = function(core.path){
  dir.list = list.dirs(paste(core.path, "Results", sep =""), full.names=TRUE, recursive=FALSE)
  
  # Prepare to loop through the second batch of files.
  sequence = seq(from = 1, to = 450, by = 1)
  
  multiCoreChecker = function(i){
    samples = list.files(dir.list[i], full.names=TRUE)
    
    # Prepare a main data table to store metadata for each sample
    dts = data.table(path = c(), nrows = c())
    
    # Loop through each of the samples in the given directory
    for (j in 1:5){
      sample = data.table::fread(samples[j])
      sample.nrow = nrow(sample)
      
      #Write the meta data of this sample to a data table.
      dt = data.table(path = samples[j], nrows = sample.nrow)
      
      # Row-bind this sample's data table to the main data table of samples
      if(nrow(dts)==0){
        dts = dt
      } else {
        dts = rbind(dts, dt)
      }
    }
    
    path.edited = gsub(paste(core.path, "Results/", sep=""), "", dir.list[i])
    data.table::fwrite(dts, paste(core.path, "ErrorChecking/", path.edited, "_rowChecks.csv", sep =""))
    
  }
  mclapply(sequence, multiCoreChecker, mc.cores = 6)
  
  
}




errorChecker2 = function(core.path){
  dir.list = list.dirs(paste(core.path, "Results", sep =""), full.names=TRUE, recursive=FALSE)
  for(i in 1:450) {
    samples = list.files(dir.list[i], full.names=TRUE)
    
    # Prepare a main data table to store metadata for each sample
    dts = data.table(path = c(), nrows = c())
    
    sequence = seq(from = 1, to = 10000, by = 1)
    multiCoreFileRead = function(j){
      sample = data.table::fread(samples[j])
      return(sample)
    }
    sample = mclapply(sequence, multiCoreFileRead, mc.cores = 6)
    my.group.names = c(1:10000)
    
    # Attach a group number to each dataframe as a column
    sample = map2(sample, my.group.names, ~cbind(.x, groupID = .y))
    sample = rbindlist(sample)
    sample = sample %>% group_by(groupID) %>% summarize(nrows = n())
    # Note that the group idea here will represent the group in the list of files
    # The list of files is not in the same order as displayed on Finder, e.g., first file starts 1_, but second file will start 10_
    # and third file will start 100_ etc. So, it's important that when finding files, the group_id is correlated with the number in the list of files
    # and not the number attached to the sample file name
    
    
    path.edited = gsub(paste(core.path, "Results/", sep=""), "", dir.list[i])
    data.table::fwrite(sample, paste(core.path, "ErrorChecking/", path.edited, "_rowChecks.csv", sep =""))
    
  }
}

# Searches for an empty data table.
errorChecker3 = function(core.path){
  
  path.to.check = paste(core.path, "Results/188_range_prop_bundles_long_500000_40_c4_8", sep ="")
  sample.files = list.files(path.to.check, full.names=TRUE)
  
  # Search for the unusual/empty data frame.
  for(i in 1:10000){
    this.sample = data.table::fread(sample.files[i])
    print(i) #When the error turns up, this will stop and print the file number BEFORE the file containing the error
    if(!is.integer(nrow(this.sample))){
      print(i)
    }
  }
  
}

# Views a specific sample of lexical bundles
errorChecker4 = function(core.path){
  
  path.to.check = paste(core.path, "Results/188_range_prop_bundles_long_500000_40_c4_8", sep ="")
  sample.files = list.files(path.to.check, full.names=TRUE)
  
  sample.file = data.table::fread(sample.files[8229])
  print(sample.file)
  
}

#Checks number of files in each sample folder.
errorChecker5 = function(core.path){
  path.to.check = paste(core.path, "Results", sep ="")
  sample.dirs = list.dirs(path.to.check, full.names=TRUE, recursive=FALSE)
  print(sample.dirs)
  
  numSamples = list()
  for(i in 1:length(sample.dirs)){
    print(i)
    sample.paths = list.files(sample.dirs[i])
    sample.count = data.table(file.path = sample.dirs[i], file.count = length(sample.paths))
    numSamples[[i]] = sample.count
  }
  numSamples = rbindlist(numSamples)
  View(numSamples)
  
  #Observe samples that have less than 10000 samples.
  numSamples.less.than.tenthousand = numSamples %>% dplyr::filter(file.count < 10000)
  print(numSamples.less.than.tenthousand)
}

#Views a specific sample of files
errorChecker6 = function(core.path){
  
  sample.to.check = paste(core.path, "Samples/range_prop_bundles_long_500000_40_c4_8.gz", sep="")
  sample = data.table::fread(sample.to.check)
  View(sample)
  
}

#Check the number of directories containing samples (Should be 450)
errorChecker7 = function(core.path){
  
  dirs = list.dirs(paste(core.path, "Results", sep=""), recursive=FALSE)
  num.dirs = length(dirs)
  print(num.dirs)
  
}







