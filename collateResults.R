

collateResults = function(core.path){
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
    data.table::fwrite(sample, paste(core.path, "Results_collated/", path.edited, "_counts.csv", sep =""))
    
  }
}