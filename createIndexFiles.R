
createIndexFiles = function(core.path, type){
  
  bundles.path = paste(core.path, type, sep="")
  bundles.files = list.files(bundles.path, full.names=TRUE)
  
  sequence = seq(from = 1, to = length(bundles.files), by = 1)
  
  multiCoreBoost = function(i){
    
    bundles = data.table::fread(bundles.files[i])
    return(bundles)
    
  }
  bundles = mclapply(sequence, multiCoreBoost, mc.cores = 5)
  bundles = rbindlist(bundles)
  bundles = bundles %>% group_by(frames) %>% 
    summarize(freq = sum(freq)) %>% 
    arrange(-freq) %>%
    mutate(index = row_number())
  
  savepath = paste(core.path, "indexFiles/", type, ".csv", sep="")
  data.table::fwrite(bundles, savepath)
  
  # Now update the individual files so that the bundles and phrase frames are replaced with the indexes.
  # This will help to reduce memory costs later on
  
  bundles = bundles %>% select(frames, index)
  print(bundles)
  multiCoreBoost_write = function(i){
    
    bundles.info = data.table::fread(bundles.files[i])
    bundles.info = bundles.info %>% left_join(bundles, by=c("frames"="frames")) %>%
      select(-frames, index, freq) %>% rename(frames=index)
    data.table::fwrite(bundles.info, bundles.files[i])
  }
  mclapply(sequence, multiCoreBoost_write, mc.cores = 5)
  
  sessionInfoPath = paste(core.path, "SessionInfo/03_indexFiles_", type, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
}



