
# core.path refers to the directory where the data is stored.
# type refers to the folder name either "BAWE_4grams" or "BAWE_4frames"

manageBundles = function(core.path, type){
  
  # 1. Retrieve the list of files for the type of lexis (4-grams or 4-frames)
  bundles.path = paste(core.path, type, sep="")
  bundles.files = list.files(bundles.path, full.names=TRUE)
  
  sequence = seq(from = 1, to = length(bundles.files), by = 1)
  
  # 2. Join the bundles together from the corpus into one large data frame
  #    and keep only the bundles that have a frequency greater than or equal to 6 (raw score for 20 per million in a 300,000 word corpus)
  multiCoreBoost_collateBundles = function(i){
    
    bundles = data.table::fread(bundles.files[i])
    
    return(bundles)
  }
  bundles = mclapply(sequence, multiCoreBoost_collateBundles, mc.cores = 5)
  bundles = rbindlist(bundles)
  bundles = bundles %>% group_by(frames) %>% summarize(freq = sum(freq)) %>% filter(freq >= 6) %>% pull(frames)
  
  # 3. Filter those bundles which occur 3 or more times in the whole corpus
  #    from each individual file and save back to the original location.
  multiCoreBoost_reduceBundles = function(i){
    bundles.to.reduce = data.table::fread(bundles.files[i])
    bundles.to.reduce = bundles.to.reduce %>% filter(frames %in% bundles)
    data.table::fwrite(bundles.to.reduce, bundles.files[i])
  }
  mclapply(sequence, multiCoreBoost_reduceBundles, mc.cores = 5)
  
  sessionInfoPath = paste(core.path, "SessionInfo/02_manage_", type, ".txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  bundles = 0
}