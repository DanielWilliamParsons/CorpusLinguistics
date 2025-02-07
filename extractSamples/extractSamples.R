

extractSamples = function(core.path, folder.of.interest, items.file){
  
  # Set up the paths to relevant files
  # The folder of samples:
  folder.to.extract = paste(core.path, "Results/", folder.of.interest, sep="")
  # The index files for each bundle/frame
  items.path = paste(core.path, "indexFiles/", items.file, sep = "")
  
  # Collect each sample and read it inta a list
  all.samples = list.files(folder.to.extract, full.names = TRUE)
  
  sequence = seq(from = 1, to = length(all.samples), by = 1)
  
  getAllItems = function(i){
    this.sample = data.table::fread(all.samples[i])
    return(this.sample)
  }
  all.items = mclapply(sequence, getAllItems, mc.cores = 6)
  
  # Attach a sample id to each sample as a column
  my.sample.ids = c(1:10000)
  all.items = map2(all.items, my.sample.ids, ~cbind(.x, sampleID = .y))
  all.items = rbindlist(all.items)
  
  # Count how many times each item occurs by counting the
  # occurrence of frames across samples
  summary.of.items = all.items %>% group_by(frames) %>%
    summarize(occurrences = n(),
              mean.freq = mean(freq),
              mean.range.texts = mean(range.all),
              mean.range.disc = mean(range.disc, na.rm=FALSE))
  
  # Convert the item index into the real item
  # First load the items file
  all.items = data.table::fread(items.path)
  all.items = all.items %>% rename(total.freq.in.corpus = freq)
  
  # Now join these with the items sampled from the corpus
  # Then start to calculate cumulative occurrence values
  summary.of.items = summary.of.items  %>% 
    left_join(all.items, by = c("frames" = "index")) %>%
    rowwise() %>%
    mutate(weighted.occurrences = occurrences * mean.freq) %>% # This will calculate a weighted occurrence, but it needs dividing by the total frequency of all items
    ungroup()
  
  total.freq.all.items = sum(summary.of.items$weighted.occurrences)
  
  # Now complete the weighted.occurrences calculation.
  summary.of.items = summary.of.items %>% rowwise() %>%
    mutate(weighted.occurrences = weighted.occurrences / total.freq.all.items) %>%
    ungroup() %>%
    arrange(-weighted.occurrences) %>%
    mutate(cumulative.weighted.occurrence = cumsum(weighted.occurrences)) %>%
    mutate(accept.reject = if_else(cumulative.weighted.occurrence <= 0.95, 1, 0))
  
  # NOTE
  # The purpose of the cumulative.weighted.occurrence calculation is to identify the SET of lexical bundles
  # that we can say with 95% certainty represent the lexical bundles within the population of texts that have been sampled.
  # i.e., there is a 95% chance that if I sample the corpus and apply threshold and range filtering
  # that one of these bundles will be found.
  # The rest should be rejected as not necessarily representative under the threshold conditions.
    
  
  # Now save the data
  save.path = paste(core.path, "extractedItems/", folder.of.interest, ".csv", sep ="")
  data.table::fwrite(summary.of.items, save.path)
  
  # Return the data to allow inspection in the main file
  return(summary.of.items)
  
}