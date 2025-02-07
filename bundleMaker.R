# This function reads all the BAWE csv files and creates lexical bundles and 
# lexical phrase frames for each file
# It stores the file in the core.path area

# Note that this function will not work without the original csv data frames

bundleMaker <- function(core.path){
  path.to.dfs = "/Volumes/Corpora/BAWE/BAWE_CORPUS_TextTag_Cleaned"
  list.of.files = list.files(path.to.dfs, full.names = TRUE)
  list.of.files.short = list.files(path.to.dfs)
  
  ####Get the metadata for all the files####
  original.corpus.file.path = "/Volumes/Corpora/BAWE/BAWE_CORPUS_ByDiscipline"
  list.of.original.files = list.files(original.corpus.file.path, recursive=TRUE, full.names=TRUE)
  list.of.original.files = gsub(original.corpus.file.path, "", list.of.original.files)
  metadata = data.table("filename" = list.of.original.files)
  metadata = metadata %>% mutate(discipline = substr(filename, 2, 3),
                                 temp1 = substr(filename, 5, nchar(filename))) %>%
    mutate(subject = gsub("/.*", "", temp1),
           file = gsub("^(.*?)/", "", temp1)) %>%
    mutate(file = substr(file, nchar(file)-8, nchar(file))) %>%
    mutate(file = gsub(".xml", ".csv", file)) %>%
    mutate(author = substr(file, 1, 4)) %>%
    mutate(filenNum = substr(file, 5, 5)) %>%
    select(-temp1, -filename)
  
  
  # Prepare for parallel processing
  sequence = seq(from = 1, to = length(list.of.files), by = 1)
  
  ####Process each file####
  multiCoreBoost = function(i){
    ####Retrieve the data####
    df = data.table::fread(list.of.files[i])
    
    ####Clean the data####
    # Step 1: Remove document start, all sentence boundaries and empty lines
    # Step 2: Then lower the text
    # Step 3: Then select only the token (removing POS and lemma information)
    # 
    df = df %>% filter(token != "\\*\\*6;0;START" & POS != "NULL") %>% # Step 1
      filter(POS != "-----") %>% 
      mutate(token = tolower(token)) %>% # Step 2
      select(token) # Step 3
    
    # To get the word count, exclude the punctuation first
    wordCt = df %>% filter(!grepl("[-!?.,:;=]", token)) %>%
      filter(!grepl("^'$", token)) %>% filter(!grepl("'s", token))
    wordCt = nrow(wordCt)
    
    ####Create bundles and frames####
    # Step 1: Create three copied columns of the token column that 
    #         are shifted up one row
    # Step 2: Combine the columns together, but keep the four new columns
    # Step 3: Filter out the bundles that contain punctuation
    # Step 4: Copy the columns to prepare the lexical phrase frames
    # Step 5: Create the frames: framesl means the middle left space is empty
    #                            framesr means the middle right space is empty
    # Step 6: Filter out anomalous bundles that get left over (These were discovered at Step 8: Create the matrix in the main program!!!)
    df = df %>% mutate(n1 = lead(token, n=0),
                       n2 = lead(token, n=1),
                       n3 = lead(token, n=2),
                       n4 = lead(token, n=3)) %>% # Step 1
      unite("bundles", n1:n4, sep = " ", remove=FALSE, na.rm=TRUE) %>% # Step 2
      select(-token) %>%
      filter(!grepl("[-!?.,:;=]", bundles)) %>% 
      filter(!grepl("' ", bundles)) %>% filter(!grepl(" 's", bundles)) %>%
      filter(!grepl(" '$", bundles)) %>% # Step 3
      mutate(m1 = n1, m2 = "*", m3 = n3, m4 = n4, p1 = n1, p2 = n2, p3 = "*", p4 = n4) %>% #Step 4
      unite("framesl", m1:m4, sep = " ", remove=TRUE, na.rm=TRUE) %>%
      unite("framesr", p1:p4, sep = " ", remove=TRUE, na.rm=TRUE) %>% #Step 5
      filter(!grepl(".* \\*$", framesl)) %>% filter(!grepl("^\\* .*$", framesl)) %>%
      filter(!grepl(".* \\*$", framesr)) %>% filter(!grepl("^\\* .*$", framesr)) #Step 6
    
    ####Count the bundles and frames in the current file####
    # Summarize the 4grams (bundles) information for the file
    bundles = df %>% select(bundles) %>% rename(frames = bundles) %>% #Renaming here so that every column can be referred to by the same name
      group_by(frames) %>% summarize(freq = n()) %>% arrange(-freq)
    
    # Summarize the 4frames (framesl) information for the file
    framesl = df %>% select(framesl) %>% rename(frames = framesl) %>%
      group_by(frames) %>% summarize(freq = n())
    
    # Summarize the 4frames (framesr) information for the file
    framesr = df %>% select(framesr) %>% rename(frames = framesr) %>%
      group_by(frames) %>% summarize(freq = n())
    
    # Join the framesl and framesr into one file
    frames = rbind(framesr, framesl)
    frames = frames %>% arrange(-freq)
    
    ####Save these frames####
    fp.bundles = paste(core.path, "BAWE_4grams/", list.of.files.short[i], sep="")
    data.table::fwrite(bundles, fp.bundles)
    
    fp.frames = paste(core.path, "BAWE_4frames/", list.of.files.short[i], sep="")
    data.table::fwrite(frames, fp.frames)
    
    ####Add word count to the metadata####
    thisMetadata = metadata %>% filter(file == list.of.files.short[i]) %>%
      mutate(wordCount = wordCt)
    
    return(thisMetadata)
      
  }
  
  finalMetadata = mclapply(sequence, multiCoreBoost, mc.cores = 5)
  
  ####Save the metadata####
  finalMetadata = rbindlist(finalMetadata)
  filename = paste(core.path, "corpusMetadata/fileByFileData.csv", sep="")
  data.table::fwrite(finalMetadata, filename)
  
  sessionInfoPath = paste(core.path, "SessionInfo/01_makeBundles.txt", sep="")
  writeLines(capture.output(devtools::session_info(include_base=TRUE)), sessionInfoPath)
  
}