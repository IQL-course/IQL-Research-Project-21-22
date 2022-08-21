
# NULL HYPOTHESIS --------------------------------------------------------------
source('R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGS: iterations, collections, cores = 3, filter = T

iters         <- as.numeric(args[[1]])
collections   <- args[[2]]
cores         <- if (length(args)>=3) as.numeric(args[[3]]) else 3
filter        <- if (length(args)>=4) as.logical(args[[4]]) else T
filter_suffix <- ifelse(filter,'','_non_filtered')



if (collections %in% c('pud','both')) {
  length_def <- 'characters'
  collection <- 'pud'
  suffix <- paste0("_",length_def)
  print(Sys.time())
  scores <- mclapply(langs_df_pud$language, function(language) {
    compute_expectation_scores_lang(language,collection,length_def,iters,filter) 
  }, mc.cores = 3)
  print(Sys.time())
  null_df <- do.call(rbind.data.frame,scores)
  write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,filter_suffix,'.csv')))
} 

if (collections %in% c('cv','both')) {
  collection <- 'cv'
  print(collection)
  res <- lapply(c(length_defs), function(length) {
    suffix <- paste0("_",length)
    print(length)
    print(Sys.time())
    scores <- mclapply(langs_df_cv$language, function(language) {
      compute_expectation_scores_lang(language,collection,length,iters,filter) 
    },mc.cores=cores)
    null_df <- do.call(rbind.data.frame,scores)
    write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,filter_suffix,'.csv')))
    print(Sys.time())
  })
}




