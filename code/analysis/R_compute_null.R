
# NULL HYPOTHESIS --------------------------------------------------------------
source('R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGS: iterations, collection, cores(CV) = 3, filter, other_def

iters       <- as.numeric(args[[1]])
collections <- args[[2]]
cores       <- if (length(args)>=3) as.numeric(args[[3]]) else 3
filter      <- if (length(args)>=4) as.logical(args[[4]]) else F
undersample <- if (length(args)>=5) as.numeric(args[[5]]) else 'no'

which_data <- ifelse(filter,'_newdata','_olddata')
folder <- if (filter==F) 'results' else 'results_filtered'


# INSTRUCTIONS FOR NULL HYPOTHESIS TESTING:
# - run the following command: Rscript R_compute_tables.R null 1000000 'job_index'
# where 'job_index' is the value from 1 to 4 that you have been assigned, and 'null' is required to 
# avoid running the computation of the optimality scores.
# - notice that the computation of pud is commented because it is already done.

# OLD JOBS DIVISION
# approx each: 21.532.712
# person 1: english (dur) 13.165.106 + german (dur) 5.569.590 + Kinyarwanda (dur) 2.673.259 <- 21.407.955
# person 2: (sorted_df$X.tokens[1:41] + Catalan + French) (dur) <- 21.657.468
# person 3: english (chars) 13.165.106 + german (chars) 5.569.590 + Kinyarwanda (chars) 2.673.259 <- 21.407.955
# person 4: (sorted_df$X.tokens[1:41] + Catalan + French) (chars) <- 21.657.468

# NEW JOB DIVISION
# 1: old data, new definition of omega
# 2: new data, old definition of omega

if (collections %in% c('pud','both')) {
  length_def <- 'characters'
  collection <- 'pud'
  suffix <- paste0("_",length_def)
  print(Sys.time())
  scores <- mclapply(langs_df_pud$language, function(language) {
    compute_expectation_scores_lang(language,collection,length_def,iters,filter,undersample) 
  }, mc.cores = 3)
  print(Sys.time())
  null_df <- do.call(rbind.data.frame,scores)
  write.csv(null_df, here(folder,paste0('null_hypothesis_',collection,suffix,'_',iters,'.csv')))
} 

if (collections %in% c('cv','both')) {
  collection <- 'cv'
  print(collection)
  res <- lapply(c(length_defs), function(length) {
    suffix <- paste0("_",length)
    print(length)
    print(Sys.time())
    scores <- mclapply(langs_df_cv$language, function(language) {
      compute_expectation_scores_lang(language,collection,length,n_experiments = iters,filter,undersample) 
    },mc.cores=cores)
    null_df <- do.call(rbind.data.frame,scores)
    write.csv(null_df, here(folder,paste0('null_hypothesis_',collection,suffix,'_',iters,which_data,'_',undersample,'.csv')))
    print(Sys.time())
  })
}




