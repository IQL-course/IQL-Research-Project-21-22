source('R_functions.R')

# WARNING! Running takes a long time!

# INSTRUCTIONS FOR NULL HYPOTHESIS TESTING:
# - run the following command: Rscript R_compute_tables.R null 1000000 'job_index'
  # where 'job_index' is the value from 1 to 4 that you have been assigned, and 'null' is required to 
  # avoid running the computation of the optimality scores.
# - notice that the computation of pud is commented because it is already done.


args = commandArgs(trailingOnly=TRUE)
corr_suffix <- paste0('_',args[[1]])


# OPTIMALITY SCORES ------------------------------------------------------------
if (args[[1]] %in% c('kendall','spearman')) {  
# optimality scores and significance of relation
lapply(COLLS, function(collection) {
    print(collection)
    if (collection == 'pud') {
      # - 1 - Significance of word lengths
      print('begin to compute tau correlations')
      tau_df <- compute_corr(collection,args[[1]])
      write.csv(tau_df, here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
      
      # - 2 - Compute Omega
      print('begin to compute optimality scores')
      opt_df <- compute_optimality_scores_coll(collection,args[[1]],'characters')
      write.csv(opt_df, here('results',paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))
      
    } else if (collection == 'cv') {
      lapply(c(length_defs), function(length) {
        suffix <- paste0("_",length)
        print(length)
        # - 1 - Significance of word lengths
        print('begin to compute tau correlations')
        tau_df <- compute_corr(collection,args[[1]],length)
        write.csv(tau_df, here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
        
        # - 2 - Compute Omega
        print('begin to compute optimality scores')
        opt_df <- compute_optimality_scores_coll(collection,args[[1]],length)
        write.csv(opt_df, here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
      })
    }
})
  
}


# NULL HYPOTHESIS --------------------------------------------------------------

# JOBS DIVISION
# approx each: 21.532.712
# person 1: english (dur) 13.165.106 + german (dur) 5.569.590 + Kinyarwanda (dur) 2.673.259 <- 21.407.955
# person 2: (sorted_df$X.tokens[1:41] + Catalan + French) (dur) <- 21.657.468
# person 3: english (chars) 13.165.106 + german (chars) 5.569.590 + Kinyarwanda (chars) 2.673.259 <- 21.407.955
# person 4: (sorted_df$X.tokens[1:41] + Catalan + French) (chars) <- 21.657.468


if (length(args) >= 2) {
  iters     <- as.numeric(args[[2]])
  job_index <- if (length(args) >= 3) as.numeric(args[[3]]) else NULL
  cores     <- if (length(args) == 4) as.numeric(args[[4]]) else 3
  lapply(COLLS, function(collection) {
    print(collection)
    if (collection == 'pud') {
      #length_def <- 'characters'
      #suffix <- paste0("_",length_def)
      #print(Sys.time())
      #scores <- mclapply(langs_df_pud$language, function(language) {
      #  compute_expectation_scores_lang(language,collection,length_def,n_experiments = iters) 
      #}, mc.cores = 3)
      #print(Sys.time())
      #null_df <- do.call(rbind.data.frame,scores)
      #write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,'_kendall.csv')))
    } else if (collection == 'cv') {
      print(Sys.time())
      null_hyp_job_cv(job_index,iters,cores)
      print(Sys.time())
    }
  })
}


# with sample -> 89
# transform   -> 106 sec

