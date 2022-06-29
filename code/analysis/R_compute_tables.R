source('R_functions.R')

# WARNING! Running takes a long time!


args = commandArgs(trailingOnly=TRUE)
corr_suffix <- paste0('_',args[[1]])

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


# null hypothesis

if (length(args) == 2) {
  iters <- as.numeric(args[[2]])
  lapply(COLLS, function(collection) {
    print(collection)
    if (collection == 'pud') {
      length_def <- 'characters'
      suffix <- paste0("_",length_def)
      print(Sys.time())
      scores <- mclapply(langs_df_pud$language, function(language) {
        compute_expectation_scores_lang(language,collection,length_def,n_experiments = iters) 
      }, mc.cores = 3)
      print(Sys.time())
      null_df <- do.call(rbind.data.frame,scores)
      write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,'_kendall.csv')))
    } else if (collection == 'cv') {
      print(Sys.time())
      lapply(c('medianDuration','characters'), function(length_def) {
        suffix <- paste0("_",length_def)
        scores <- mclapply(langs_df_cv$language, function(language) {
          compute_expectation_scores_lang(language,collection,length_def,n_experiments = iters) 
        }, mc.cores = 3)
        null_df <- do.call(rbind.data.frame,scores)
        write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,'_kendall.csv')))
      })
      print(Sys.time())
    }
  })
}




