source('R_functions.R')

# WARNING! Running takes a long time!


args = commandArgs(trailingOnly=TRUE)
corr_suffix <- paste0('_',args[[1]])
  
# optimality scores and significance of relation
#lapply(COLLS, function(collection) {
#    print(collection)
#    if (collection == 'pud') {
#      # - 1 - Significance of word lengths
#      print('begin to compute tau correlations')
#      tau_df <- compute_corr(collection,args[[1]])
#      write.csv(tau_df, here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
#      
#      # - 2 - Compute Omega
#      print('begin to compute optimality scores')
#      opt_df <- compute_optimality_scores_coll(collection,args[[1]],'characters')
#      write.csv(opt_df, here('results',paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))
#      
#    } else if (collection == 'cv') {
#      #lapply(c(length_defs,'meanDuration'), function(length) {
#      #  suffix <- paste0("_",length)
#      #  print(length)
#      #  # - 1 - Significance of word lengths
#      #  print('begin to compute tau correlations')
#      #  tau_df <- compute_corr(collection,args[[1]],length)
#      #  write.csv(tau_df, here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
#      #  
#      #  # - 2 - Compute Omega
#      #  print('begin to compute optimality scores')
#      #  opt_df <- compute_optimality_scores_coll(collection,args[[1]],length)
#      #  write.csv(opt_df, here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
#      #})
#    }
#    
#})


# null hypothesis
iterations <- 10^6
lapply(COLLS, function(collection) {
  print(collection)
  if (collection == 'pud') {
    length_def <- 'characters'
    suffix <- paste0("_",length_def)
    scores <- lapply(langs_df_pud$language, function(language) {
      compute_expectation_scores_lang(language,collection,length_def,n_experiments = iterations) 
    })
    null_df <- do.call(rbind.data.frame,scores)
    write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_kendall.csv')))
  } else if (collection == 'cv') {
    lapply(c(length_defs), function(length_def) {
      suffix <- paste0("_",length_def)
      scores <- lapply(langs_df_cv$language, function(language) {
        compute_expectation_scores_lang(language,collection,length_def,n_experiments = iterations) 
      })
      null_df <- do.call(rbind.data.frame,scores)
      write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_kendall.csv')))
    })
  }
})




