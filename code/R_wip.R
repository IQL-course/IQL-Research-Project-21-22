source('code/R_functions.R')

# IMPLEMENTATION

lapply(COLLS, function(collection) {
  lapply(length_defs, function(length) {
    print(paste(collection, length,sep='-'))
    suffix <- ifelse(collection == 'pud','',paste0("_",length))
    
    # - 1 - Compute Omega
    #print('begin to compute optimality scores')
    #opt_df <- compute_optimality_scores(collection,length)
    #write.csv(opt_df, here('results',paste0('optimality_scores_',collection,suffix,'.csv')))
    
    # - 2 - Significance of word lengths
    print('begin to compute tau correlations')
    tau_df <- compute_tau_corr(collection,length)
    write.csv(tau_df, here('results',paste0('tau_correlation_',collection,suffix,'.csv')))
  
  })
})

