source('R_functions.R')

# WARNING! Running takes a long time!


# checks on data - coefficient of variation in cv
rows <- lapply(1:length(ISO_cv), function(i) {
  iso_code <- ISO_cv[i]
  dialect  <- dialects_cv[i]
  read_language(iso_code,'cv',dialect) %>% 
    summarise(meanDuration,stDevDuration,coeff_var = stDevDuration/meanDuration)
})
df <- do.call(rbind,rows)
write.csv(df, here('results','coefficient_variation.csv'))



# optimality scores and significance of relation
lapply(COLLS, function(collection) {
  lapply(length_defs, function(length) {
    print(paste(collection, length,sep='-'))
    suffix <- ifelse(collection == 'pud','',paste0("_",length))
    
    # - 1 - Compute Omega
    print('begin to compute optimality scores')
    opt_df <- compute_optimality_scores(collection,length)
    write.csv(opt_df, here('results',paste0('optimality_scores_',collection,suffix,'.csv')))
    
    # - 2 - Significance of word lengths
    print('begin to compute tau correlations')
    tau_df <- compute_tau_corr(collection,length)
    write.csv(tau_df, here('results',paste0('tau_correlation_',collection,suffix,'.csv')))
    
  })
})