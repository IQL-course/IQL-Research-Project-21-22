source('R_functions.R')

# WARNING! Running takes a long time!

# ARGS: corr_type='kendall', collection

#Sys.setlocale("LC_ALL","English")
args = commandArgs(trailingOnly=TRUE)
corr_suffix <- paste0('_',args[[1]])
collections <- args[[2]]

# OPTIMALITY SCORES ------------------------------------------------------------
if (args[[1]] %in% c('kendall','spearman')) {  
    if (collections %in% c('pud','both')) {
      collection <- 'pud'
      print(collection)
      # - 1 - Significance of word lengths
      print('begin to compute tau correlations')
      tau_df <- compute_corr(collection,args[[1]])
      write.csv(tau_df, here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
      
      # - 2 - Compute scores
      print('begin to compute optimality scores')
      opt_df <- compute_optimality_scores_coll(collection,args[[1]],'characters')
      write.csv(opt_df, here('results',paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))
      
    } 
    if (collections %in% c('cv','both')) {
      collection <- 'cv'
      print(collection)
      res <- lapply(c(length_defs), function(length) {
        suffix <- paste0("_",length)
        print(length)
        # - 1 - Significance of word lengths
        print('begin to compute tau correlations')
        tau_df <- compute_corr(collection,args[[1]],length)
        write.csv(tau_df, here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
        
        # - 2 - Compute scores
        print('begin to compute optimality scores')
        opt_df <- compute_optimality_scores_coll(collection,args[[1]],length)
        write.csv(opt_df, here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
      })
    }
} else print("Choose and available correlation type, among: 'kendal, 'spearman'")




