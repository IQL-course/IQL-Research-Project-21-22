source('R_functions.R')

# WARNING! Running takes a long time!

# ARGS: corr_type=('kendall','spearman'), collection = ('pud','cv','both), what =('corr','scores','both'), filter?

#Sys.setlocale("LC_ALL","English")
args = commandArgs(trailingOnly=TRUE)

corr_suffix <- if (args[[1]] == 'kendall') '' else paste0('_',args[[1]])
collections <- args[[2]]
what <- args[[3]]
filter <- if (length(args)>=4) as.logical(args[[4]]) else T
folder <- if (filter==F) 'results' else 'results_filtered'

# OPTIMALITY SCORES ------------------------------------------------------------
if (args[[1]] %in% c('kendall','spearman','pearson')) {  
    if (collections %in% c('pud','both')) {
      collection <- 'pud'
      print(collection)
      if (what %in% c('corr','both')) {
        # - 1 - Significance of word lengths
        print('begin to compute tau correlations')
        tau_df <- compute_corr(collection,args[[1]],'characters',F,filter)
        write.csv(tau_df, here(folder,paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
        }
      if (what %in% c('scores','both')) {
      # - 2 - Compute scores
      print('begin to compute optimality scores')
      opt_df <- compute_optimality_scores_coll(collection,args[[1]],'characters',F,filter)
      write.csv(opt_df, here(folder,paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))
      }
    } 
    if (collections %in% c('cv','both')) {
      collection <- 'cv'
      print(collection)
      res <- lapply(c(length_defs), function(length) {
        suffix <- paste0("_",length)
        print(length)
        if (what %in% c('corr','both')) {
        # - 1 - Significance of word lengths
          print('begin to compute tau correlations')
          tau_df <- compute_corr(collection,args[[1]],length,F,filter)
          write.csv(tau_df, here(folder,paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
        }
        if (what %in% c('scores','both')) {
          ## - 2 - Compute scores
          print('begin to compute optimality scores')
          opt_df <- compute_optimality_scores_coll(collection,args[[1]],length,F,filter)
          write.csv(opt_df, here(folder,paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
        }
      })
    }
} else print("Choose and available correlation type, among: 'kendal, 'spearman'")




