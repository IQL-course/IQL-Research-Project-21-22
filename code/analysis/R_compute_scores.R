source('R_functions.R')


# ARGUMENTS: corr_type  collections  what  filter
# where:
  # corr_type = ('kendall','spearman','pearson')
  # collections = ('pud','cv','both')
  # what = ('corr','scores','both')
  # filter = (T,F)

# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following commands,
# where [filter] can be set to either T or F:
  # - Rscript R_compute_scores.R kendall both both [filter]
  # - Rscript R_compute_scores.R spearman pud both [filter]
  # - Rscript R_compute_scores.R pearson both corr [filter]



Sys.setlocale("LC_ALL","English")     # might be needed for Windows
args = commandArgs(trailingOnly=TRUE)

corr_suffix <- if (args[[1]] == 'kendall') '' else paste0('_',args[[1]])
collections <- if (length(args)>=2) args[[2]] else 'both'
what        <- if (length(args)>=3) args[[3]] else 'both'
filter      <- if (length(args)>=4) as.logical(args[[4]]) else T

# OPTIMALITY SCORES ------------------------------------------------------------
if (args[[1]] %in% c('kendall','spearman','pearson')) {  
    if (collections %in% c('pud','both')) {
      collection <- 'pud'
      print(collection)
      if (what %in% c('corr','both')) {
        # - 1 - Significance of word lengths
        print('begin to compute tau correlations')
        tau_df <- compute_corr(collection,args[[1]],'characters',F,filter)
        write.csv(tau_df, here(which_folder('results',filter),paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
        }
      if (what %in% c('scores','both')) {
      # - 2 - Compute scores
      print('begin to compute optimality scores')
      opt_df <- compute_optimality_scores_coll(collection,args[[1]],'characters',F,filter)
      write.csv(opt_df, here(which_folder('results',filter),paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))
      }
    } 
    if (collections %in% c('cv','both')) {
      collection <- 'cv'
      print(collection)
      lengths <- if (args[[1]] == 'kendall') c(length_defs,'meanDuration') else length_defs
      res <- lapply(lengths, function(length) {
        suffix <- paste0("_",length)
        print(length)
        if (what %in% c('corr','both')) {
        # - 1 - Significance of word lengths
          print('begin to compute tau correlations')
          tau_df <- compute_corr(collection,args[[1]],length,F,filter)
          write.csv(tau_df, here(which_folder('results',filter),paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
        }
        if (what %in% c('scores','both')) {
          ## - 2 - Compute scores
          print('begin to compute optimality scores')
          opt_df <- compute_optimality_scores_coll(collection,args[[1]],length,F,filter)
          write.csv(opt_df, here(which_folder('results',filter),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
        }
      })
    }
} else print("Choose and available correlation type, among: 'kendal, 'spearman', 'pearson'")




