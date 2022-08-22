
# CORRELATIONS AND OPTIMALITY SCORES  ------------------------------------------
source('code/analysis/R_functions.R')
Sys.setlocale("LC_ALL","English")     # might be needed for Windows
args = commandArgs(trailingOnly=TRUE)

# ARGUMENTS: corr_type  collections  what  filter

## description:
  # - corr_type:   correlation to be used
  # - collections: collections to be used
  # - what :       what should be computed
  # - filter:      whether to apply the optional filtering

## values:
  # - corr_type:   one of ('kendall','spearman','pearson')
  # - collections: one of ('pud','cv','both') [default is 'both']
  # - what :       one of ('corr','scores','both') [default is 'both']
  # - filter:      one of (T,F) [default is T]

# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following commands:
  # - Rscript R_compute_scores.R kendall both both [filter]
  # - Rscript R_compute_scores.R spearman pud both [filter]
  # - Rscript R_compute_scores.R pearson both corr [filter]


corr_suffix <- if (args[[1]] == 'kendall') '' else paste0('_',args[[1]])
collections <- if (length(args)>=2) args[[2]] else 'both'
what        <- if (length(args)>=3) args[[3]] else 'both'
filter      <- if (length(args)>=4) as.logical(args[[4]]) else T


# GLOBALS  --------------------------------------------------------
## pud
langs_df_pud <- read.csv(here(which_folder('data',filter),"descriptive_tables/pud.csv"))
## cv
langs_df_cv <- read.csv(here(which_folder('data',filter),"descriptive_tables/common_voice.csv")) %>% 
  shorten_names()


# OPTIMALITY SCORES ------------------------------------------------------------
if (args[[1]] %in% c('kendall','spearman','pearson')) {  
    if (collections %in% c('pud','both')) {
      collection <- 'pud'
      print(collection)
      if (what %in% c('corr','both')) {
        # - 1 - Correlations with significance
        print('begin to compute tau correlations')
        tau_df <- compute_corr(collection,args[[1]],'characters',F,filter)
        write.csv(tau_df, here(which_folder('results',filter),paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))
        }
      if (what %in% c('scores','both')) {
      # - 2 - Optimality scores
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
        # - 1 - Correlations with significance
          print('begin to compute tau correlations')
          tau_df <- compute_corr(collection,args[[1]],length,F,filter)
          write.csv(tau_df, here(which_folder('results',filter),paste0('correlation_',collection,suffix,corr_suffix,'.csv')))
        }
        if (what %in% c('scores','both')) {
          ## - 2 - Optimality scores
          print('begin to compute optimality scores')
          opt_df <- compute_optimality_scores_coll(collection,args[[1]],length,F,filter)
          write.csv(opt_df, here(which_folder('results',filter),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))
        }
      })
    }
} else print("Choose and available correlation type, among: 'kendal, 'spearman', 'pearson'")




