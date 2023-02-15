
# CORRELATIONS AND OPTIMALITY SCORES  ------------------------------------------
source('code/analysis/R_functions.R')
#Sys.setlocale("LC_ALL","English")     # might be needed for Windows
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


collections <- if (length(args)>=1) args[[1]] else 'both'
filter      <- if (length(args)>=2) as.logical(args[[2]]) else T


# GLOBALS  --------------------------------------------------------
langs_df_pud <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/pud.csv"))
langs_df_cv <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/common_voice.csv")) %>% 
  shorten_names()


# OPTIMALITY SCORES ------------------------------------------------------------
if (collections %in% c('pud','both')) {
  collection <- 'pud'
  length_def <- 'characters'
  # compute
  corrs_kendall_df <- compute_corr(collection,'kendall',length_def,F,filter)
  corrs_pearson_df <- compute_corr(collection,'pearson',length_def,F,filter)
  opt_df           <- compute_optimality_scores_coll(corrs_kendall_df,collection,length_def,F,filter)
  # rename
  corrs_pearson_df <- rename(corrs_pearson_df, r = corr,   r_pval = pvalue, r_min = corr_min)
  corrs_kendall_df <- rename(corrs_kendall_df, tau = corr, tau_pval = pvalue, tau_min = corr_min)
  # merge
  df <- merge(corrs_kendall_df,corrs_pearson_df, by='language') %>% merge(opt_df,  by='language') %>% 
    mutate(length_def = length_def)
  write.csv(df, paste0(which_folder('results',filter),'/scores_',collection,'.csv'))
  
} else if (collections %in% c('cv','both')) {
  collection <- 'cv'
  res <- lapply(c(length_defs,'meanDuration'), function(length_def) {
    # compute
    corrs_kendall_df <- compute_corr(collection,'kendall',length_def,F,filter)
    corrs_pearson_df <- compute_corr(collection,'pearson',length_def,F,filter)
    opt_df           <- compute_optimality_scores_coll(corrs_kendall_df,collection,length_def,F,filter)
    # rename
    corrs_pearson_df <- rename(corrs_pearson_df, r = corr,   r_pval = pvalue, r_min = corr_min)
    corrs_kendall_df <- rename(corrs_kendall_df, tau = corr, tau_pval = pvalue, tau_min = corr_min)
    # merge
    merge(corrs_kendall_df,corrs_pearson_df, by='language') %>% merge(opt_df,  by='language') %>% 
      mutate(length_def = length_def)
  })
  df <- do.call(rbind.data.frame,res)
  write.csv(df, paste0(which_folder('results',filter),'/scores_',collection,'.csv'))
}




