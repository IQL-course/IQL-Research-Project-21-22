
# SCORES CONVERGENCE -----------------------------------------------------------
source('R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGUMENTS: collections  n_experiments  filter

## description:
  # - collections:    collection to be used
  # - n_experiments:  number of experiments for each sample size, on which average is computed
  # - filter:         whether to apply the optional filtering

## values:
  # - collections:    one of ('pud','cv','both') [default is 'both']
  # - n_experiments:  any integer value [default is 10^2]
  # - filter:         one of (T,F) [default is T]

# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following command:
  # - Rscript R_compute_convergence.R both [n_experiments] [filter]


collections   <- if (length(args)>=1) args[[1]] else 'both'
n_experiments <- if (length(args)>=2) as.numeric(args[[2]]) else 10^2
filter        <- if (length(args)>=3) as.logical(args[[3]]) else F
sample_sizes <- c(2^seq(3,23))


if (collections %in% c('pud','both')) {
    collection <- 'pud'
    length_def <- 'characters'
    suffix <- paste0("_",length_def)
    start <- Sys.time()
    print(collection)
    print(start)
    scores_df <- scores_convergence(collection,length_def,sample_sizes,n_experiments,filter)
    end <- Sys.time()
    print(end)
    print(paste0('started at:',start, '- ended at:',end))
    write.csv(scores_df,here(which_folder('results',filter),paste0('scores_convergence_',collection,suffix,'.csv')))
} 
if (collections %in% c('cv','both')) {
    collection <- 'cv'
    print(collection)
    res <- lapply(c(length_defs), function(length) {
      suffix <- paste0("_",length)
      print(length)
      start <- Sys.time()
      print(start)
      scores_df <- scores_convergence(collection,length,sample_sizes,n_experiments,filter)
      end <- Sys.time()
      print(end)
      print(paste0('started at:',start, '- ended at:',end))
      write.csv(scores_df,here(which_folder('results',filter),paste0('scores_convergence_',collection,suffix,'.csv')))
    })
}
