
# SCORES CONVERGENCE -----------------------------------------------------------

# ARGS: n_experiments = 10^2

source('R_functions.R')
args = commandArgs(trailingOnly=TRUE)
collections <- args[[1]]
n_experiments <- if (length(args)==2) as.numeric(args[[2]]) else 10^2
sample_sizes <- c(2^seq(3,23))


if (collections %in% c('pud','both')) {
    collection <- 'pud'
    length_def <- 'characters'
    suffix <- paste0("_",length_def)
    start <- Sys.time()
    print(collection)
    print(start)
    scores_df <- scores_convergence(collection,length_def,sample_sizes,n_experiments)
    end <- Sys.time()
    print(end)
    print(paste0('started at:',start, '- ended at:',end))
    write.csv(scores_df,here('results',paste0('scores_convergence_',collection,suffix,'.csv')))
} 
if (collections %in% c('cv','both')) {
    collection <- 'cv'
    print(collection)
    res <- lapply(c(length_defs), function(length) {
      suffix <- paste0("_",length)
      print(length)
      start <- Sys.time()
      print(start)
      scores_df <- scores_convergence(collection,length,sample_sizes,n_experiments)
      end <- Sys.time()
      print(end)
      print(paste0('started at:',start, '- ended at:',end))
      write.csv(scores_df,here('results',paste0('scores_convergence_',collection,suffix,'.csv')))
    })
}
