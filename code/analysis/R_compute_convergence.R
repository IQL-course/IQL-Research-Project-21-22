
# SCORES CONVERGENCE -----------------------------------------------------------

# ARGS: n_experiments = 10^2

source('R_functions.R')
args = commandArgs(trailingOnly=TRUE)
n_experiments <- if (length(args)==1) as.numeric(args[[1]]) else 10^2
sample_sizes <- c(2^seq(3,14))
languages <- langs_df_pud$language


start <- Sys.time()
print(start)
scores_df <- scores_convergence(languages,sample_sizes,n_experiments)
end <- Sys.time()
print(end)
print(paste0('started at:',start, '- ended at:',end))
write.csv(scores_df,here('results','scores_convergence.csv'))

