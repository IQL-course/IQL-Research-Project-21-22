
# SCORES CONVERGENCE -----------------------------------------------------------
source('code/analysis/R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGUMENTS: collections  n_experiments  filter length_def

## description:
  # - collections:    collection to be used
  # - n_experiments:  number of experiments for each sample size, on which average is computed
  # - filter:         whether to apply the optional filtering
  # - length_def:     which definition of length to use in cv

## values:
  # - collections:    one of ('pud','cv','both') [default is 'both']
  # - n_experiments:  any integer value [default is 10^2]
  # - filter:         one of (T,F) [default is T]
  # - length_def:     one of ('characters','medianDuration') [default is 'characters']

# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following command:
  # - Rscript R_compute_convergence.R both [n_experiments] [filter] ['all']


collections   <- if (length(args)>=1) args[[1]] else 'both'
n_experiments <- if (length(args)>=2) as.numeric(args[[2]]) else 10^2
filter        <- if (length(args)>=3) as.logical(args[[3]]) else T
length_def    <- if (length(args)>=4) as.character(args[[4]]) else 'characters'
sample_sizes  <- c(2^seq(3,23))

# GLOBALS  --------------------------------------------------------
## pud
langs_df_pud <- read.csv(here(which_folder('data',filter),"descriptive_tables/pud.csv"))
## cv
langs_df_cv <- read.csv(here(which_folder('data',filter),"descriptive_tables/common_voice.csv")) %>% 
  shorten_names()

# CONVERGENCE SCORES --------------------------------------------------------
if (collections %in% c('pud','both')) {
    collection <- 'pud'
    length <- 'characters'
    run_convergence(collection,length,sample_sizes,n_experiments,filter)
} 
if (collections %in% c('cv','both')) {
    collection <- 'cv'
    print(collection)
    if (length_def == 'all') {
      res <- lapply(c(length_defs), function(length) {
        run_convergence(collection,length,sample_sizes,n_experiments,filter)
      })
    } else run_convergence(collection,length_def,sample_sizes,n_experiments,filter)
}


