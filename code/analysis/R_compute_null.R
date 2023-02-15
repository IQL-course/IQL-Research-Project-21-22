
# NULL HYPOTHESIS --------------------------------------------------------------
source('code/analysis/R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGUMENTS: randomizations  collections  cores  filter length_def

## description:
  # - randomizations: number of randomizations to perform
  # - collections:    collection to be used
  # - cores:          number of cores to be used
  # - filter:         whether to apply the optional filtering
  # - length_def:     which definition of length to use in cv


## values:
  # - randomizations: any integer value
  # - collections:    one of ('pud','cv','both') [default is 'both']
  # - cores:          any integer value lower than or equal to your available number of cores [default is 1]
  # - filter:         one of (T,F) [default is T]
  # - length_def:     one of ('characters','medianDuration') [default is 'characters']


# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following commands:
  # - Rscript R_compute_null.R 1000 both [cores] [filter]  ['all']
  # - Rscript R_compute_null.R 10000 both [cores] [filter]  ['all']
  # - Rscript R_compute_null.R 100000 both [cores] [filter]  ['all']
  # - Rscript R_compute_null.R 1000000 both [cores] [filter] ['all']

randomizations <- as.numeric(args[[1]])
collections    <- if (length(args)>=2) args[[2]] else 'both'
cores          <- if (length(args)>=3) as.numeric(args[[3]]) else 1
filter         <- if (length(args)>=4) as.logical(args[[4]]) else T
length_def     <- if (length(args)>=5) as.character(args[[5]]) else 'characters'


# GLOBALS  --------------------------------------------------------
## pud
langs_df_pud <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/pud.csv"))
## cv
langs_df_cv <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/common_voice.csv")) %>% 
  shorten_names()


# SCORES EXPECTATIONS   --------------------------------------------------------
if (collections %in% c('pud','both')) {
  length_def <- 'characters'
  collection <- 'pud'
  run_null(length_def,collection,randomizations,filter,cores)
} 

if (collections %in% c('cv','both')) {
  collection <- 'cv'
  print(collection)
  if (length_def == 'all') {
  res <- lapply(c(length_defs), function(length) {
    run_null(length,collection,randomizations,filter,cores)
  })
  } else run_null(length_def,collection,randomizations,filter,cores)
}



