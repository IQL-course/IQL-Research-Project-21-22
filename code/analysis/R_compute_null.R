
# NULL HYPOTHESIS --------------------------------------------------------------
source('code/analysis/R_functions.R')
args = commandArgs(trailingOnly=TRUE)

# ARGUMENTS: randomizations  collections  cores  filter

## description:
  # - randomizations: number of randomizations to perform
  # - collections:    collection to be used
  # - cores:          number of cores to be used
  # - filter:         whether to apply the optional filtering

## values:
  # - randomizations: any integer value
  # - collections:    one of ('pud','cv','both') [default is 'both']
  # - cores:          any integer value lower than or equal to your available number of cores [default is 1]
  # - filter:         one of (T,F) [default is T]


# NOTE:
# The minimum outputs to run the script "R_analysis.R" are obtained by the following commands:
  # - Rscript R_compute_null.R 1000 both [cores] [filter]
  # - Rscript R_compute_null.R 10000 both [cores] [filter]
  # - Rscript R_compute_null.R 100000 both [cores] [filter]
  # - Rscript R_compute_null.R 1000000 both [cores] [filter]

randomizations <- as.numeric(args[[1]])
collections    <- if (length(args)>=2) args[[2]] else 'both'
cores          <- if (length(args)>=3) as.numeric(args[[3]]) else 1
filter         <- if (length(args)>=4) as.logical(args[[4]]) else T


# GLOBALS  --------------------------------------------------------
## pud
langs_df_pud <- read.csv(here(which_folder('data',filter),"descriptive_tables/pud.csv"))
## cv
langs_df_cv <- read.csv(here(which_folder('data',filter),"descriptive_tables/common_voice.csv")) %>% 
  shorten_names()


# SCORES EXPECTATIONS   --------------------------------------------------------
if (collections %in% c('pud','both')) {
  length_def <- 'characters'
  collection <- 'pud'
  suffix <- paste0("_",length_def)
  print(Sys.time())
  scores <- mclapply(langs_df_pud$language, function(language) {
    compute_expectation_scores_lang(language,collection,length_def,randomizations,filter) 
  }, mc.cores = cores)
  print(Sys.time())
  null_df <- do.call(rbind.data.frame,scores)
  write.csv(null_df, here(which_folder('results',filter),paste0('null_hypothesis_',collection,suffix,'_',randomizations,'.csv')))
} 

if (collections %in% c('cv','both')) {
  collection <- 'cv'
  print(collection)
  res <- lapply(c(length_defs), function(length) {
    suffix <- paste0("_",length)
    print(length)
    print(Sys.time())
    scores <- mclapply(langs_df_cv$language, function(language) {
      compute_expectation_scores_lang(language,collection,length,randomizations,filter) 
    },mc.cores=cores)
    null_df <- do.call(rbind.data.frame,scores)
    write.csv(null_df, here(which_folder('results',filter),paste0('null_hypothesis_',collection,suffix,'_',randomizations,'.csv')))
    print(Sys.time())
  })
}




