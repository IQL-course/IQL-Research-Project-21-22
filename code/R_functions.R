
library('here')
library('dplyr')
library('ISOcodes')
library('ggplot2')
require("ggrepel")

# TO DO 
# - get ISO codes of cvfa languages
# - compute Omega for cvfa



# globals
langs_df   <- read.csv(here("pud_data/utils/PUD_languages.csv")) %>% `colnames<-` (c("language","iso"))
langs_pud  <- langs_df$language
ISO_pud    <- sapply(langs_pud,function(l) ISO_639_3 %>% filter(Name==l) %>% dplyr::select(1)) %>% unlist() %>% unname()


# functions
read_df <- function(language, collection, strokes=T) {
  iso_language <- if (language %in% langs_pud) ISO_pud[langs_pud == language] else ISO_cvfa
  if (collection == 'pud') {
    str_suffix <- ifelse(strokes == T & iso_language %in% c('zho','jpn'),'_strokes','')
    read.csv(here("pud_data",paste0(iso_language,"_pud",str_suffix,".csv")))
  } else if (collection == 'cvfa') {
    str_suffix <- ifelse(strokes == T & iso_language %in% c('zho','jpn'),'_strokes','')
    read.csv(here("code/common-voice-forced-alignments",paste0(iso_language,"_pud",str_suffix,".csv")))
  }
}

compute_optimality_scores <- function(collection) {
  res <- lapply(langs, function(lang) {
    df         <- read_df(lang,collection)[-1] %>% mutate(rank=1:nrow(.))
    N_types    <- nrow(df)
    p <- df$frequency/sum(df$frequency)
    Lmin  <- sum(sort(df$length)*p)                                                # min baseline
    L     <- sum(df$length*p)                                                      # real value (weight by freq)
    Lrand <- sum(df$length)/N_types                                                # random baseline (unweighted)
    eta   <- Lmin/L
    omega <- (Lrand-L)/(Lrand-Lmin)
    list("language"=lang, "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta,"omega"=omega)
  }) 
  do.call(rbind.data.frame,res)
}
