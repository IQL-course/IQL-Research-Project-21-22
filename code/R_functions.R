
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
    #str_suffix <- ifelse(strokes == T & iso_language %in% c('zho','jpn'),'_strokes','')
    #read.csv(here("code/common-voice-forced-alignments",paste0(iso_language,"_pud",str_suffix,".csv")))
  }
}

compute_optimality_scores <- function(collection) {
  langs <- if (collection == 'pud') langs_pud 
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
  return(do.call(rbind.data.frame,res))
}


compute_tau_corr <- function(collection) {
  langs <- if (collection == 'pud') langs_pud
  cors <- lapply(langs, function(lang) {
    df   <- read_df(lang, collection)
    res  <- cor.test(df$frequency,df$length, method="kendall",alternative = "less")                          # test statistics
    list("language"=lang,"tau"=res$estimate,"pvalue"=res$p.value)
  }) 
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%  # Holm-Bonferroni correction
    mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
  return(df)
}

get_ranked_langs <- function(opt_df, metric) {
  if (metric == 'omega') {
    opt_df %>% arrange(desc(omega)) %>% dplyr::select(language) %>% mutate(ranking_omega = 1:nrow(opt_df))
  } else if (metric == 'eta') {
    opt_df %>% arrange(desc(eta))   %>% dplyr::select(language) %>% mutate(ranking_eta   = 1:nrow(opt_df))
  }
}


plotRanks <- function(a, b, labels.offset=0.1, arrow.len=0.1,title) {
  old.par <- par(mar=c(1,1,1,1))
  a <- rev(a)
  b <- rev(b)
  # Find the length of the vectors
  len.1 <- length(a)
  len.2 <- length(b)
  # Plot two columns of equidistant points
  plot(rep(1, len.1), 1:len.1, pch=20, cex=0.8, 
       xlim=c(0, 3), ylim=c(0, max(len.1, len.2)),
       axes=F, xlab="", ylab="",main=title) # Remove axes and labels
  points(rep(2, len.2), 1:len.2, pch=20, cex=0.8)
  # Put labels next to each observation
  text(rep(1-labels.offset, len.1), 1:len.1, a)
  text(rep(2+labels.offset, len.2), 1:len.2, b)
  # Map where the elements of a are in b
  a.to.b <- match(a, b)
  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b, 
         length=arrow.len, angle=20)
  par(old.par)
}
