
library('here')
library('dplyr')
library('ISOcodes')
library('ggplot2')
require("ggrepel")
library('reshape2')
library('xtable')
library(data.table)

# TO DO 
# - get ISO codes of cvfa languages
# - compute Omega for cvfa

'%!in%' <- function(x,y)!('%in%'(x,y))
COLLS       <- c('pud','cv')
length_defs <- c('meanDuration','medianDuration','n_chars')

# globals
## pud
langs_df_pud <- read.csv(here("explanatory_tables/pud.csv"))
langs_pud    <- langs_df_pud$language
ISO_pud      <- langs_df_pud$iso_code
labs_pud <- langs_pud; names(labs_pud) <- ISO_pud

## cv
langs_df_cv <- read.csv(here("explanatory_tables/common_voice.csv")) %>% filter(iso_code %!in% c('ja','zh'))
langs_cv    <- langs_df_cv$language
ISO_cv      <- langs_df_cv$iso_code
dialects_cv <- langs_df_cv$dialect
labs_cv <- langs_cv; names(labs_cv) <- ISO_cv


# functions
read_language <- function(iso_code, collection, dialect = NULL, pud_strokes = T) {
  # if no dialect is specified and it's unique for the language, it will be inferred
  # specify the desired dialect if this is not unique
  if (collection == 'cv') {
    if (is.null(dialect)) dialect <- dialects_cv[ISO_cv==iso_code]
    dialect  <- ifelse(dialect != '',paste0('-',dialect),'')
    read.csv(here("code/common-voice-forced-alignment",paste0(iso_code,dialect,"-word.csv"))) %>% 
      arrange(desc(repetitions)) %>% filter(orthographic_form %!in% c('','<unk>')) %>% 
      rename(frequency = repetitions, word = orthographic_form) %>% 
      mutate(n_chars = nchar(word), language = paste0(iso_code,dialect))
  } else if (collection == 'pud') {
    str_suffix <- ifelse(pud_strokes == T & iso_code %in% c('zho','jpn'),'_strokes','')
    read.csv(here("data/pud/",paste0(iso_code,"_pud",str_suffix,".csv")))[-1]
  } else print('specify an available collection')
}



compute_optimality_scores <- function(collection, length = 'meanDuration') {
  iso_codes <- if (collection == 'pud') ISO_pud else if (collection == 'cv') ISO_cv
  res <- lapply(1:length(iso_codes), function(i) {
    iso_code <- iso_codes[i]
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    df <- read_language(iso_code,collection,dialect) %>% mutate(rank=1:nrow(.))
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$n_chars
    }
    N_types    <- nrow(df)
    p     <- df$frequency/sum(df$frequency)
    Lmin  <- sum(sort(df$length)*p)                                                # min baseline
    L     <- sum(df$length*p)                                                      # real value (weight by freq)
    Lrand <- sum(df$length)/N_types                                                # random baseline (unweighted)
    eta   <- Lmin/L
    omega <- (Lrand-L)/(Lrand-Lmin)
    list("language"=iso_code, "dialect"=dialect, "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta,"omega"=omega)
  }) 
  return(do.call(rbind.data.frame,res))
}



compute_tau_corr <- function(collection, length = 'meanDuration') {
  iso_codes <- if (collection == 'pud') ISO_pud else if (collection == 'cv') ISO_cv
  cors <- lapply(1:length(iso_codes), function(i) {
    iso_code <- iso_codes[i]
    print(iso_code)
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    df <- read_language(iso_code,collection,dialect) %>% mutate(rank=1:nrow(.))
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$n_chars
    }
    res  <- cor.test(df$frequency,df$length, method="kendall",alternative = "less")                     # test statistics
    list("language"=iso_code, "dialect"=dialect, "tau"=res$estimate, "pvalue"=res$p.value)
  }) 
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
    mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
  return(df)
}


assign_stars <- function(df) {
  df %>% mutate(stars = case_when(hb_pvalue<=0.01                  ~ '***',
                                  hb_pvalue>0.01 & hb_pvalue<=0.05 ~ '**',
                                  hb_pvalue>0.05 & hb_pvalue<=0.1  ~ '*',
                                  hb_pvalue>0.1                    ~ 'x'))
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
