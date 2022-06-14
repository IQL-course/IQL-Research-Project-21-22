
library('here')
library('dplyr')
library('ISOcodes')
library('ggplot2')
require("ggrepel")
library('reshape2')
library('xtable')
library(data.table)
library(ggcorrplot)


# TO DO 
# - get ISO codes of cvfa languages
# - compute Omega for cvfa

'%!in%' <- function(x,y)!('%in%'(x,y))


# globals
cutoff      <- 0.25
COLLS       <- c('pud','cv')
length_defs <- c('meanDuration','medianDuration','n_chars')

## pud
langs_df_pud <- read.csv(here("explanatory_tables/pud.csv"))
langs_pud    <- langs_df_pud$language
ISO_pud      <- langs_df_pud$iso_code
labs_pud <- langs_pud; names(labs_pud) <- ISO_pud

## cv
langs_df_cv <- read.csv(here("explanatory_tables/common_voice.csv")) %>% filter(iso_code %!in% c('ja','zh')) %>%
  rows_update(tibble(language = "Interlingua", iso_code = 'ia'), by = "iso_code") %>%
  rows_update(tibble(language = "Oriya", iso_code = 'or'), by = "iso_code") %>% 
  rows_update(tibble(language = "Modern Greek", iso_code = 'el'), by = "iso_code") %>% 
  filter(dialect != 'vallader') %>%
  rows_update(tibble(X.types = 9801, iso_code = 'rm'), by = "iso_code") %>% 
  rows_update(tibble(X.types = 44192, iso_code = 'rm'), by = "iso_code") %>% 
  rows_update(tibble(dialect = '', iso_code = 'rm'), by = "iso_code")

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
    language <- langs_cv[ISO_cv==iso_code]
    read.csv(here("code/common-voice-forced-alignment",paste0(iso_code,dialect,"-word.csv"))) %>% 
      arrange(desc(repetitions)) %>% filter(orthographic_form %!in% c('','<unk>')) %>% 
      rename(frequency = repetitions, word = orthographic_form) %>% 
      mutate(n_chars = nchar(word), language = language) %>%
      mutate(coeff_var = meanDuration/stDevDuration)
  } else if (collection == 'pud') {
    str_suffix <- ifelse(pud_strokes == T & iso_code %in% c('zho','jpn'),'_strokes','')
    read.csv(here("data/pud/",paste0(iso_code,"_pud",str_suffix,".csv")))[-1]
  } else print('specify an available collection')
}



compute_optimality_scores <- function(collection, length = 'meanDuration') {
  iso_codes <- if (collection == 'pud') ISO_pud else if (collection == 'cv') ISO_cv
  res <- lapply(1:length(iso_codes), function(i) {
    iso_code <- iso_codes[i]
    language <- langs_cv[i]
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    df <- read_language(iso_code,collection,dialect) %>% mutate(rank=1:nrow(.))
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$n_chars
    }
    N_types    <- nrow(df)
    p          <- df$frequency/sum(df$frequency)
    Lmin       <- sum(sort(df$length)*p)                                                # min baseline
    L          <- sum(df$length*p)                                                      # real value (weight by freq)
    Lrand      <- sum(df$length)/N_types                                                # random baseline (unweighted)
    eta        <- Lmin/L
    omega      <- (Lrand-L)/(Lrand-Lmin)
    list("language"=language, "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta,"omega"=omega)
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
    list("language"=language, "tau"=res$estimate, "pvalue"=res$p.value)
  }) 
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
    mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
  return(df)
}


opt_score_summary <- function(score) {
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      rows_cv <- lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,'.csv')))[-1]
        df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta
        df %>% mutate(collection = paste(collection,length_def,sep='-'))
      })
      do.call(rbind,rows_cv)
    } else {
      df <- read.csv(here('results',paste0('optimality_scores_',collection,'.csv')))[-1]
      df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta
      df %>% mutate(collection = paste(collection,'n_chars',sep='-'))
    }
  })
  df <- do.call(rbind,rows); setDT(df)
  df[, as.list(summary(score)), by = collection]
}


assign_stars <- function(df) {
  df %>% mutate(stars = case_when(hb_pvalue<=0.01                  ~ '***',
                                  hb_pvalue>0.01 & hb_pvalue<=0.05 ~ '**',
                                  hb_pvalue>0.05 & hb_pvalue<=0.1  ~ '*',
                                  hb_pvalue>0.1                    ~ 'x'))
}


remove_at_cutoff <- function(cutoff) {
  df <- read.csv(here('results','coefficient_variation.csv'))
  df_retained <- df %>% group_by(language) %>% mutate(total_lang = n()) %>% 
    filter(coeff_var<=cutoff) %>% summarise(retained = n()/total_lang) %>% unique()
  ggplot(df_retained) + geom_bar(aes(language,retained,fill=retained),stat='identity') + 
    labs(title=paste0('Cutoff: ',cutoff)) + coord_flip() + 
    geom_hline(yintercept = min(df_retained$retained),color='red',linetype='dashed')
}



get_ranked_langs <- function(opt_df, metric) {
  if (metric == 'omega') {
    opt_df %>% arrange(desc(omega)) %>% dplyr::select(language) %>% mutate(ranking_omega = 1:nrow(opt_df))
  } else if (metric == 'eta') {
    opt_df %>% arrange(desc(eta))   %>% dplyr::select(language) %>% mutate(ranking_eta   = 1:nrow(opt_df))
  }
}


# PLOT FUNCTIONS 

plot_omega_composition <- function(opt_df,plot_title) {
  L_diff_df <- opt_df %>% select(language,Lmin,L,Lrand,omega) %>% summarise(language,omega,Lmin,`Lrand-L` = Lrand-L, `L-Lmin` = L-Lmin)
  L_diff_df$language <- factor(L_diff_df$language, levels = L_diff_df$language[order(L_diff_df$omega)])
  melted <- melt(L_diff_df, id.vars=c("language","omega")) %>% 
    mutate(masked_omega = ifelse(variable == "Lrand-L",round(omega,2),""))
  melted$alphacol <- ifelse(melted$variable=="Lmin",0,1)
  melted$variable <- factor(melted$variable, levels=c("Lrand-L","L-Lmin","Lmin"))
  ggplot(melted,aes(x=language,y=value,fill=variable)) + 
    geom_bar(stat="identity",aes(alpha=alphacol)) + labs(y="difference",title=plot_title) +
    geom_text(aes(label = masked_omega, color="omega"),position = position_stack(vjust = .5)) + coord_flip() +
    scale_alpha_identity()+ scale_fill_manual(values = c("#6EE1A4","#E16E9C","blue"),limits = c('Lrand-L', 'L-Lmin'))
}

plot_omega <- function(opt_df,plot_title) {
  ggplot(opt_df) + geom_bar(aes(reorder(language,omega),omega),stat='identity',fill='lightblue') + 
    coord_flip() + labs(x='language', title = plot_title)
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
