
library('here')
library('dplyr')
library('ISOcodes')
library('ggplot2')
require("ggrepel")
library('reshape2')
library('xtable')
library(data.table)
library(ggcorrplot)


'%!in%' <- function(x,y)!('%in%'(x,y))


# globals ----------------------------------------------------------------------
COLLS       <- c('pud','cv')
length_defs <- c('characters','medianDuration')   # 'meanDuration'

## pud
langs_df_pud <- read.csv(here("explanatory_tables_with_script/pud.csv")) %>% 
  mutate(family = case_when(language == "Turkish" ~ "Turkic",
                           language == "Japanese" ~ "Japonic",
                           language == "Korean" ~ "Koreanic",
                           language == "Chinese" ~ "Sino-Tibetan",
                           language == "Thai" ~ "Tai-Kadai",
                           language == "Finnish" ~ "Uralic",
                           TRUE ~ "Indo-European")) %>% 
  rbind(list('language' ='Chinese-strokes', 'iso_code' = 'zho', 'dialect' = NA, 
             'X.types' = as.integer(4971), 'X.tokens' = as.integer(17850), 'script'= 'Chinese', 'family' = 'Sino-Tibetan')) %>% 
  rbind(list('language' ='Japanese-strokes', 'iso_code' = 'jpn', 'dialect' = NA, 
             'X.types' = as.integer(4852), 'X.tokens' = as.integer(24737), 'script'= 'Kanji-Kana', 'family' = 'Japonic')) %>% 
  arrange(iso_code)
langs_pud    <- langs_df_pud$language
ISO_pud      <- langs_df_pud$iso_code
labs_pud     <- langs_pud; names(labs_pud) <- ISO_pud

## cv
langs_df_cv <- read.csv(here("explanatory_tables_with_script/common_voice.csv")) %>% filter(iso_code %!in% c('ja','zh')) %>%
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





# functions  -------------------------------------------------------------------
read_language <- function(iso_code, collection, dialect = NULL, pud_strokes = T) {
  # if no dialect is specified and it's unique for the language, it will be inferred
  # specify the desired dialect if this is not unique
  if (collection == 'cv') {
    if (is.null(dialect)) dialect <- dialects_cv[ISO_cv==iso_code]
    dialect  <- ifelse(dialect != '',paste0('-',dialect),'')
    language <- langs_cv[ISO_cv==iso_code]
    read.csv(here("code/common-voice-forced-alignment/",paste0(iso_code,dialect,"-word.csv"))) %>% 
      arrange(desc(repetitions)) %>% filter(orthographic_form %!in% c('','<unk>')) %>% 
      rename(frequency = repetitions, word = orthographic_form) %>% 
      mutate(characters = nchar(word), language = language) 
  } else if (collection == 'pud') {
    str_suffix <- ifelse(pud_strokes == T & iso_code %in% c('zho','jpn'),'_strokes','')
    read.csv(here("data/pud/",paste0(iso_code,"_pud",str_suffix,".csv")))[-1]
  } else print('specify an available collection')
}




compute_corr <- function(collection, corr_type, length = 'characters') {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  cors <- lapply(1:nrow(langs_df), function(i) {
    iso_code <- langs_df$iso_code[i]
    language <- langs_df$language[i]
    family   <- langs_df$family[i]
    script   <- langs_df$script[i]
    print(iso_code)
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    strokes <- ifelse(stringr::str_detect(language,'-strokes'), T, F)
    df <- read_language(iso_code,collection,dialect,strokes) %>% mutate(rank=1:nrow(.))
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$characters
    }
    res  <- cor.test(df$frequency,df$length, method = corr_type, alternative = "less")                     # test statistics
    list("language"=language, "family"=family, "script"=script, "corr"=res$estimate, "pvalue"=res$p.value)
  }) 
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
    mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
  return(df)
}


compute_optimality_scores <- function(collection, corr_type, length_def = 'characters') {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  res <- lapply(1:nrow(langs_df), function(i) {
    iso_code <- langs_df$iso_code[i]
    print(iso_code)
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    df <- read_language(iso_code,collection,dialect)
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length_def == 'meanDuration') df$meanDuration else if (length_def == 'medianDuration') df$medianDuration else df$characters
    }
    N_types    <- nrow(df)
    p          <- df$frequency/sum(df$frequency)
    Lmin       <- sum(sort(df$length)*p)                                                # min baseline
    L          <- sum(df$length*p)                                                      # real value (weight by freq)
    Lrand      <- sum(df$length)/N_types                                                # random baseline (unweighted)
    corr    <- read.csv(here('results',paste0('correlation_',collection,'_',length_def,'_',corr_type,'.csv'))) %>% 
      filter(language == langs_df$language[i]) %>% select(corr) %>% as.numeric()
    corr_min    <- cor.test(df$frequency,sort(df$length), method=corr_type, alternative = "less")$estimate %>% unname()
    # scores:
    eta   <- Lmin/L
    psi   <- (Lrand-L)/(Lrand-Lmin)
    omega <- corr/corr_min 
    list("language"=langs_df$language[i], "family"=langs_df$family[i], "script"=langs_df$script[i], 
         "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta, "psi"=psi, "omega"=omega)
  }) 
  df <- do.call(rbind.data.frame,res) %>% arrange(family,script,language)
  return(df)
}


opt_score_summary <- function(score, corr_type='kendall') {
  corr_suffix <- paste0('_',corr_type)
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      rows_cv <- lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] 
        df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
        df %>% mutate(collection = paste(collection,length_def,sep='-'))
      })
      do.call(rbind,rows_cv)
    } else {
      df <- read.csv(here('results',paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))[-1]  %>%
        filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
      df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
      df %>% mutate(collection = paste(collection,'characters',sep='-'))
    }
  })
  df <- do.call(rbind,rows); setDT(df)
  df <- df[, as.list(summary(score)), by = collection]
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


# PLOT FUNCTIONS 

plot_score_composition <- function(score,opt_df,plot_title, corr_type) {
  if (score == 'psi') {
    L_diff_df <- opt_df %>% select(language,Lmin,L,Lrand,psi) %>% summarise(language,psi,Lmin,`Lrand-L` = Lrand-L, `L-Lmin` = L-Lmin)
    L_diff_df$language <- factor(L_diff_df$language, levels = L_diff_df$language[order(L_diff_df$psi)])
    melted <- melt(L_diff_df, id.vars=c("language","psi")) %>% 
      mutate(masked_psi = ifelse(variable == "Lrand-L",round(psi,2),""))
    melted$alphacol <- ifelse(melted$variable=="Lmin",0,ifelse(melted$variable=="L-Lmin",0.3,1))
    melted$variable <- factor(melted$variable, levels=c("Lrand-L","L-Lmin","Lmin"))
    ggplot(melted,aes(x=language,y=value,fill=variable)) + theme(legend.position = 'bottom') + coord_flip() + 
      labs(y="Length", title=plot_title,subtitle='score composition') +
      geom_bar(stat="identity",aes(alpha=alphacol),color='white') +
      scale_alpha_identity() + scale_fill_manual(values = c("blue","lightblue"),limits = c('L-Lmin','Lrand-L'))
  } else if (score == 'omega') {
    corr_diff_df <- opt_df %>% select(language,omega,corr,corr_min) %>% 
      summarise(language,omega,corr,'corr_min-corr' = corr_min-corr)
    corr_diff_df$language <- factor(corr_diff_df$language, levels = corr_diff_df$language[order(corr_diff_df$omega)])
    melted <- melt(corr_diff_df, id.vars=c("language","omega")) %>% 
      mutate(masked_omega = ifelse(variable == "corr",round(omega,2),""))
    melted$variable <- factor(melted$variable, levels=c("corr_min-corr",'corr'))
    melted$alphacol <- ifelse(melted$variable=="corr",1,0.3)
    ggplot(melted,aes(x=language,y=value,fill=variable))  + theme(legend.position = 'none') +
      geom_bar(stat="identity",aes(alpha=alphacol),color='white') + coord_flip() + 
      labs(y=paste0(corr_type,' correlation'), title=plot_title,subtitle='score composition') +
      scale_alpha_identity() + scale_fill_manual(values = c("blue","lightblue"),limits = c('corr_min-corr', 'corr'))
  }
}

plot_score <- function(score,opt_df,plot_title) {
  opt_df$score <- if (score == 'omega') opt_df$omega else if (score == 'psi') opt_df$psi
  ggplot(opt_df,aes(reorder(language,score),score)) + geom_bar(stat='identity',fill='lightblue') + 
    geom_text(aes(label = round(score,3)),nudge_y = -0.04, size=3) + theme(legend.position = 'bottom') +
    coord_flip() + labs(x='language', y = score, title = plot_title, subtitle = 'score value')
}


plot_timeVSspace <- function(score,corr_type) {
  df_chars <- read.csv(here('results',paste0('optimality_scores_cv_characters',corr_suffix,'.csv')))[-1] 
  df_chars$space <- if (score == 'omega') df_chars$omega else if (score == 'psi') df_chars$psi
  rows_cv <- lapply(c('medianDuration','meanDuration'), function(length_def) {
    suffix       <- paste0("_",length_def)
    df <- read.csv(here('results',paste0('optimality_scores_cv',suffix,corr_suffix,'.csv')))[-1 ]%>% dplyr::select(-Lmin,-L,-Lrand,-eta)
    df$score <- if (score == 'omega') df$omega else if (score == 'psi') df$psi
    df %>% rename(time = score) %>% mutate(time_def = length_def)  
  })
  merged_dfs <- lapply(rows_cv, function(df_time) merge(df_time,df_chars, by = c('language')))
  df <- do.call(rbind.data.frame,merged_dfs) %>% 
    mutate(label = ifelse(abs(time-space)>=0.10,language,''),color = ifelse(abs(time-space)<0.10,'deviation<10%','deviation>=10%'))
  df %>% 
    ggplot(aes(space,time,label = label,color=color)) + geom_point() + theme(legend.position = 'bottom') +
    facet_wrap(~factor(time_def, levels=c('medianDuration','meanDuration'))) + 
    geom_abline(intercept = 0, slope=1, color = 'purple') + geom_text(nudge_x = 0.04,nudge_y = 0.02,size = 3) +
    labs(x=paste0(score,' (characters)'), y = paste0(score,' (duration)')) +
    scale_color_manual(values = c("blue","red"),limits = c('deviation>=10%', 'deviation<10%'))
}


plot_corrplot_params <- function(collection, length_def,corr_type='kendall') {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  parameters <- lapply(1:nrow(langs_df), function(i) {
    iso_code <- langs_df$iso_code[i]
    language <- langs_df$language[i]
    types   <- langs_df$X.types[i]
    tokens   <- langs_df$X.tokens[i]
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    strokes <- ifelse(stringr::str_detect(language,'-strokes'), T, F)
    df <- read_language(iso_code,collection,dialect,strokes) 
    alphabet_size <- unique(unlist(strsplit(df$word, ''))) %>% length()
    list("language"=language, "types"=types, "tokens"=tokens, 'ab_size'=alphabet_size)
  })
  params_df <- do.call(rbind.data.frame,parameters) %>% filter(language %!in% c('Japanese-strokes','Chinese-strokes'))
  opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,'_',length_def,corr_suffix,'.csv'))) %>% 
    select(language,L,eta,psi,omega)
  all_df <- merge(params_df,opt_df, by='language') %>% select(-language)
  cors <- round(cor(all_df,method='kendall'), 2)
  p.mat <- cor_pmat(all_df)
  ggcorrplot(cors, type = "lower", p.mat = p.mat, lab=T, lab_size = 5, tl.cex = 20, pch.cex = 20) + 
    labs(title=paste(collection,length_def,sep='-')) + theme(plot.title = element_text(size=22))
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
