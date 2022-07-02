
library('here')
library('dplyr')
library('ISOcodes')
library('ggplot2')
require("ggrepel")
library('reshape2')
library('xtable')
library(data.table)
library(ggcorrplot)
library(pcaPP)
library(parallel)


'%!in%' <- function(x,y)!('%in%'(x,y))


# globals ----------------------------------------------------------------------
COLLS       <- c('pud','cv')
length_defs <- c('characters','medianDuration')   # 'meanDuration'

## pud
langs_df_pud <- read.csv(here("data/descriptive_tables/pud.csv")) %>% 
  mutate(family = case_when(language == "Turkish" ~ "Turkic",
                           language == "Japanese" ~ "Japonic",
                           language == "Korean" ~ "Koreanic",
                           language == "Chinese" ~ "Sino-Tibetan",
                           language == "Thai" ~ "Tai-Kadai",
                           language == "Finnish" ~ "Uralic",
                           TRUE ~ "Indo-European")) %>% 
  mutate(script = case_when(language == "Arabic" ~ "Arabic",
                            language == "Hindi" ~ "Devanagari",
                            language == "Japanese" ~ "Kanji-Kana",
                            language == "Japanese-strokes" ~ "Kanji-Kana",
                            language == "Korean" ~ "Hangul",
                            language == "Russian" ~ "Cyrillic",
                            language == "Thai" ~ "Thai",
                            language == "Chinese" ~ "Chinese",
                            language == "Chinese-strokes" ~ "Chinese",
                            TRUE ~ "Latin")) %>% 
  arrange(iso_code) 
langs_pud    <- langs_df_pud$language
ISO_pud      <- langs_df_pud$iso_code
labs_pud     <- langs_pud; names(labs_pud) <- ISO_pud


## cv
langs_df_cv <- read.csv(here("data/descriptive_tables/common_voice.csv")) %>% filter(iso_code %!in% c('ja','zh')) %>%
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


corr_colors_r = c("blue", "white", "red")
corr_colors_tau = c("#41B85C", "white", "orange")


# SET
corr_type <- 'kendall'
corr_suffix <- paste0('_',corr_type)


# functions  -------------------------------------------------------------------
read_language <- function(iso_code, collection, dialect = NULL, alternative = NULL) {
  # if no dialect is specified and it's unique for the language, it will be inferred
  # specify the desired dialect if this is not unique
  if (collection == 'cv') {
    if (is.null(dialect)) dialect <- dialects_cv[ISO_cv==iso_code]
    dialect  <- ifelse(dialect != '',paste0('-',dialect),'')
    language <- langs_cv[ISO_cv==iso_code]
    read.csv(here("data/cv",paste0(iso_code,dialect,"-word.csv")), encoding = 'UTF-8') %>% 
      arrange(desc(repetitions)) %>% filter(orthographic_form %!in% c('','<unk>')) %>% 
      rename(frequency = repetitions, word = orthographic_form) %>% 
      mutate(characters = nchar(word), language = language) 
  } else if (collection == 'pud') {
    str_suffix <- ifelse (is.null(alternative),'',paste0('_',alternative))
    read.csv(here("data/pud",paste0(iso_code,"_pud",str_suffix,".csv")), encoding = 'UTF-8')[-1]
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
    alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
    df <- read_language(iso_code,collection,dialect,alternative) %>% mutate(rank=1:nrow(.))
    # choose definition of 'length' in cv
    if (collection == 'cv') {
      df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$characters
    }
    res <- cor.test(df$frequency,df$length, method=corr_type, alternative = "less")
    list("language"=language, "family"=family, "script"=script, "corr"=res$estimate, "pvalue"=res$p.value)
  }) 
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
    mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
  return(df)
}



compute_optimality_scores_lang <- function(lang, collection, length_def='characters', corr_type='kendall', sample_tokens=NULL) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  iso_code  <- langs_df$iso_code[langs_df$language==lang]
  dialect   <- langs_df$dialect[langs_df$language==lang]
  family    <- langs_df$family[langs_df$language==lang]
  script    <- langs_df$script[langs_df$language==lang]
  alternative <- if (stringr::str_detect(lang,'-')) sub(".*-","",lang) else NULL
  df <- read_language(iso_code,collection,dialect,alternative)
  # sample
  if (!is.null(sample_tokens)) {
    df <- df[rep(seq_len(nrow(df)), df$frequency),]
    df <- if (sample_tokens<=nrow(df)) {
        set.seed(300)
        sample_n(df,sample_tokens) %>% group_by(word) %>% summarise(frequency = n(),length=length) %>% 
        unique() %>% arrange(desc(frequency))
        } else df %>% mutate(length = NA, frequency=NA)
  }
  # choose definition of 'length' in cv
  if (collection == 'cv') {
    df$length <- if (length_def == 'meanDuration') df$meanDuration else if (length_def == 'medianDuration') df$medianDuration else df$characters
  }
  N_types    <- nrow(df)
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)                                                # min baseline
  L          <- sum(df$length*p)                                                      # real value (weight by freq)
  Lrand      <- sum(df$length)/N_types                                                # random baseline (unweighted)
  corr <- if (!is.null(sample_tokens)) {
    if (!is.na(df$frequency[1])) {
      cor.test(df$frequency,df$length, method=corr_type, alternative = "less")$estimate %>% unname()
    } else NA
  } else {
    read.csv(here('results',paste0('correlation_',collection,'_',length_def,'_',corr_type,'.csv'))) %>% 
      filter(language == lang) %>% select(corr) %>% as.numeric()
  }
  corr_min  <- if (!is.na(df$frequency[1])) { 
    cor.test(df$frequency,sort(df$length), method=corr_type, alternative = "less")$estimate %>% unname()
  } else NA
  # scores:
  eta   <- Lmin/L
  psi   <- (Lrand-L)/(Lrand-Lmin)
  omega <- corr/corr_min 
  data.frame("language"=lang, "family"=family, "script"=script, 
       "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta, "psi"=psi, "omega"=omega)
}


compute_optimality_scores_coll <- function(collection, corr_type='kendall', length_def = 'characters',sample_tokens = NULL) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  res <- lapply(1:nrow(langs_df), function(i) {
    language <- langs_df$language[i]
    print(language)
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    compute_optimality_scores_lang(language,collection,length_def,corr_type,sample_tokens)
  })
  df <- do.call(rbind.data.frame,res) %>% arrange(family,script,language)
  return(df)
}



compute_expectation_scores_lang <- function(lang, collection, length_def='characters', n_experiments = 10^2) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  iso_code  <- langs_df$iso_code[langs_df$language==lang]
  dialect   <- langs_df$dialect[langs_df$language==lang]
  alternative <- if (stringr::str_detect(lang,'-')) sub(".*-","",lang) else NULL
  df <- read_language(iso_code,collection,dialect,alternative)
  # choose definition of 'length' in cv
  if (collection == 'cv') {
    df$length <- if (length_def == 'meanDuration') df$meanDuration 
    else if (length_def == 'medianDuration') df$medianDuration 
    else df$characters
  }
  N_types    <- nrow(df)
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)                                                # min baseline
  Lrand      <- sum(df$length)/N_types    # random baseline (unweighted)
  corr_min   <- cor.fk(df$frequency, sort(df$length))
  
  set.seed(23)
  scores <- lapply(1:n_experiments, function(i) {
    length     <- sample(df$length)                                 # shuffle length, each time different
    L          <- sum(length*p)                                       # real value (weight by freq)
    corr       <- cor.fk(df$frequency, length)
    # scores:
    eta   <- Lmin/L
    psi   <- (Lrand-L)/(Lrand-Lmin)
    omega <- corr/corr_min 
    list('L'=L,'eta'=eta,'psi'=psi,'omega'=omega)
  })
  
  sums <- sapply(1:length(scores[[1]]), function(score_index) {
    score <- names(scores[[1]])[score_index]
    value <- do.call(c,lapply(scores, `[[`, score_index)) %>% sum()
    names(value) <- score
    value
  })
  averages <- sums/n_experiments
  data.frame("language"=lang,
             "Lmin"=Lmin, "L"=averages[1], "Lrand"=Lrand, "eta"=averages[2], "psi"=averages[3], "omega"=averages[4])
}


null_hyp_job_cv <- function(job_index,iters,cores) {
  collection <- 'cv'
  big_langs <- c('German','English','Kinyarwanda')
  length_def <- ifelse(job_index %in% c(1,2), 'medianDuration','characters')
  langs      <- if (job_index %in% c(1,3)) big_langs else langs_df_cv$language[langs_df_cv$language %!in% big_langs]
  suffix <- paste0("_",length_def)
  scores <- mclapply(langs, function(language) {
    compute_expectation_scores_lang(language,collection,length_def,n_experiments = iters) 
  }, mc.cores = cores)
  null_df <- do.call(rbind.data.frame,scores)
  write.csv(null_df, here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,'_',job_index,'.csv')))
}


opt_score_summary <- function(score, corr_type='kendall',null=F, iters = 1000) {
  corr_suffix <- paste0('_',corr_type)
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      rows_cv <- lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- if (null == F) read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] 
              else read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
        df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
        df %>% mutate(collection = paste(collection,length_def,sep='-'))
      })
      do.call(rbind,rows_cv)
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      df <- if (null == F) read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] 
            else read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
      df <- df %>% filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
      df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
      df %>% mutate(collection = paste(collection,'characters',sep='-'))
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


HB_correction <- function(p.mat) {
  # find lower diagonal values
  ind <- which( lower.tri(p.mat,diag=F) , arr.ind = TRUE )
  p_vals <- data.frame( col = dimnames(p.mat)[[2]][ind[,2]] ,
                        row = dimnames(p.mat)[[1]][ind[,1]] ,
                        pvalue = p.mat[ ind ] ) %>% arrange(pvalue) %>% 
    mutate(index=1:nrow(.)) %>% mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)  
  for (i in 1:nrow(p_vals)) p.mat[p_vals$row[i], p_vals$col[i]] <- p_vals$hb_pvalue[i] 
  return(p.mat)
}

get_langs_params <- function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  parameters <- lapply(1:nrow(langs_df), function(i) {
    iso_code <- langs_df$iso_code[i]
    language <- langs_df$language[i]
    types   <- langs_df$X.types[i]
    tokens   <- langs_df$X.tokens[i]
    dialect  <- ifelse(collection=='pud','',dialects_cv[i])
    alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
    df <- read_language(iso_code,collection,dialect,alternative) 
    words <- if (is.null(alternative)) df$word else if (alternative == 'strokes') df$word else tolower(df$romanized_form)
    alphabet_size <- unique(unlist(strsplit(words, ''))) %>% length()
    list("language"=language, "types"=types, "tokens"=tokens, 'ab_size'=alphabet_size)
  })
  return(do.call(rbind.data.frame,parameters))
}


add_corr_min <- function(opt_df,suffix,corr_suffix) {
  corr_df <- read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]      # to remove if add tau and tau_min before
  merge(opt_df,corr_df, by = c('language','family','script')) %>%                         # to remove if add tau and tau_min before
    select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)  # to remove if add tau and tau_min before
}






# PLOT FUNCTIONS 

plot_score_composition <- function(score,opt_df, corr_type) {
  if (score == 'psi') {
    L_diff_df <- opt_df %>% select(language,Lmin,L,Lrand,psi) %>% summarise(language,psi,Lmin,`Lrand-L` = Lrand-L, `L-Lmin` = L-Lmin)
    L_diff_df$language <- factor(L_diff_df$language, levels = L_diff_df$language[order(L_diff_df$psi)])
    melted <- melt(L_diff_df, id.vars=c("language","psi")) %>% 
      mutate(masked_psi = ifelse(variable == "Lrand-L",round(psi,2),""))
    melted$alphacol <- ifelse(melted$variable=="Lmin",0,ifelse(melted$variable=="L-Lmin",0.3,1))
    melted$variable <- factor(melted$variable, levels=c("Lrand-L","L-Lmin","Lmin"))
    ggplot(melted,aes(x=language,y=value,fill=variable)) + coord_flip() + 
      theme(legend.position = 'right',axis.title.y = element_blank(),axis.text.y = element_blank()) +
      labs(y="Length", title ='score composition') +
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
      labs(y=paste0(corr_type,' correlation'), title = 'score composition') +
      scale_alpha_identity() + scale_fill_manual(values = c("blue","lightblue"),limits = c('corr_min-corr', 'corr'))
  }
}

plot_score <- function(score,opt_df) {
  opt_df$score <- if (score == 'omega') opt_df$omega else if (score == 'psi') opt_df$psi
  ggplot(opt_df,aes(reorder(language,score),score)) + geom_bar(stat='identity',fill='lightblue') + 
    geom_text(aes(label = round(score,3)),nudge_y = -0.04, size=3) + theme(legend.position = 'bottom') +
    coord_flip() + labs(x='language', y = score, title = 'score value')
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
    geom_abline(intercept = 0, slope=1, color = 'purple') + geom_text_repel(size = 3) + 
    labs(x=paste0(score,' (characters)'), y = paste0(score,' (duration)')) +
    scale_color_manual(values = c("blue","red"),limits = c('deviation>=10%', 'deviation<10%'))
}


plotRanks <- function(a, b, title, labels.offset=0.1, arrow.len=0.1) {
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
  text(rep(1-labels.offset, len.1), 1:len.1, a, cex=0.7)
  text(rep(2+labels.offset, len.2), 1:len.2, b, cex=0.7)
  # Map where the elements of a are in b
  a.to.b <- match(a, b)
  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b, 
         length=arrow.len, angle=20, col =  ifelse(abs(1:length(a)-a.to.b) >10,'red','black'))
  par(old.par)
}


# CORRELOGRAMS

plot_correlogram <- function(df,plot_corr,type,HB_correct=T,lab_size,tl.cex,pch.cex) {
  if ('Lrand' %in% colnames(df)) df <- df %>% rename(Lr = Lrand)
  if (type == 'null')   df <- df %>% mutate(`Lmin/Lr`=Lmin/Lr) %>% dplyr::select(Lmin,Lr,`Lmin/Lr`,eta,psi,omega)
  
  title        <- switch(type, 'scores'='Relations among scores',
                        'params'='Relations with parameters', 'null'='Under null hypothesis')
  greek_names  <- switch(type,
                         'scores'=c('L','E[\u03B7]','E[\u03A8]','E[\u03A9]'),
                          'params'=c('n. types', 'n. tokens', 'alphabet', 'E[\u03B7]','E[\u03A8]','E[\u03A9]'), 
                         'null'=c(bquote(~L[min]),'L',bquote(~L[r]),'E[\u03B7]','E[\u03A8]','E[\u03A9]'))
  legend_title <-  switch(plot_corr, 'kendall' = '\u03C4 corr', 'pearson'='r corr')
  
  cors  <- round(cor(df, method=plot_corr), 2)
  p.mat <- cor_pmat(df, method=plot_corr)
  if (HB_correct) p.mat <- HB_correction(p.mat)
  ggcorrplot(cors, type = "lower", p.mat = p.mat,lab=T, lab_size = lab_size, tl.cex = tl.cex, pch.cex = pch.cex, 
             colors = switch(plot_corr, 'kendall'=corr_colors_tau, 'pearson'=corr_colors_r),
             legend.title = legend_title) + 
    labs(title = title) + 
    scale_x_discrete(labels = greek_names[-1]) + 
    scale_y_discrete(labels = greek_names[-length(greek_names)]) + 
    theme(plot.title = element_text(size=20))
}

