
library('dplyr')
library("ISOcodes")
library("ggplot2")
require("ggrepel")
library("reshape2")
library("xtable")
library("data.table")
library("ggcorrplot")
library("pcaPP")
library("parallel")
library("DescTools")
library("ggpmisc")
library("Ckmeans.1d.dp")
library("psych") 
library('latex2exp')
library('mblm')
options(dplyr.summarise.inform = FALSE)

# preliminary functions
'%!in%' <- function(x,y)!('%in%'(x,y))
which_folder <- function(folder,filter=T) paste(folder,ifelse(filter,'filtered','non_filtered'),sep='/')


# globals ----------------------------------------------------------------------
corr_type <- 'kendall'
COLLS       <- c('pud','cv')
length_defs <- c('characters','medianDuration')
exp_scores_labs <- c('E[eta]'='E[\u03B7]','E[psi]'='E[\u03A8]','E[omega]'='E[\u03A9]')
length_labs <- c('medianDuration'='duration','characters'='characters')
scores_labs <- c('n', 'T', 'A','L','\u03B7','\u03A8','\u03A9',
                 bquote(~L[min]),bquote(~L[r]),bquote(~L[min]/L[r]),
                 'E[\u03B7]','E[\u03A8]','E[\u03A9]')
names(scores_labs) <- c('n', 'T', 'A','L','eta','psi','omega',
                        'Lmin','Lr','Lmin/Lr',
                        'E[eta]','E[psi]','E[omega]')
corr_colors_r = c("blue", "white", "red")
corr_colors_tau = c("#41B85C", "white", "orange")
non_imm_langs <- c('Chinese-strokes',"Japanese-strokes",'Chinese-pinyin','Japanese-romaji')
corr_suffix   <- if (corr_type=='kendall') '' else paste0('_',corr_type)





# functions  -------------------------------------------------------------------

shorten_names <- function(df) {
  df %>% rows_update(tibble(language = "Interlingua", iso_code = 'ina'), by = "iso_code") %>% 
    rows_update(tibble(language = "Modern Greek", iso_code = 'ell'), by = "iso_code") %>% 
    rows_update(tibble(language = "Oriya", iso_code = 'ori'), by = "iso_code")
}

do_remove_vowels <- function(iso_code,words) {
  if(iso_code=='fin'|iso_code=='isl'|iso_code=='fra'|iso_code=='pol'|iso_code=='ces'){
    gsub("[aeiouáóéíúàèùìòâôîêûyýäöæą\U0105ę\U0119ů\U016F]","", words)          # with y ý
  } else if(iso_code=='zho'){
    gsub("[aeiouā\U0101áǎ\U01CEàō\u014dóǒ\u01d2òē\U0113éě\U011Bè
         ī\u012bíǐ\u01d0ìū\u016búǔ\u01d4ùǖǘ\U01D8ǚ\U01DAǜ\U01DCü]","",words)                     
  } else {
    gsub("[AEUIOaeiouàèùìòâôîêûäöüåı]","",words)                     # no y
  }
}

form_table <- function(score,filter){
  cat(score,"==============")
  df_remove   <- read.csv(paste0(which_folder('results',filter),'/optimality_scores_pud_remove_vowels.csv'))
  df_noremove <- read.csv(paste0(which_folder('results',filter),'/optimality_scores_pud_characters.csv'))
  df <- merge(df_noremove[c("language",score)], 
              df_remove[c("language",score)], by="language") %>% 
    mutate(class=score) 
  colnames(df) <- c("language","x","y","class")
  cat("\n", score, "linear regression")
  cat("\nstd. error:", summary(lm(y~x,df))$coefficients[2, 2])
  cat("\nR^2:", summary(lm(y~x,df))$r.squared)
  cat("\n", score, "pearson correlation:",round(cor(df$x, df$y),3))
  cat("\n", score, "pearson correlation p-value:",cor.test(df$x,df$y)$p.value,"\n")
  df
}

read_language <- function(language, collection, remove_vowels=FALSE, filtered=TRUE) {
  folder <- if (filtered==T) 'data/filtered/corpora/' else 'data/non_filtered/corpora/'
  if(!remove_vowels){
    if (collection == 'cv') {
      iso_code <- langs_df_cv$iso_code[langs_df_cv$language==language]
      read.csv(paste0(folder,collection,'/',iso_code,"-word.csv"), encoding = 'UTF-8') %>% 
        arrange(desc(frequency)) 
    } else if (collection == 'pud') {
      iso_code <- langs_df_pud$iso_code[langs_df_pud$language==language]
      alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
      str_suffix <- ifelse (is.null(alternative),'',paste0('_',alternative))
      read.csv(paste0(folder,collection,'/',iso_code,"_pud",str_suffix,".csv"), encoding = 'UTF-8') %>% 
        arrange(desc(frequency)) 
    } else print('specify an available collection')
  }
  else {
    if(langs_df_pud$script[langs_df_pud$language==language]=='Latin'){
      iso_code    <- langs_df_pud$iso_code[langs_df_pud$language==language]
      alternative <- if(iso_code=='zho') "pinyin" else if(iso_code=='jpn') "romaji" else NULL   # file suffix
      str_suffix  <- ifelse (is.null(alternative),'',paste0('_',alternative))
      df          <- read.csv(paste0(folder,collection,'/',iso_code,"_pud",str_suffix,".csv"), encoding = 'UTF-8')[-1]
      df$word     <- if(iso_code=='zho' | iso_code=='jpn') df$romanized_form else df$word      # word <- Latin script
      # remove vowels
      df$word   <- do_remove_vowels(iso_code,df$word)
      df$length <- nchar(df$word)   
      cat("number of length 0:",length(which(df$length==0)),
          "\nrows:", which(df$length==0), "\n")
      df
    } else print("Please specify a language of latin script")
  }
}


read_file <- function(what, collection, filter=T, length='characters', corr_type='kendall', iters=1e+06) {
  file <- if (what %in% c('corr','opt')) 'scores_' else if (what=='null') 'null_hypothesis_'
  length_suffix <- ifelse(what=='null',paste0('_',length),'')
  iters_suffix  <- ifelse(what=='null',paste0('_',iters),'')
  df <- read.csv(paste0(which_folder('results',filter),'/',file,collection,length_suffix,iters_suffix,'.csv'))[-1]
  # for correlation files
  if (what=='corr' & corr_type == 'kendall') { 
    df <- select(df, language, tau, tau_pval, length_def) %>% rename(corr = tau, pvalue = tau_pval) 
  } else if (what=='corr' & corr_type == 'pearson') { 
    df <- select(df, language, r,   r_pval,   length_def) %>% rename(corr = r,   pvalue = r_pval)  }
  return(df)
}


compute_corr <- function(collection,corr_type='kendall',length = 'characters', remove_vowels=FALSE, filter=T) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  languages <- if (remove_vowels==T & collection == 'pud') langs_df$language[langs_df$script=='Latin'] else langs_df$language
  cors <- lapply(languages, function(language) {
    print(language)
    df <- read_language(language,collection,remove_vowels,filter) %>% mutate(rank=1:nrow(.))
    # definition of length
    df$length <- if (collection == 'cv') { 
      switch(length, 'meanDuration'=df$meanDuration,'medianDuration'=df$medianDuration,'characters'=df$n_characters)
    } else if (collection == 'pud') df$n_characters
    # compute corr and pvalue
    res <- cor.test(df$frequency,df$length, method=corr_type, alternative = "less")
    # compute minimum correlation
    corr_min  <- if (corr_type=='kendall') { cor.fk(df$frequency, sort(df$length))
    } else { cor.test(df$frequency,sort(df$length), method=corr_type, alternative = "less")$estimate %>% unname()}
    
    list("language"=language, "corr"=res$estimate, "corr_min"=corr_min, "pvalue"=res$p.value)
  })
  df <- do.call(rbind.data.frame,cors) %>% 
    arrange(pvalue) %>% mutate(pvalue = p.adjust(pvalue))   
  return(df)
}



compute_optimality_scores_lang <- function(df, corr_df_lang, lang) {

  # compute values
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)                                                # min baseline
  L          <- sum(df$length*p)                                                      # real value (weight by freq)
  Lrand      <- sum(df$length)/nrow(df)                                                # random baseline (unweighted)
  
  # correlations
  corr     <- corr_df_lang$corr
  corr_min <- corr_df_lang$corr_min
  
  # scores
  if ((Lmin > 0) & (Lrand > Lmin) & (corr_min!=0)){
    eta   <- Lmin/L
    psi   <- (Lrand-L)/(Lrand-Lmin)
    omega <- corr/corr_min
    return(data.frame("language"=lang, "Lmin"=Lmin, "L"=L, "Lrand"=Lrand, "eta"=eta, "psi"=psi, "omega"=omega))
  } else if (Lmin <= 0){
    # error 1: Lmin<=0
    stop(" L_min <= 0 ")
  } else if (Lrand <= Lmin){
    # error 2: L_rand <= L_min
    stop(" L_rand <= L_min ")
  } else if (corr_min == 0){
    # error 3: corr_min == 0
    stop("corr_min equals to 0 ")
  }
}


# NEW: iterate over corr_df
compute_optimality_scores_coll <- function(corrs_df, collection, length_def='characters',remove_vowels=F, filter=T) {
  res <- mclapply(corrs_df$language, function(lang) {
    df <- read_language(lang,collection,remove_vowels,filter)
    # definition of length
    df$length <- if (collection == 'cv') { 
      switch(length_def, 'meanDuration'=df$meanDuration,'medianDuration'=df$medianDuration,'characters'=df$n_characters)
    } else if (collection == 'pud') df$n_characters
    # correlations
    corr_df_lang <- filter(corrs_df, language==lang)
    # compute scores
    compute_optimality_scores_lang(df, corr_df_lang, lang)
  }, mc.cores=3)
  df <- do.call(rbind.data.frame,res)
  return(df)
}

compute_convergence_scores_lang <- function(df_all,lang, n_sample, n_experiments = 10^2) {
  set.seed(962)
  if (n_sample<=nrow(df_all)) {
  scores <- lapply(1:n_experiments, function(i) {
    # sample
    df <- sample_n(df_all,n_sample) %>% group_by(word) %>% summarise(frequency = n(),length) %>% 
        unique() %>% arrange(desc(frequency))
    N_types    <- nrow(df)
    p          <- df$frequency/sum(df$frequency)
    Lmin       <- sum(sort(df$length)*p)                                               
    Lrand      <- sum(df$length)/N_types    
    L          <- sum(df$length*p)      
    corr       <- if (!is.na(df$frequency[1])) cor.fk(df$frequency, df$length) else NA
    corr_min   <- if (!is.na(df$frequency[1])) cor.fk(df$frequency, sort(df$length)) else NA
    # scores:
    eta   <- Lmin/L
    psi   <- (Lrand-L)/(Lrand-Lmin)
    omega <- corr/corr_min 
    list('eta'=eta,'psi'=psi,'omega'=omega)
  })
  } else scores <- list('eta'=NA,'psi'=NA,'omega'=NA)
  
  averages <- sapply(1:length(scores[[1]]), function(score_index) {
    scores_list   <- sapply(scores, `[[`, score_index)
    scores_sum <- sum(scores_list[!is.na(scores_list)])
    scores_num <- sum(!is.na(scores_list))
    scores_sum/scores_num
  })
  data.frame("language"=lang, "eta"=averages[1], "psi"=averages[2], "omega"=averages[3], 't'=n_sample)
}


scores_convergence <- function(collection,length_def='characters',sample_sizes,n_experiments,filter) {

  languages <- if (collection=='pud') langs_df_pud$language else langs_df_cv$language 
  scores <- mclapply(languages, function(lang) {
    print(lang)
    df <- read_language(lang,collection,F,filter)
    # definition of length
    df$length <- if (collection == 'cv') { 
      switch(length_def, 'meanDuration'=df$meanDuration,'medianDuration'=df$medianDuration,'characters'=df$n_characters)
    } else if (collection == 'pud') df$n_characters
    df <- df %>% select(word,frequency,length)
    df_all <- df[rep(seq_len(nrow(df)), df$frequency),]
    lang_scores <- lapply(sample_sizes, function(n_sample) {
      set.seed(19)
      compute_convergence_scores_lang(df_all,lang,n_sample,n_experiments)
    })
    do.call(rbind.data.frame,lang_scores)
  },mc.cores=3)
  
  do.call(rbind.data.frame,scores)
}

run_convergence <- function(collection,length_def,sample_sizes,n_experiments,filter) {
  suffix <- paste0("_",length_def)
  print(length_def)
  start <- Sys.time()
  print(start)
  scores_df <- scores_convergence(collection,length_def,sample_sizes,n_experiments,filter)
  end <- Sys.time()
  print(end)
  print(paste0('started at:',start, '- ended at:',end))
  write.csv(scores_df,paste0(which_folder('results',filter),'/scores_convergence_',collection,suffix,'_',n_experiments,'.csv'))
}

run_null <- function(length,collection,randomizations,filter,cores) {
  suffix <- paste0("_",length)
  print(length)
  print(Sys.time())
  scores <- mclapply(langs_df_cv$language, function(language) {
    compute_expectation_scores_lang(language,collection,length,randomizations,filter) 
  },mc.cores=cores)
  print(Sys.time())
  null_df <- do.call(rbind.data.frame,scores)
  write.csv(null_df, paste0(which_folder('results',filter),'/null_hypothesis_',collection,suffix,'_',randomizations,'.csv'))
}


compute_expectation_scores_lang <- function(lang,collection,length_def='characters', n_experiments = 10^2,filter=T) {
  df <- read_language(lang,collection,F,filter)
  # choose definition of 'length' in cv
  df$length <- if (collection == 'cv') { 
    switch(length_def, 'meanDuration'=df$meanDuration,'medianDuration'=df$medianDuration,'characters'=df$n_characters)
  } else if (collection == 'pud') df$n_characters
  N_types    <- nrow(df)
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)            # min baseline
  Lrand      <- sum(df$length)/N_types            # random baseline (unweighted)
  corr_min   <- cor.fk(df$frequency, sort(df$length))
  
  set.seed(962)
  scores <- lapply(1:n_experiments, function(i) {
    length     <- sample(df$length)     # shuffle length
    L          <- sum(length*p)         # real value 
    corr       <- cor.fk(df$frequency,length)
    # scores:
    eta   <- Lmin/L
    psi   <- (Lrand-L)/(Lrand-Lmin)
    list('L'=L,'eta'=eta,'psi'=psi,'corr'=corr)
  })
  sums <- sapply(1:length(scores[[1]]), function(score_index) {
    value <- do.call(c,lapply(scores, `[[`, score_index)) %>% sum()
  })
  averages <- sums/n_experiments
  
  data.frame("language"=lang,"Lmin"=Lmin, "L"=averages[1], "Lrand"=Lrand, 
             "eta"=averages[2], "psi"=averages[3], "omega"=averages[4]/corr_min)
}



opt_score_summary <- function(score, null=F, iters = 1000) {
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      rows_cv <- lapply(length_defs, function(length_def) {
        df <- if (null == F) read_file('opt',collection,filter,length_def) 
              else read_file('null',collection,filter,length_def,iters)  
        df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
        length_def <- ifelse(length_def=='medianDuration','duration',length_def)
        df %>% mutate(collection = paste(toupper(collection),length_def,sep='-'))
      })
      do.call(rbind,rows_cv)
    } else {
      length_def <- 'characters'
      df <- if (null == F) read_file('opt',collection,filter,length_def) 
            else read_file('null',collection,filter,length_def,iters) 
      df <- df %>% filter(language %!in% non_imm_langs)
      df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
      df %>% mutate(collection = paste(toupper(collection),'characters',sep='-'))
    }
  })
  df <- do.call(rbind,rows); setDT(df)
  df[, as.list(summary(score)), by = collection] %>% cbind('sd'=df[, as.list(sd(score)), by = collection]$V1)
}


symmetrise_mat <- function(p.mat) {
  p.mat[lower.tri(p.mat)] <- t(p.mat)[lower.tri(p.mat)]
  return(p.mat)
}

add_corr_min <- function(opt_df,collection,suffix,corr_suffix,filter) {
  corr_df <- read.csv(paste0(which_folder('results',filter),'/correlation_',collection,suffix,corr_suffix,'.csv'))[-1] 
  merge(opt_df,corr_df, by = c('language','family','script')) %>%                         
    select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language) 
}

get_filtered_ori_df <- function(collection,length_def) {
  dfs <- lapply(c(T,F), function(filtering) {
    dfs <- lapply(c('psi','omega'), function(score) {
      df <- read.csv(paste0(which_folder('results',filtering),'/optimality_scores_',collection,'_',length_def,corr_suffix,'.csv'))[-1]    
      df <- if (score == 'psi') select(df,language,psi) %>% mutate(score = 'psi') else select(df,language,omega) %>% mutate(score = 'omega')
      if (score == 'psi') rename(df,value=psi)  else rename(df,value=omega)
    })
    df <- do.call(rbind.data.frame,dfs)
    if (filtering == F) rename(df, original=value ) else rename(df, filtered=value )
  })
  merge(dfs[[1]],dfs[[2]],by=c('language','score')) %>% 
    mutate(score = factor(score,levels=c('psi','omega')))
}




# PLOT FUNCTIONS 

plot_score_composition <- function(score,opt_df) {
  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  L_diff_df <- opt_df %>% 
    select(language,Lmin,L,Lrand,psi) %>% 
    summarise(language,psi,Lmin,`Lrand-L` = Lrand-L, `L-Lmin` = L-Lmin)
  L_diff_df$language <- factor(L_diff_df$language, levels = L_diff_df$language[order(L_diff_df$psi)])
  melted <- reshape2::melt(L_diff_df, id.vars=c("language","psi")) %>% 
    mutate(masked_psi = ifelse(variable == "Lrand-L",round(psi,2),""))
  melted$alphacol <- ifelse(melted$variable=="Lmin",0,0.6)
  melted$variable <- factor(melted$variable, levels=c("Lrand-L","L-Lmin","Lmin"))
  
  ggplot(melted,aes(x=language,y=value,fill=variable)) + coord_flip() + 
    theme(legend.position = 'right',axis.title.y = element_blank(),axis.text.y = element_blank(),
          legend.key = element_rect(fill = "#edeff2"), legend.text.align = 0) +
    labs(x=score_latex, y="Length") + 
    guides(fill=guide_legend(title="difference",override.aes = list(alpha = 0.6))) +
    geom_bar(stat="identity",aes(alpha=alphacol),color='white') + 
    standart_theme +
    scale_alpha_identity() + 
    scale_fill_manual(values = c("blue","lightblue"),
                      limits = c('L-Lmin','Lrand-L'),label= c(TeX("$L-L_{min}$"),TeX("$L_r-L$")))
}

plot_score <- function(score,opt_df) {
  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  opt_df$score <- if (score == 'omega') opt_df$omega else if (score == 'psi') opt_df$psi
  
  ggplot(opt_df,aes(reorder(language,score),score)) + geom_bar(stat='identity',fill='lightblue') + 
    geom_text(aes(label = round(score,3)),nudge_y = -0.04, size=3.5) + theme(legend.position = 'bottom') +
    coord_flip() + labs(x='language', y = score_latex) + standart_theme
}

sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

plot_timeVSspace <- function(score,length_def,filter,robust) {
  # get data
  df_chars       <- read_file('opt','cv',filter,'characters') %>% select(language,omega,psi)
  df_chars$space <- if (score == 'omega') df_chars$omega else if (score == 'psi') df_chars$psi
  df_time        <- read_file('opt','cv',filter,length_def) %>% select(language,omega,psi)
  df_time$time   <- if (score == 'omega') df_time$omega else if (score == 'psi') df_time$psi
  df <- merge(df_time,df_chars, by = c('language')) %>% merge(langs_df_cv[,c('language','X.tokens')]) %>% 
     mutate(`T`=log10(X.tokens), label = ifelse(abs(time-space)>=0.10,language,''))
  
  if (robust==F) {
    cat("\nstd. error:", summary(lm(time~space,df))$coefficients[2, 2])
  } else cat("\nstd. error:", summary(mblm(time~space,df))$coefficients[2, 2])
  
  cat("\n", score, "pearson correlation:",round(cor(df$space, df$time),3))
  cat("\n", score, "pearson correlation p-value:",cor.test(df$space,df$time)$p.value,"\n")
  
  # plot
  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  ggplot(df,aes(space,time,label = label,fill=`T`)) + 
    geom_point(colour="black",pch=21, size=4) + 
    geom_abline(intercept = 0, slope=1, color = 'purple') + geom_text_repel(size = 5,color='black') + 
    labs(x=bquote(.(score_latex)[chars]), y = bquote(.(score_latex)[dur]),
         fill=expression(paste(log[10],'T'))) +
    theme(text = element_text(size = 20),legend.position = 'bottom',
          legend.text = element_text(size = 15),legend.title = element_text(size = 15)) +
    scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 5) +
    geom_smooth(method = ifelse(robust==T,sen,'lm')) +
    stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                 label.x.npc = "left",label.y.npc = 1.5,
                 eq.with.lhs = "italic(hat(y))~`=`~",eq.x.rhs = "~italic(x)",
                 method=ifelse(robust==T,sen,'lm'), parse = TRUE, size = 4, color="blue") +
    geom_text_npc(mapping = aes(npcx=0.15, npcy=0.95, label="y = x"), 
                  vjust="right", hjust="top", size = 4,
                  nudge_x=0.1, nudge_y=0.1, color="purple")
}



scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}


# CORRELATIONS
standart_theme <- theme(text = element_text(size = 16),
                        legend.text = element_text(size = 13),
                        legend.title = element_text(size = 13))

library(scales)
format_log_y_axis <- scale_y_log10(labels = label_log())

#format_log_y_axis <- scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
#                                   labels=scales::trans_format('log10',scales::math_format(10^.x))) 

assign_stars <- function(df) {
  df %>% mutate(stars = case_when(pvalue<=0.01               ~ '***',
                                  pvalue>0.01 & pvalue<=0.05 ~ '**',
                                  pvalue>0.05 & pvalue<=0.1  ~ '*',
                                  pvalue>0.1                 ~ 'x'))
}

plot_corr_significance <- function(df,corr_type) {
  low_col  <- ifelse(corr_type=='kendall','#41B85C','blue')
  high_col <- ifelse(corr_type=='kendall','orange','red')
  df$language <- factor(df$language, levels = c(sort(unique(df$language), decreasing = TRUE)))
  
  df %>% assign_stars() %>% 
    ggplot(aes(y=language, x=length_def, fill=corr)) + 
    labs(x="length definition", y="language") + standart_theme +
    geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
    scale_fill_gradient2(midpoint=0, low=low_col,high = high_col, mid = "white", na.value = "#b9d0ed")
}

plot_correlogram <- function(df,plot_corr,type,HB_correct=T,lab_size,tl.cex,pch.cex) {
  if ('Lrand' %in% colnames(df)) df <- df %>% rename(Lr = Lrand)
  if (type == 'null')   df <- df %>% mutate(`Lmin/Lr`=Lmin/Lr) %>% dplyr::select(Lmin,Lr,`Lmin/Lr`,eta,psi,omega)
  
  greek_names  <- switch(type,
                         'scores'=c('L','\u03B7','\u03A8','\u03A9'),
                         'params'=c('n', 'T', 'A', '\u03B7','\u03A8','\u03A9'), 
                         'null'=c(bquote(~L[min]),bquote(~L[r]),bquote(~L[min]/L[r]),
                                  'E[\u03B7]','E[\u03A8]','E[\u03A9]'))
  cors  <- round(cor(df, method=plot_corr), 2)
  p.mat <- corr.test(df, method=plot_corr, adjust = 'holm')$p
  if (HB_correct) p.mat <- symmetrise_mat(p.mat)
  ggcorrplot(cors, type = "lower", p.mat = p.mat,lab=T, lab_size = lab_size, tl.cex = tl.cex, pch.cex = pch.cex, 
             colors = switch(plot_corr, 'kendall'=corr_colors_tau, 'pearson'=corr_colors_r)) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=13)) + #change legend text font size  
    scale_x_discrete(labels = greek_names[-1]) + 
    scale_y_discrete(labels = greek_names[-length(greek_names)])
}



plot_etaVSlowerbound <- function(df) {
  df <- df %>% rename(`E[eta]`=eta) %>% mutate(`Lmin/Lr` = Lmin/Lrand) %>% 
    select(language,`Lmin/Lr`,`E[eta]`)
  ggplot(df, aes(x=`Lmin/Lr`,y=`E[eta]`,label=language)) + 
    labs(y = 'E[\u03B7]', x = bquote(~L[min]/L[r])) + 
    geom_abline(slope=1,intercept=0,color='purple')+
    geom_point(size=3) + theme(text = element_text(size = 20)) +
    standart_theme
}


plot_convergence <- function(df) {
  ggplot(df) + geom_line(aes(`t`,score,color=variable),size=1) + 
    facet_wrap(~language, ncol = 6) + geom_hline(yintercept=0,linetype='dashed',color='purple') + 
    theme(strip.text = element_text(size = 12),legend.position = 'bottom') +
    guides(color=guide_legend(title="score",nrow = 1)) +
    scale_color_discrete(labels=c('\u03B7','\u03A8','\u03A9')) +
    scale_x_log10(breaks = 10^(c(1,3,5,7)), labels = label_log()) + standart_theme
}


plot_score_comparison <- function(df) {
  ggplot(df,aes(x=x,y=y,label=language)) + 
    facet_wrap(~class, nrow = 1, scales="free", 
               labeller = labeller(class=c(eta='\u03B7',psi='\u03A8',omega='\u03A9'))) + 
    geom_text_repel(max.overlaps=50) + 
    labs(y = 'new scores', x = "original scores") + 
    geom_abline(slope=1,intercept=0,color='purple', size = 1,show.legend = TRUE)+
    geom_point(colour="black", size=4) +
    geom_smooth(method = 'lm', formula = y~x) +
    stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                 label.x.npc = "left", 
                 label.y.npc = 1.5,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 eq.x.rhs = "~italic(x)",
                 formula = y~x, parse = TRUE, size = 4, color="blue") +
    geom_text_npc(mapping = aes(npcx=0.15, npcy=0.95, label="y = x"), 
                  vjust="right", hjust="top", size = 4,
                  nudge_x=0.1, nudge_y=0.1, color="purple") +
    theme(text = element_text(size = 20))
}

plot_score_comparison <- function(score,df){
  score <- if (score=='omega') '\\Omega'  else if (score=='psi') '\\Psi'else if (score=='eta') '\\eta'
  ggplot(df,aes(x=x, y=y, label=language)) + 
    geom_text_repel(max.overlaps=50, color='black') + 
    geom_abline(slope=1,intercept=0,color='purple', size = 1,show.legend = TRUE)+
    geom_point() +
    geom_smooth(method = 'lm', formula = y~x) +
    stat_poly_eq(aes(label = paste(..eq.label.., sep = "~~~")), 
                 label.x.npc = "left", 
                 label.y.npc = 1.5,
                 eq.with.lhs = "italic(hat(y))~`=`~",
                 eq.x.rhs = "~italic(x)",
                 formula = y~x, parse = TRUE, size = 4, color="blue",coef.digits=3,f.digits=3) +
    geom_text_npc(mapping = aes(npcx=0.15, npcy=0.95, label="y = x"), 
                  vjust="right", hjust="top", size = 4,
                  nudge_x=0.1, nudge_y=0.1, color="purple") +
    theme(text = element_text(size = 17,family="sans"),legend.position = 'bottom') +
    labs(y = TeX(paste0(score,"$_{rem}$")), x = TeX(paste(score,"$_{chars}$")) ) 
}

params_labs <- c('Lmin'=expression(L[min]),'Lr'=expression(L[r]),'Lmin/Lr'=expression(L[min]/L[r]))
scores_labs <- c('eta'='\u03B7','psi'='\u03A8','omega'='\u03A9')


plot_corr_evolution <- function(df) {
  df <- df %>% 
    mutate(correlation=ifelse(correlation=='kendall', 'Kendall correlation','Pearson correlation')) %>% 
  mutate(parameter = ifelse(parameter=='Lmin','L[min]',ifelse(parameter=='Lr','L[r]','L[min]/L[r]'))) %>% 
    mutate(parameter = factor(parameter, levels = c('L[min]','L[r]','L[min]/L[r]')))
  ggplot(df,aes(randomizations,coefficient,color=score)) + 
    geom_line(size=1) + geom_point(aes(shape=significance),size=4) +
    facet_grid(rows=vars(correlation),cols=vars(parameter),labeller = labeller(parameter = label_parsed)) +
    geom_hline(yintercept = 0,linetype='dashed') +
    scale_shape_manual(values=c('significant'=1,'non-significant'=4)) +
    scale_color_discrete(labels = c('\u03B7','\u03A8','\u03A9')) +
    scale_x_log10(labels = label_log()) + standart_theme
}


long_corr_df <- function(df,plot_corr,HB_correct=T) {
  df    <- df %>% rename(Lr = Lrand) %>% mutate(`Lmin/Lr`=Lmin/Lr) %>% dplyr::select(Lmin,Lr,`Lmin/Lr`,eta,psi,omega)
  cors  <- round(cor(df, method=plot_corr), 2)
  p.mat <- corr.test(df, method=plot_corr, adjust = 'holm')$p
  if (HB_correct) p.mat <- symmetrise_mat(p.mat)
  ind <- which( lower.tri(p.mat,diag=F) , arr.ind = TRUE )
  data.frame(parameter = factor(dimnames(p.mat)[[2]][ind[,2]],levels=c('Lmin','Lr','Lmin/Lr')),
             score = factor(dimnames(p.mat)[[1]][ind[,1]],levels=c('eta','psi','omega')),
             coefficient = cors[ind],
             pvalue = p.mat[ ind ] ) %>% 
    filter(score %in% c('eta','psi','omega') & parameter %in% c('Lmin','Lr','Lmin/Lr')) %>% 
    mutate(significance=ifelse(pvalue<=0.05,'significant','non-significant'),
           correlation=plot_corr)
}



plot_scores_ingredients <- function(filter, collection, length_def) {
  df <- read_file('opt',collection,filter,length_def) %>% select(language, L,Lmin,Lrand,tau_min,tau)

  # add control points
  df_control <- df %>% mutate(L=Lrand, tau=0, language='')
  df <- rbind(df,df_control) %>% mutate(color=ifelse(language=='','control','real'))
  
  # a)
  ggplot(df,aes(y=Lmin, x=L, label=language,color=color)) + geom_point(size=2) + labs(y=bquote(L[min])) +
    geom_abline(slope=1, intercept=0, color = 'purple', size = 1) + geom_text_repel(size=4) +
    theme(text = element_text(size = 20), legend.position = 'none') + 
    scale_color_manual(values = c("real" = "black", "control" = "red"))
  ggsave(paste0(which_folder('figures',filter),'/a_',collection,"_",length_def,'.pdf'),
         device = cairo_pdf, width = 6, height = 8)
  # b)
  df %>% mutate(`Lr-Lmin`=Lrand-Lmin, `Lr-L`=Lrand-L) %>% 
    ggplot(aes(x=`Lr-Lmin`, y=`Lr-L`, label=language,color=color)) + geom_point(size=2) + 
    labs(x=bquote(L[r]-L[min]), y = bquote(L[r]-L)) +
    geom_abline(slope=1, intercept=0, color = 'purple') + geom_text_repel(size=4) +
    theme(text = element_text(size = 20), legend.position = 'none') + 
    scale_color_manual(values = c("real" = "black", "control" = "red"))
  ggsave(paste0(which_folder('figures',filter),'/b_',collection,"_",length_def,'.pdf'),
         device = cairo_pdf, width = 6, height = 8)
  # c)
  df %>% mutate(tau=-tau, tau_min=-tau_min) %>% 
  ggplot(aes(y=tau, x=tau_min, label=language,color=color)) + geom_point(size=2) + 
    labs(x=expression(-tau[min]), y=expression(-tau)) + scale_y_continuous(limits = c(0,1)) +
    geom_abline(slope=1, intercept=0, color = 'purple') + geom_text_repel(size=4) +
    theme(text = element_text(size = 20), legend.position = 'none') + 
    scale_color_manual(values = c("real" = "black", "control" = "red"))
  ggsave(paste0(which_folder('figures',filter),'/c_',collection,"_",length_def,'.pdf'),
         device = cairo_pdf, width = 6, height = 8)
}

