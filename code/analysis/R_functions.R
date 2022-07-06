
library('here')
library('dplyr')
options(dplyr.summarise.inform = FALSE)
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
corr_type <- 'kendall'
corr_suffix <- paste0('_',corr_type)
scores_labs <- c('\u03B7','\u03A8','\u03A9'); names(scores_labs) <- c('eta','psi','omega')
corr_colors_r = c("blue", "white", "red")
corr_colors_tau = c("#41B85C", "white", "orange")


## pud
langs_df_pud <- read.csv(here("data/descriptive_tables/pud.csv")) %>% select(-dialect)
langs_pud    <- langs_df_pud$language


## cv
langs_df_cv <- read.csv(here("data/descriptive_tables/common_voice.csv")) %>% 
  filter(iso_code %!in% c('jpn','zho')) %>% select(-dialect) %>%                                             # remove Chinese and Japanese
  rows_update(tibble(language = "Interlingua", iso_code = 'ina'), by = "iso_code") %>%  # shorten names 
  rows_update(tibble(family = "Conlang", iso_code = 'ina'), by = "iso_code") %>%
  rows_update(tibble(family = "Conlang", iso_code = 'epo'), by = "iso_code") %>%
  rows_update(tibble(language = "Oriya", iso_code = 'ori'), by = "iso_code") %>%
  rows_update(tibble(language = "Modern Greek", iso_code = 'ell'), by = "iso_code")
langs_cv    <- langs_df_cv$language
ISO_cv      <- langs_df_cv$iso_code


# functions  -------------------------------------------------------------------
read_language <- function(language, collection, remove_vowels=FALSE) {
  if(!remove_vowels){
    if (collection == 'cv') {
      iso_code <- langs_df_cv$iso_code[langs_df_cv$language==language]
      read.csv(here("data/cv",paste0(iso_code,"-word.csv")), encoding = 'UTF-8') %>% 
        rename(frequency = repetitions, word = orthographic_form) %>% 
        arrange(desc(frequency)) %>% 
        mutate(characters = nchar(word), language = language) 
    } else if (collection == 'pud') {
      iso_code <- langs_df_pud$iso_code[langs_df_pud$language==language]
      alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
      str_suffix <- ifelse (is.null(alternative),'',paste0('_',alternative))
      read.csv(here("data/pud",paste0(iso_code,str_suffix,"_pud.csv")), encoding = 'UTF-8')[-1]
    } else print('specify an available collection')
  }
  else {
    if(langs_df_pud$script[langs_df_pud$language==language]=='Latin'){
      iso_code <- langs_df_pud$iso_code[langs_df_pud$language==language]
      alternative <- if(iso_code=='zho') "pinyin" else if(iso_code=='jpn') "romaji" else NULL   # file suffix
      str_suffix <- ifelse (is.null(alternative),'',paste0('_',alternative))
      cat("=============file name",paste0(iso_code,str_suffix,"_pud.csv\n"))
      df <- read.csv(here("data/pud",paste0(iso_code,str_suffix,"_pud.csv")), encoding = 'UTF-8')[-1]
      cat("=============chinese",length(df))##########test
      df$word <- if(iso_code=='zho' | iso_code=='jpn') df$romanized_form else df$word      # word <- Latin script
      # remove vowels
      df$word <- if(iso_code=='fin' | iso_code=='fra' | iso_code=='pol')              # with y
        gsub("[aeiouAEIOUāáǎàōóǒòēéěèīíǐìūúǔùǖǘǚǜäöüůåyąę]","", df$word) else
          if(iso_code=='isl' | iso_code=='ces')                                       # with ý
            gsub("[aeiouAEIOUāáǎàōóǒòēéěèīíǐìūúǔùǖǘǚǜäöüůåýąęı]","", df$word) else
              gsub("[aeiouAEIOUāáǎàōóǒòēéěèīíǐìūúǔùǖǘǚǜäöüůåąęı]","", df$word)        # no y
      df$length <- nchar(df$word)                                                     # compute new length
      return(df[-which(df$length==0),])
    }
  }
}



compute_corr <- function(collection, corr_type='kendall', length = 'characters', remove_vowels=FALSE) {
  if(!remove_vowels){
    langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
    cors <- mclapply(langs_df$language, function(language) {
      print(language)
      df <- read_language(language,collection) %>% mutate(rank=1:nrow(.))
      if (collection == 'cv') {
        df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$characters
      }
      res <- cor.test(df$frequency,df$length, method=corr_type, alternative = "less")
      list("language"=language, "corr"=res$estimate, "pvalue"=res$p.value)
    },mc.cores = 3) 
    df <- do.call(rbind.data.frame,cors) %>% 
      arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
      mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
    return(df)
  }
  else {
    langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
    cors <- mclapply(langs_df$language[langs_df$script=='Latin'], function(language) {
      print(language)
      df <- read_language(language,collection,TRUE) %>% mutate(rank=1:nrow(.))
      #if (collection == 'cv') {
      #  df$length <- if (length == 'meanDuration') df$meanDuration else if (length == 'medianDuration') df$medianDuration else df$characters
      #}
      res <- cor.test(df$frequency,df$length, method=corr_type, alternative = "less")
      list("language"=language, "corr"=res$estimate, "pvalue"=res$p.value)
    },mc.cores = 1)  # original 3
    df <- do.call(rbind.data.frame,cors) %>% 
      arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
      mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   
    return(df)
  }
}



compute_optimality_scores_lang <- function(lang, collection, length_def='characters', corr_type='kendall') {
  df <- read_language(lang,collection)
  # choose definition of 'length' in cv
  if (collection == 'cv') {
    df$length <- if (length_def == 'meanDuration') df$meanDuration else if (length_def == 'medianDuration') df$medianDuration else df$characters
  }
  N_types    <- nrow(df)
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)                                                # min baseline
  L          <- sum(df$length*p)                                                      # real value (weight by freq)
  Lrand      <- sum(df$length)/N_types                                                # random baseline (unweighted)
  corr <- read.csv(here('results',paste0('correlation_',collection,'_',length_def,'_',corr_type,'.csv'))) %>% 
      filter(language == lang) %>% select(corr) %>% as.numeric()
  corr_min  <- if (corr_type=='kendall') { 
    cor.fk(df$frequency, sort(df$length))
  } else cor.test(df$frequency,sort(df$length), method=corr_type, alternative = "less")$estimate %>% unname()
  # scores:
  eta   <- Lmin/L
  psi   <- (Lrand-L)/(Lrand-Lmin)
  omega <- corr/corr_min 
  data.frame("language"=lang, "Lmin"=Lmin, "L"=L, "Lrand"=Lrand,'corr'=corr, 'corr_min'=corr_min, "eta"=eta, "psi"=psi, "omega"=omega)
}


compute_optimality_scores_coll <- function(collection, corr_type='kendall', length_def = 'characters') {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  res <- mclapply(langs_df$language, function(language) {
    compute_optimality_scores_lang(language,collection,length_def,corr_type)
  },mc.cores = 3)
  df <- do.call(rbind.data.frame,res) 
  df <- merge(df,langs_df[,c('language','family','script')], by='language') %>% arrange(family,script,language)
  return(df)
}




compute_convergence_scores_lang <- function(df_all,lang, n_sample, n_experiments = 10^2) {
  set.seed(962)
  scores <- lapply(1:n_experiments, function(i) {
    # sample
    df <- if (n_sample<=nrow(df_all)) {
      sample_n(df_all,n_sample) %>% group_by(word) %>% summarise(frequency = n(),length=length) %>% 
        unique() %>% arrange(desc(frequency))
    } else df %>% mutate(length = NA, frequency=NA)
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
  
  sums <- sapply(1:length(scores[[1]]), function(score_index) {
    score <- names(scores[[1]])[score_index]
    value <- do.call(c,lapply(scores, `[[`, score_index)) %>% sum()
    names(value) <- score
    value
  })
  averages <- sums/n_experiments
  data.frame("language"=lang, "eta"=averages[1], "psi"=averages[2], "omega"=averages[3], 't'=n_sample)
}


scores_convergence <- function(languages,sample_sizes,n_experiments) {
  scores <- mclapply(languages, function(lang) {
    df <- read_language(lang,'pud')
    df_all <- df[rep(seq_len(nrow(df)), df$frequency),]
    
    lang_scores <- lapply(sample_sizes, function(n_sample) {
      set.seed(19)
      compute_convergence_scores_lang(df_all,lang,n_sample,n_experiments)
    })
    do.call(rbind.data.frame,lang_scores)
  },mc.cores=3)
  
  do.call(rbind.data.frame,scores)
}




compute_expectation_scores_lang <- function(lang, collection, length_def='characters', n_experiments = 10^2) {
  df <- read_language(lang,collection)
  # choose definition of 'length' in cv
  if (collection == 'cv') {
    df$length <- if (length_def == 'medianDuration') df$medianDuration else df$characters
  }
  N_types    <- nrow(df)
  p          <- df$frequency/sum(df$frequency)
  Lmin       <- sum(sort(df$length)*p)                                                # min baseline
  Lrand      <- sum(df$length)/N_types    # random baseline (unweighted)
  corr_min   <- cor.fk(df$frequency, sort(df$length))
  
  set.seed(962)
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


opt_score_summary <- function(score,null=F, iters = 1000) {
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      rows_cv <- lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- if (null == F) read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] 
              else read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
        df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
        length_def <- ifelse(length_def=='medianDuration','duration',length_def)
        df %>% mutate(collection = paste(toupper(collection),length_def,sep='-'))
      })
      do.call(rbind,rows_cv)
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      df <- if (null == F) read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] 
            else read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
      df <- df %>% filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
      df$score <- if (score=='omega') df$omega else if (score == 'eta') df$eta else if (score == 'psi') df$psi
      df %>% mutate(collection = paste(toupper(collection),'characters',sep='-'))
    }
  })
  df <- do.call(rbind,rows); setDT(df)
  df[, as.list(summary(score)), by = collection] %>% cbind('sd'=df[, as.list(sd(score)), by = collection]$V1)
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



add_corr_min <- function(opt_df,collection,suffix,corr_suffix) {
  corr_df <- read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]      # to remove if add tau and tau_min before
  merge(opt_df,corr_df, by = c('language','family','script')) %>%                         # to remove if add tau and tau_min before
    select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)  # to remove if add tau and tau_min before
}





# PLOT FUNCTIONS 

plot_score_composition <- function(score,opt_df) {
  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  if (score == 'psi') {
    L_diff_df <- opt_df %>% select(language,Lmin,L,Lrand,psi) %>% summarise(language,psi,Lmin,`Lrand-L` = Lrand-L, `L-Lmin` = L-Lmin)
    L_diff_df$language <- factor(L_diff_df$language, levels = L_diff_df$language[order(L_diff_df$psi)])
    melted <- reshape2::melt(L_diff_df, id.vars=c("language","psi")) %>% 
      mutate(masked_psi = ifelse(variable == "Lrand-L",round(psi,2),""))
    melted$alphacol <- ifelse(melted$variable=="Lmin",0,ifelse(melted$variable=="L-Lmin",0.3,1))
    melted$variable <- factor(melted$variable, levels=c("Lrand-L","L-Lmin","Lmin"))
    ggplot(melted,aes(x=language,y=value,fill=variable)) + coord_flip() + 
      theme(legend.position = 'right',axis.title.y = element_blank(),axis.text.y = element_blank()) +
      labs(x=score_latex, y="Length") + guides(fill=guide_legend(title="difference")) +
      geom_bar(stat="identity",aes(alpha=alphacol),color='white') +
      scale_alpha_identity() + scale_fill_manual(values = c("blue","lightblue"),limits = c('L-Lmin','Lrand-L'))
  } else if (score == 'omega') {
    corr_diff_df <- opt_df %>% select(language,omega,corr,corr_min) %>% 
      summarise(language,omega,corr,'corr_min-corr' = corr_min-corr)
    corr_diff_df$language <- factor(corr_diff_df$language, levels = corr_diff_df$language[order(corr_diff_df$omega)])
    melted <- reshape2::melt(corr_diff_df, id.vars=c("language","omega")) %>% 
      mutate(masked_omega = ifelse(variable == "corr",round(omega,2),""))
    melted$variable <- factor(melted$variable, levels=c("corr_min-corr",'corr'))
    melted$alphacol <- ifelse(melted$variable=="corr",1,0.3)
    ggplot(melted,aes(x=language,y=value,fill=variable))  + theme(legend.position = 'none') +
      geom_bar(stat="identity",aes(alpha=alphacol),color='white') + coord_flip() + 
      labs(x=score_latex, y= 'correlation') + guides(fill=guide_legend(title="difference")) +
      scale_alpha_identity() + scale_fill_manual(values = c("blue","lightblue"),limits = c('corr_min-corr', 'corr'))
  }
}

plot_score <- function(score,opt_df) {
  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  opt_df$score <- if (score == 'omega') opt_df$omega else if (score == 'psi') opt_df$psi
  
  ggplot(opt_df,aes(reorder(language,score),score)) + geom_bar(stat='identity',fill='lightblue') + 
    geom_text(aes(label = round(score,3)),nudge_y = -0.04, size=3) + theme(legend.position = 'bottom') +
    coord_flip() + labs(x='language', y = score_latex)
}


plot_timeVSspace <- function(score,corr_type,length_def) {
  df_chars <- read.csv(here('results',paste0('optimality_scores_cv_characters',corr_suffix,'.csv')))[-1] 
  df_chars$space <- if (score == 'omega') df_chars$omega else if (score == 'psi') df_chars$psi
  
  suffix       <- paste0("_",length_def)
  length_def <- gsub('Duration', "", length_def)
  df <- read.csv(here('results',paste0('optimality_scores_cv',suffix,corr_suffix,'.csv')))[-1 ]%>% dplyr::select(-Lmin,-L,-Lrand,-eta)
  df$score <- if (score == 'omega') df$omega else if (score == 'psi') df$psi
  df_time <- df %>% rename(time = score) %>% mutate(time_def = length_def)

  score_latex <- if (score=='omega') '\u03A9'  else if (score=='psi') '\u03A8'
  df <- merge(df_time,df_chars, by = c('language')) %>% 
    mutate(label = ifelse(abs(time-space)>=0.10,language,''),color = ifelse(abs(time-space)<0.10,'<10%','>=10%'))
  df %>% 
    ggplot(aes(space,time,label = label,color=color)) + geom_point() + theme(legend.position = 'bottom') +
    geom_abline(intercept = 0, slope=1, color = 'purple') + geom_text_repel(size = 3) + 
    labs(x=bquote(.(score_latex)[chars]), y = bquote(.(score_latex)[dur])) +
    scale_color_manual(values = c("blue","red"),limits = c('>=10%', '<10%')) + 
    guides(color=guide_legend(title="deviation")) + theme(text = element_text(size = 20))
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
         length=arrow.len, angle=20, col =  ifelse(abs(1:length(a)-a.to.b) >10,'blue','black'))
  par(old.par)
}


# CORRELATIONS
plot_corr_significance <- function(df,corr_type) {
  low_col  <- ifelse(corr_type=='kendall','#41B85C','blue')
  high_col <- ifelse(corr_type=='kendall','orange','red')
  
  df %>% assign_stars() %>% 
    ggplot(aes(y=language, x=length_def, fill=corr)) + 
    labs(x="length definition", y="language") +
    geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
    scale_fill_gradient2(midpoint=0, low=low_col,high = high_col, mid = "white", na.value = "#b9d0ed")
}

plot_correlogram <- function(df,plot_corr,type,HB_correct=T,lab_size,tl.cex,pch.cex) {
  if ('Lrand' %in% colnames(df)) df <- df %>% rename(Lr = Lrand)
  if (type == 'null')   df <- df %>% mutate(`Lmin/Lr`=Lmin/Lr) %>% dplyr::select(Lmin,Lr,`Lmin/Lr`,eta,psi,omega)
  
  greek_names  <- switch(type,
                         'scores'=c('L','\u03B7','\u03A8','\u03A9'),
                         'params'=c('n', 'T', 'A', '\u03B7','\u03A8','\u03A9'), 
                         'null'=c(bquote(~L[min]),bquote(~L[r]),bquote(~L[min]/L[r]),'E[\u03B7]','E[\u03A8]','E[\u03A9]'))
  legend_title <-  switch(plot_corr, 'kendall' = '\u03C4 corr', 'pearson'='r corr')
  
  cors  <- round(cor(df, method=plot_corr), 2)
  p.mat <- cor_pmat(df, method=plot_corr)
  if (HB_correct) p.mat <- HB_correction(p.mat)
  ggcorrplot(cors, type = "lower", p.mat = p.mat,lab=T, lab_size = lab_size, tl.cex = tl.cex, pch.cex = pch.cex, 
             colors = switch(plot_corr, 'kendall'=corr_colors_tau, 'pearson'=corr_colors_r),
             legend.title = legend_title) +
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.title = element_text(size=16), #change legend title font size
          legend.text = element_text(size=13)) + #change legend text font size  
    scale_x_discrete(labels = greek_names[-1]) + scale_y_discrete(labels = greek_names[-length(greek_names)])
}

plot_etaVSlowerbound <- function(df) {
  df <- df %>% rename(`E[eta]`=eta) %>% mutate(`Lmin/Lr` = Lmin/Lrand) %>% 
    select(language,`Lmin/Lr`,`E[eta]`)
  ggplot(df, aes(x=`Lmin/Lr`,y=`E[eta]`,label=language)) + 
    labs(y = 'E[\u03B7]', x = bquote(~L[min]/L[r])) + 
    geom_abline(slope=1,intercept=0,color='purple')+
    geom_point() + theme(text = element_text(size = 20))
}

