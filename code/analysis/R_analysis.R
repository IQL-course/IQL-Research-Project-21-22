
source('R_functions.R')


# RESULTS TO PRODUCE
# - X tables of optimality scores for every collection and length definition (prepare tables for Latex)
# - X density plot of omega 
# - X distribution of omega values
# - X tables of tau correlation with pvalues for every collection and length definition (plots)
# - X density plot of null hypothesis (Mengxue)
# - X how to show rankings by degree of optimality? (Mengxue)
# - X omega bar plots (left value of omega, right composition in bars) (Sonia)
# - X correlogram of opt scores (Sonia)
# - X plot of Omega_time vs Omega_chars with 45 diagonal (Sonia)
# - X compute omega with spearman correlation
# - X correlation between scores and n types, n tokens, alphabet size (exclude strokes)
# - X plot of timeVSspace for Psi as well
# - X convergence of scores


# NOTES FOR ANALYSIS
# - X for Romansh mix both dialects (Pre-processing people)
# - X in opt scores tables: sort by family, writing system, language name (retrieve family info from Glottolog)
# - X for Romansh we mixed both dialects, specify each dialect in materials, remove dialect from latex tables
# - X For Japanese and Chinese use both length in strokes and in characters
# - X Change order of script and family in table 5
# - X figure 4 should be pud not cv
# - X remove strokes from density plot and summary tables of opt scores
# - X add pinyin and romaji to code logics
# - X correlation between CV rankings
# - O prettify labels of correlograms


# NOTES FOR REPORT
# - criterion to remove types based on sd is based on the assumption of linear relation between sd and mean
# - use median, it's more robust (move mean to appendix)
# - O add kendall in correlogram scores
# - O would we preserve PUD rankings if we measured in an other way?
# - O pearson as robustness check for law of abbreviation 
   #(sometimes increase sometimes decrease)
# - X E[eta] against  Lmin/E[L] plot
# - X plot with arrows (6.4)
# - O be careful when stating averages over all corpora
# - X add HB note to correlogram captions



# QUESTIONS
# - what does it mean when Pearson is significant and kendall not?







# IMPLEMENTATION

# + collections summary 
lapply(COLLS, function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  sum_coll <- langs_df %>% mutate(dialect = NULL, iso_code = NULL) %>% rename(tokens = X.tokens, types = X.types)
  sum_coll <- sum_coll[,c('language','family','script','types','tokens')] %>% 
    filter(stringr::str_detect(language,'-strokes') == F) %>% arrange(family,script,language)
  print(xtable(sum_coll, type = "latex"), 
        file = here('latex_tables',paste0('coll_summary_',collection,".tex")),
        include.rownames=FALSE,include.colnames=FALSE, only.contents = TRUE,
        hline.after = c(nrow(sum_coll)))
})




# OPTIMALITY SCORES ------------------------------------------------------

# + scores for each language, collection, length_def
lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      label_length <- switch(length_def,"medianDuration" = 'median duration', 'characters' = 'number of characters')
      opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
      tau_df  <- read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]          # to remove if add tau and tau_min before
      merged  <- merge(opt_df,tau_df, by = c('language','family','script')) %>%                                     # to remove if add tau and tau_min before
        select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)             # to remove if add tau and tau_min before
      merged <- merged[,c('language', 'family', 'script', 'Lmin' , 'L', 'Lrand', 'corr', 'corr_min', 'eta' , 'psi' ,'omega')]
      print(xtable(merged,type = "latex"), 
            file = here('latex_tables',paste0(collection,"_opt_scores",suffix,corr_suffix,".tex")),
            include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE)
      })
  } else {
    opt_df  <- read.csv(here('results',paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))[-1]
    corr_df  <- read.csv(here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))[-1]       # to remove if add tau and tau_min before
    merged  <- merge(opt_df,corr_df, by = c('language','family','script')) %>%                           # to remove if add tau and tau_min before
      select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)    # to remove if add tau and tau_min before
    merged <- merged[,c('language', 'family', 'script', 'Lmin' , 'L', 'Lrand', 'corr', 'corr_min', 'eta' , 'psi' ,'omega')]
    print(xtable(merged,type = "latex"), 
          file = here('latex_tables',paste0(collection,"_opt_scores_characters",corr_suffix,".tex")),
          include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE)
  }
})


# + summary opt scores
lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score,corr_type) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(8,1,2,3,4,5,6,7)]
  print(xtable(summ, type = "latex"), 
        file = here('latex_tables',paste0("opt_scores_summary_",score,corr_suffix,".tex")),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,only.contents = TRUE)
})


# + correlogram of opt scores 
lapply(c('kendall','pearson'), function(plot_corr) {
  plot_corr_suffix <- paste0('_',plot_corr)
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        title <- paste(collection,length_def,sep='-')
        df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
          dplyr::select(L,eta,psi,omega)
        plot_correlogram(df,plot_corr,title,'scores',HB_correct = T,8,20,25)
        ggsave(here('figures',paste0('corrplot_',collection,suffix,plot_corr_suffix,'.pdf')))
      })
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      title <- paste(collection,length_def,sep='-')
      df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        dplyr::select(L,eta,psi,omega)
      plot_correlogram(df,plot_corr,title,'scores',HB_correct = T,8,20,25)
      ggsave(here('figures',paste0('corrplot_',collection,suffix,plot_corr_suffix,'.pdf')))
    }
    })
})


# + Omega in time vs Omega in chars
lapply(c('omega','psi'), function(score) {
  plot_timeVSspace(score, corr_type)
  ggsave(here('figures',paste0(score,'_timeVSspace',corr_suffix,'.pdf')))
})


# + ranking of Duration VS time
ranked_langs <- lapply(length_defs, function(length_def) {
  df <- read.csv(here('results',paste0('optimality_scores_cv_',length_def,corr_suffix,'.csv')))[-1] %>% 
    arrange(desc(psi))
  df$language
})
pdf(here('figures',paste0('timeVSspace_ranks',corr_suffix,'.pdf')))
plotRanks(ranked_langs[[1]],ranked_langs[[2]], 'characters   -   duration',labels.offset = 0.3)
dev.off()

# + correlation between rankings
psi_values <- lapply(length_defs, function(length_def) {
  df <- read.csv(here('results',paste0('optimality_scores_cv_',length_def,corr_suffix,'.csv')))[-1]
  df$psi
})
df_psi <- do.call(cbind,psi_values) 
cor <- cor(df_psi,method='kendall')[2,1]
pval <- cor_pmat(df_psi,method='kendall')[2,1]




# - 2 - correlation significance --------------------------------------------------------
lapply(c('kendall','pearson'), function(corr_type) {
  low_col  <- ifelse(corr_type=='kendall','#41B85C','blue')
  high_col <- ifelse(corr_type=='kendall','orange','red')
  corr_suffix <- paste0('_',corr_type)
  ## cv
  collection <- 'cv'
  rows_cv <- lapply(length_defs, function(length_def) {
    suffix <- paste0("_",length_def)
    read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      mutate(length_def = length_def)
  })
  df <- do.call(rbind,rows_cv) %>% assign_stars()
  ggplot(df,aes(y=language, x=length_def, fill=corr)) + 
    labs(x="length definition", y="language", title=paste0(corr_type,' correlation - ',collection)) +
    geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
    scale_fill_gradient2(midpoint=0, low=low_col,high = high_col, mid = "white", na.value = "#b9d0ed")
  ggsave(here('figures',paste0('corr_significance_',collection,corr_suffix,'.pdf')))
  
  ## pud
  collection <- 'pud'
  df <- read.csv(here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))[-1] %>% 
    mutate(length_def = 'characters') %>% assign_stars()
  ggplot(df,aes(y=language, x=length_def, fill=corr)) + 
    labs(x="length definition", y="language", title=paste0(corr_type,' correlation - ',collection)) +
    geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
    scale_fill_gradient2(midpoint=0, low=low_col,high = high_col, mid = "white", na.value = "#b9d0ed")
  ggsave(here('figures',paste0('corr_significance_',collection,corr_suffix,'.pdf')))
})





# SCORES DISTRIBUTION ---------------------------------------------------------

# + density plots
rows <- lapply(COLLS, function(collection) {
  if (collection =='cv') {
    rows <- lapply(length_defs, function(length) {
      suffix <- paste0("_",length)
      read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        select(language,eta,psi,omega) %>% mutate(collection = collection, `length definition` = length)
    })
    do.call(rbind.data.frame,rows)
  } else if (collection =='pud') {
    length <- 'characters'
    suffix <- paste0("_",length)
    read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      select(language,eta,psi,omega) %>% mutate(collection = collection, `length definition` = length) %>%
      filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
  }
})
df <- do.call(rbind.data.frame,rows) %>%
  reshape2::melt(id.vars = c('language','collection','length definition'))
means <- df %>% group_by(collection,`length definition`,variable) %>% summarise(meanvalue = mean(value))
ggplot(df) + geom_density(aes(x=value,color = `length definition`, fill = `length definition`),alpha = 0.2) + facet_grid(rows = vars(collection), cols = vars(variable)) +
  geom_vline(data=means, aes(xintercept=meanvalue, color = `length definition`),linetype='dashed') +
  theme(legend.position = 'bottom') + theme(axis.text.x = element_text(angle = 60, vjust = 0, hjust=0))
ggsave(here('figures',paste0('opt_scores_density',corr_suffix,'.pdf')))


# + Scores values and composition
rows <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    rows_cv <- lapply(length_defs, function(length_def) {
      suffix     <- paste0("_",length_def)
      plot_title <- paste0(collection,' - ',length_def)
      opt_df  <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        add_corr_min(suffix,corr_suffix)
      # PLOTS
      lapply(c('psi','omega'), function(score) {
        # plot 1
        plot_score(score,opt_df,plot_title)
        ggsave(here('figures',paste0(score,'_',collection,suffix,corr_suffix,'.pdf')))
        # plot 2
        plot_score_composition(score,opt_df,plot_title,corr_type)
        ggsave(here('figures',paste0(score,'_composition_',collection,suffix,corr_suffix,'.pdf')))
      })
      
    })
    do.call(rbind,rows_cv)
  } else if (collection == 'pud') {
    length_def <- 'characters'
    suffix       <- paste0("_",length_def)
    plot_title <- paste0(collection,' - ',length_def)
    opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      add_corr_min(suffix,corr_suffix)
    # PLOTS
    lapply(c('psi','omega'), function(score) {
      # plot 1
      plot_score(score,opt_df,plot_title)
      ggsave(here('figures',paste0(score,'_',collection,suffix,corr_suffix,'.pdf')))
      # plot 2
      plot_score_composition(score,opt_df,plot_title,corr_type)
      ggsave(here('figures',paste0(score,'_composition_',collection,suffix,corr_suffix,'.pdf')))
    })
    
  }
})



# + kendall vs spearman tables 
collection <- 'pud'
suffix <- '_characters'
opt_scores_dfs <- lapply(c('kendall','spearman'), function(corr_type) {
  opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,'_',corr_type,'.csv')))[-1] %>%
    select(language,family, script,omega)
  corr_df  <- read.csv(here('results',paste0('correlation_',collection,suffix,'_',corr_type,'.csv')))[-1]      # to remove if add tau and tau_min before
  opt_df  <- merge(opt_df,corr_df, by = c('language','family','script')) %>%                                 # to remove if add tau and tau_min before
    select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language) 
  if (corr_type=='kendall') {
    opt_df %>% rename(omega_tau=omega, tau = corr, tau_min = corr_min) 
    } else opt_df %>% rename(omega_ro=omega, ro = corr, ro_min = corr_min)
})

df <- merge(opt_scores_dfs[[1]],opt_scores_dfs[[2]], by = c('language','family','script'))
df <- df[,c('language','family','script','tau','ro','tau_min','ro_min','omega_tau','omega_ro')] %>% 
  arrange(family,script,language)
print(xtable(df,type = "latex"), 
      file = here('latex_tables',paste0("omega_tau_omega_ro_",collection,".tex")),
      include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE)









# FINDING THE BEST SCORE  --------------------------------------------------------------
# + correlation of scores with basic parameters (n tokens, n types, alphabet, L, eta, psi, omega)
lapply(COLLS, function(collection) {
  params_df <- get_langs_params(collection)
  lapply(c('kendall','pearson'), function(plot_corr) {
    plot_corr_suffix <- paste0('_',plot_corr)
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        # data
        opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,'_',length_def,'_',corr_type,'.csv'))) %>% 
          select(language,eta,psi,omega)
        df <- merge(params_df,opt_df, by='language') %>% select(-language)
        # plot
        title <- paste(collection,length_def,sep='-')
        plot_correlogram(df,plot_corr,title,'params',HB_correct=T,5,18,18)
        ggsave(here('figures',paste0('corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf')))
      })
    } else {
      length_def <- 'characters'
      # data
      params_df <- params_df %>% filter(language %!in% c('Japanese-strokes','Chinese-strokes'))
      opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,'_',length_def,'_',corr_type,'.csv'))) %>% 
        select(language,eta,psi,omega)
      df <- merge(params_df,opt_df, by='language') %>% select(-language)
      # plot
      title <- paste(collection,length_def,sep='-')
      plot_correlogram(df,plot_corr,title,'params',HB_correct=T,5,18,18)
      ggsave(here('figures',paste0('corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf')))
    }
  })
})



# + convergence of scores
sample_sizes <- c(2^seq(3,14),14000, 17000, 20000)
languages <- langs_df_pud$language
scores <- lapply(languages, function(lang) {
  lang_scores <- lapply(sample_sizes, function(n_sample) {
    compute_optimality_scores_lang(lang,'pud','characters','kendall',n_sample) %>%
      select(language,eta,psi,omega) %>% mutate(`number of tokens`=n_sample)
  })
  do.call(rbind.data.frame,lang_scores)
})
scores_df <- do.call(rbind.data.frame,scores)
melt(scores_df, id.vars=c('language','number of tokens')) %>%
  ggplot() + geom_line(aes(`number of tokens`,value,color=variable)) + 
  facet_wrap(~language) + geom_hline(yintercept=0,linetype='dashed',color='purple') + 
  theme(strip.text = element_text(size = 8)) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^as.integer(x)),
                labels=scales::trans_format('log10',scales::math_format(10^.x)))
ggsave(here('figures',paste0('convergence_pud.pdf')))




# NULL HYPOTHESYS --------------------------------------------------------------
iters <- 1e05

# + summary opt scores null
lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score,corr_type,null=T,iters = iters) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(8,1,2,3,4,5,6,7)]
  print(xtable(summ, type = "latex",digits=3), 
        file = here('latex_tables',paste0("opt_scores_summary_null_",score,corr_suffix,".tex")),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,only.contents = TRUE)
})


## correlation wit Lmin, Lr, and Lmin/Lr
remove_out <- F
lapply(c('kendall','pearson'), function(plot_corr) {
  plot_corr_suffix <- paste0('_',plot_corr)
  out_suffix <- ifelse(remove_out==T,'_noOut','')
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
        if (remove_out == T) df <-  df %>% filter(language %!in% c('Abkhazian','Panjabi'))
        title <- paste(collection,length_def,sep='-')
        plot_correlogram(df,plot_corr,title,'null',HB_correct=T,6,15,18)
          ggsave(here('figures',paste0('corrplot_null_',collection,suffix,plot_corr_suffix,out_suffix,'.pdf')))
      })
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      df <- read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1]
      if (remove_out == T) df <-  df %>% filter(language %!in% c('Abkhazian','Panjabi'))
      title <- paste(collection,length_def,sep='-')
      plot_correlogram(df,plot_corr,title,'null',HB_correct=T,6,15,18)
        ggsave(here('figures',paste0('corrplot_null_',collection,suffix,plot_corr_suffix,out_suffix,'.pdf')))
    }
  })
})


# E[scores] vs Lmin/Lr
collection <- 'cv'
rows_cv <- lapply(length_defs, function(length_def) {
  suffix       <- paste0("_",length_def)
  df <- read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
  df <- df %>% mutate(`Lmin/Lrand` = Lmin/Lrand) %>% dplyr::select(language,Lmin,psi,omega) %>% 
    rename(`E[psi]`=psi, `E[omega]`=omega) %>% mutate(length_def = length_def)
})
df <- do.call(rbind,rows_cv)
reshape2::melt(df, id.vars=c('language','Lmin','length_def')) %>% 
  ggplot(aes(x=`Lmin`,y=value,label=language)) + 
  geom_abline(slope=1,intercept=0,color='purple')+ 
  geom_point() + geom_hline(yintercept = 0,color='purple') +
  facet_grid(cols=vars(length_def),rows=vars(variable),scales = 'free')
ggsave(here('figures',paste0('correlation_scores_Lmin.pdf')))


# E[eta] vs Lmin/E[L]
rows <- lapply(COLLS, function(collection) {
  if (collection == 'pud') {
    length_def <- 'characters'
    suffix       <- paste0("_",length_def)
    df <- read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
    df <- df %>% rename(`E[eta]`=eta) %>% 
      mutate(`Lmin/E[L]` = Lmin/L,collection = paste0(collection,'-',length_def)) %>% 
      select(language,`Lmin/E[L]`,`E[eta]`,collection)
  } else {
    rows_cv <- lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      df <- read.csv(here('results',paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
      df <- df %>% rename(`E[eta]`=eta) %>% 
        mutate(`Lmin/E[L]` = Lmin/L,collection = paste0(collection,'-',length_def)) %>% 
        select(language,`Lmin/E[L]`,`E[eta]`,collection)
    })
    df <- do.call(rbind,rows_cv)
  }
})
df <- do.call(rbind,rows)
ggplot(df, aes(x=`Lmin/E[L]`,y=`E[eta]`,label=language)) + 
  geom_abline(slope=1,intercept=0,color='purple')+
  geom_point() + #geom_text(nudge_x = -0.1, nudge_y = -0.00005) +
  facet_wrap(~factor(collection, levels = c('pud-characters','cv-medianDuration','cv-characters')))
ggsave(here('figures',paste0('correlation_E[eta]_LminE[L].pdf')))




# PUD and CV languages  --------------------------------------------------------
common_langs <- langs_df_cv[langs_df_cv$language %in% langs_df_pud$language,]$language
dfs <- lapply(COLLS, function(collection) {
  suffix <- '_characters'
  dfs <- lapply(c('psi','omega'), function(score) {
    df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      filter(language %in% common_langs)
    df <- if (score == 'psi') select(df,language,psi) %>% mutate(score = 'psi') else select(df,language,omega) %>% mutate(score = 'omega')
    if (score == 'psi') rename(df,value=psi)  else rename(df,value=omega)
    })
  df <- do.call(rbind.data.frame,dfs)
  if (collection == 'cv') rename(df, cv=value ) else rename(df, pud=value )
})

df_common <- merge(dfs[[1]],dfs[[2]],by=c('language','score'))
ggplot(df_common,aes(pud,cv,label=language)) + geom_abline(intercept = 0,slope=1,color='purple')+
  geom_point() + geom_text(size=2,nudge_y = 0.01) + facet_wrap(~score,scales='free')
ggsave(here('figures',paste0('cvVSpud.pdf')))


# + ranking correlation
sapply(c('psi','omega'), function(scoree) {
  score_df <- df_common %>% filter(score == scoree) %>% arrange(pud) %>% select(pud,cv)
  cor  <- cor(score_df,method='kendall')[2,1]
  pval <- cor_pmat(score_df,method='kendall')[2,1]
  list('cor'=cor,'pval'=pval)
})




