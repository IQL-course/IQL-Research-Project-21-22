
source('R_functions.R')


# RESULTS TO PRODUCE
# - X fix pud processing script
# - X family and script to iso codes tables (someone)
# - X recompute 1e05 with new seed (job 1 running)
# - X compute convergence on available values
# - X add strokes to pud descriptive table
# - X recompute all tables (to add abk and ara)
# - X convergence of CV
# - O recompute expected values with DescTools:::.DoCount(x,y,wts) (to Run)



# NOTES FOR ANALYSIS


# NOTES FOR REPORT
# - criterion to remove types based on sd is based on the assumption of linear relation between sd and mean
# - use median, it's more robust (move mean to appendix)
# - X E[eta] against  Lmin/E[L] plot
# - X plot with arrows (6.4)
# - X add HB note to correlogram captions
# - X add kendall in correlogram scores
# - X would we preserve PUD rankings if we measured in an other way?
# - X pearson as robustness check for law of abbreviation 
   #(sometimes increase sometimes decrease)
# - X be careful when stating averages over all corpora
# - pud can make comparisons, but is not very large (still) the ranking could be preserved






# IMPLEMENTATION

# + filter alphabet with k-means
library(Ckmeans.1d.dp)
lapply(COLLS,function(collection) {
  iso_codes <- if (collection == 'pud') langs_df_pud$iso_code else if (collection == 'cv') langs_df_cv$iso_code
  lapply(iso_codes, function(iso_code) {
    df <- read.csv(here('code/preprocessing/',paste0(collection,'/characters/',iso_code,'-character.csv'))) %>% 
      mutate(Freq=log10(frequencyTot)) %>% arrange(desc(Freq))
    df$group_opt <- Ckmeans.1d.dp(df$Freq, 2)$cluster
    df <- df %>% filter(group_opt == 2)
    alphabet <- df$character
    write(alphabet, here('data/alphabets',paste0(collection,'/alphabet_',iso_code,'.txt')))
  })
})

# + alphabet sizes ADAPT
lapply(COLLS,function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  parameters <- lapply(langs_df$language, function(language) {
    df       <- read_language(language,collection,F,T)
    words    <- if ('romanized_form' %in% colnames(df)) tolower(df$romanized_form) else df$word
    alphabet <- unique(unlist(strsplit(words, '')))
    alphabet_size <- alphabet %>% length()
    list("language"=language, 'A'=alphabet_size)
  })
  df = do.call(rbind.data.frame,parameters)
  write.csv(df, here(paste0('results',folder_suffix),paste0('alphabet_sisez_',collection,'.csv')))
})


# + collections summary 
lapply(COLLS, function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  sum_coll <- langs_df %>% mutate(dialect = NULL, iso_code = NULL) %>% rename(T = X.tokens, n = X.types)
  A_coll   <- read.csv(here(paste0('results',folder_suffix),paste0('alphabet_sisez_',collection,'.csv')))
  sum_coll$A <- A_coll$A
  sum_coll <- sum_coll[,c('language','family','script','A','n','T')] %>% 
    arrange(family,script,language)
  write.csv(sum_coll,here(paste0('results',folder_suffix),paste0('coll_summary_',collection,'.csv')))
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
      opt_df  <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
      opt_df <- opt_df[,c('language', 'family', 'script', 'Lmin' , 'L', 'Lrand', 'corr', 'corr_min', 'eta' , 'psi' ,'omega')]
      print(xtable(opt_df,type = "latex"), 
            file = here('latex_tables',paste0(collection,"_opt_scores",suffix,corr_suffix,".tex")),
            include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(opt_df)))
      })
  } else {
    opt_df  <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,'_characters',corr_suffix,'.csv')))[-1]
    opt_df <- opt_df[,c('language', 'family', 'script', 'Lmin' , 'L', 'Lrand', 'corr', 'corr_min', 'eta' , 'psi' ,'omega')]
    print(xtable(opt_df,type = "latex"), 
          file = here('latex_tables',paste0(collection,"_opt_scores_characters",corr_suffix,".tex")),
          include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(opt_df)))
  }
})


# + summary opt scores
lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(9,1,2,3,4,5,6,7,8)]
  print(xtable(summ, type = "latex"),
        file = here('latex_tables',paste0("opt_scores_summary_",score,corr_suffix,".tex")),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,
        only.contents = TRUE,hline.after = c(nrow(summ)))
})


# + correlogram of opt scores 
lapply(c('kendall','pearson'), function(plot_corr) {
  plot_corr_suffix <- paste0('_',plot_corr)
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        suffix       <- paste0("_",length_def)
        df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
          dplyr::select(L,eta,psi,omega)
        plot_correlogram(df,plot_corr,'scores',HB_correct = T,10,22,25)
        ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_',collection,suffix,plot_corr_suffix,'.pdf')),device = cairo_pdf)
      })
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        dplyr::select(L,eta,psi,omega)
      plot_correlogram(df,plot_corr,'scores',HB_correct = T,10,22,25)
      ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_',collection,suffix,plot_corr_suffix,'.pdf')),device = cairo_pdf)
    }
    })
})


# + Omega in time vs Omega in chars
score <- 'psi'
rows_cv <- lapply(c('medianDuration','meanDuration'), function(length_def) {
  suffix <- paste0("_",length_def)
  plot_timeVSspace(score, corr_type,length_def)
  ggsave(here(paste0('figures',folder_suffix),paste0(score,'_timeVSspace',suffix,'.pdf')),device = cairo_pdf)
})




# + ranking of Duration VS time
ranked_langs <- lapply(length_defs, function(length_def) {
  df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_cv_',length_def,corr_suffix,'.csv')))[-1] %>% 
    arrange(desc(psi))
  df$language
})
pdf(here(paste0('figures',folder_suffix),paste0('timeVSspace_ranks',corr_suffix,'.pdf')))
plotRanks(ranked_langs[[1]],ranked_langs[[2]], 'characters   -   duration',labels.offset = 0.3)
dev.off()

# + correlation between rankings
psi_values <- lapply(length_defs, function(length_def) {
  df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_cv_',length_def,corr_suffix,'.csv')))[-1]
  df$psi
})
df_psi <- do.call(cbind,psi_values) 
cor <- cor(df_psi,method='kendall')[2,1]
pval <- cor_pmat(df_psi,method='kendall')[2,1]





# - 2 - correlation significance --------------------------------------------------------

lapply(c('kendall','pearson'), function(corr_type) {
  plot_corr_suffix <- paste0('_',corr_type)
  ## cv
  collection <- 'cv'
  rows_cv <- lapply(length_defs, function(length_def) {
    suffix <- paste0("_",length_def)
    df <- read.csv(here(paste0('results',folder_suffix),paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]
    length_def <- ifelse(length_def=='medianDuration','duration',length_def)
    df$length_def <- length_def
    df
  })
  df <- do.call(rbind,rows_cv) 
  plot_corr_significance(df,corr_type)
  ggsave(here(paste0('figures',folder_suffix),paste0('corr_significance_',collection,plot_corr_suffix,'.pdf')))
  
  ## pud
  collection <- 'pud'
  df <- read.csv(here(paste0('results',folder_suffix),paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))[-1] %>% 
    mutate(length_def = 'characters')
  plot_corr_significance(df,corr_type)
  ggsave(here(paste0('figures',folder_suffix),paste0('corr_significance_',collection,plot_corr_suffix,'.pdf')))
})





# SCORES DISTRIBUTION ---------------------------------------------------------

# + density plots
rows <- lapply(COLLS, function(collection) {
  if (collection =='cv') {
    rows <- lapply(length_defs, function(length) {
      suffix <- paste0("_",length)
      length <- ifelse(length=='medianDuration','duration',length)
      read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        select(language,eta,psi,omega) %>% mutate(collection = toupper(collection), `length definition` = length)
    })
    do.call(rbind.data.frame,rows)
  } else if (collection =='pud') {
    length <- 'characters'
    suffix <- paste0("_",length)
    read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      select(language,eta,psi,omega) %>% mutate(collection = toupper(collection), `length definition` = length) %>%
      filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
  }
})
df <- do.call(rbind.data.frame,rows) %>%
  reshape2::melt(id.vars = c('language','collection','length definition'))
means <- df %>% group_by(collection,`length definition`,variable) %>% summarise(meanvalue = mean(value))
ggplot(df) + geom_density(aes(x=value,color = `length definition`, fill = `length definition`),alpha = 0.2) + 
  facet_grid(rows = vars(collection), cols = vars(variable),
             labeller = labeller(variable=scores_labs)) +
  geom_vline(data=means, aes(xintercept=meanvalue, color = `length definition`),linetype='dashed') +
  theme(legend.position = 'bottom') + theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave(here(paste0('figures',folder_suffix),paste0('opt_scores_density',corr_suffix,'.pdf')), device = cairo_pdf)


# + Scores values and composition
score <- 'psi'
rows <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    print(collection)
    lapply(length_defs, function(length_def) {
      suffix     <- paste0("_",length_def)
      opt_df  <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        add_corr_min(collection,suffix,corr_suffix)
      # plot 1
      plot_score(score,opt_df)
      ggsave(here(paste0('figures',folder_suffix),paste0(score,'_',collection,suffix,'.pdf')), device = cairo_pdf)
      # plot 2
      plot_score_composition(score,opt_df)
      ggsave(here(paste0('figures',folder_suffix),paste0(score,'_composition_',collection,suffix,'.pdf')))
    })
  } else if (collection == 'pud') {
    print(collection)
    length_def   <- 'characters'
    suffix       <- paste0("_",length_def)
    opt_df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      add_corr_min(collection,suffix,corr_suffix)
    # plot 1
    plot_score(score,opt_df)
    ggsave(here(paste0('figures',folder_suffix),paste0(score,'_',collection,suffix,'.pdf')), device = cairo_pdf)
    # plot 2
    plot_score_composition(score,opt_df)
    ggsave(here(paste0('figures',folder_suffix),paste0(score,'_composition_',collection,suffix,'.pdf')))
  }
})



# + kendall vs spearman tables 
collection <- 'pud'
suffix <- '_characters'
opt_scores_dfs <- lapply(c('kendall','spearman'), function(corr_type) {
  opt_df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,'_',corr_type,'.csv')))[-1] %>%
    select(language,family, script,omega)
  corr_df  <- read.csv(here(paste0('results',folder_suffix),paste0('correlation_',collection,suffix,'_',corr_type,'.csv')))[-1]      # to remove if add tau and tau_min before
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
      include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(df)))









# FINDING THE BEST SCORE  --------------------------------------------------------------
## correlation of scores with basic parameters (n tokens, n types, alphabet, L, eta, psi, omega)
lapply(COLLS, function(collection) {
  params_df <-   read.csv(here(paste0('results',folder_suffix),paste0('coll_summary_',collection,'.csv'))) %>% 
    select(-X,-family,-script)
  lapply(c('kendall','pearson'), function(plot_corr) {
    plot_corr_suffix <- paste0('_',plot_corr)
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        # data
        opt_df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,'_',length_def,corr_suffix,'.csv'))) %>% 
          select(language,eta,psi,omega)
        df <- merge(params_df,opt_df, by='language') %>% select(-language)
        # plot
        plot_correlogram(df,plot_corr,'params',HB_correct=T,8,21,18)
        ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf')),device = cairo_pdf)
      })
    } else {
      length_def <- 'characters'
      # data
      params_df <- params_df %>% filter(language %!in% c('Japanese-strokes','Chinese-strokes'))
      opt_df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,'_',length_def,corr_suffix,'.csv'))) %>% 
        select(language,eta,psi,omega)
      df <- merge(params_df,opt_df, by='language') %>% select(-language)
      # plot
      plot_correlogram(df,plot_corr,'params',HB_correct=T,8,21,18)
      ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf')),device = cairo_pdf)
    }
  })
})



## convergence of scores 
sample_sizes <- c(2^seq(3,23))
rows <- lapply(COLLS, function(collection) {
  languages <- if (collection=='pud') langs_df_pud$language else langs_df_cv$language
  if (collection == 'cv') {
    print(collection)
    lapply(length_defs, function(length_def) {
      suffix     <- paste0("_",length_def)
      scores_df <- read.csv(here(paste0('results',folder_suffix),paste0('scores_convergence_',collection,suffix,'.csv')))[-1]  %>% 
        mutate(t = rep(sample_sizes,length(languages)))
      melt_df   <- reshape2::melt(scores_df, id.vars=c('language','t')) %>% 
        rename(score = value) %>% na.omit()
      melt_df_1 <- subset(melt_df, language %in% languages[1:23])
      plot_convergence(melt_df_1)
      ggsave(here(paste0('figures',folder_suffix),paste0('convergence_',collection,suffix,'_1.pdf')),device = cairo_pdf)
      melt_df_2 <- subset(melt_df, language %in% languages[24:46])
      plot_convergence(melt_df_2)
      ggsave(here(paste0('figures',folder_suffix),paste0('convergence_',collection,suffix,'_2.pdf')),device = cairo_pdf)
    })
  } else if (collection == 'pud') {
    print(collection)
    suffix <- '_characters'
    scores_df <- read.csv(here(paste0('results',folder_suffix),paste0('scores_convergence_',collection,suffix,'.csv')))[-1]  %>% 
      mutate(t = rep(sample_sizes,length(languages)))
    melt_df   <- reshape2::melt(scores_df, id.vars=c('language','t')) %>% 
      rename(score = value) %>% na.omit()
    plot_convergence(melt_df)
    ggsave(here(paste0('figures',folder_suffix),paste0('convergence_',collection,suffix,'.pdf')),device = cairo_pdf)
  }
})



# NULL HYPOTHESYS --------------------------------------------------------------
iters <- 1e06

# merge jobs 1 2 
lapply(length_defs,function(length_def) {
  job_ids <- if (length_def=='characters') 3:4 else 1:2
  dfs <- lapply(job_ids,function(job_id) read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_cv_',length_def,'_',iters,'_',job_id,'.csv'))) )
  df <- do.call(rbind.data.frame,dfs)[-1]
  write.csv(df, here(paste0('results',folder_suffix),paste0('null_hypothesis_cv_',length_def,'_',iters,'_kendall.csv')))
})

# + summary opt scores null
lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score,null=T,iters = iters) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(9,1,2,3,4,5,6,7,8)]
  print(xtable(summ, type = "latex",digits=3), 
        file = here('latex_tables',paste0("opt_scores_summary_null_",score,corr_suffix,".tex")),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,
        only.contents = TRUE,hline.after = c(nrow(summ)))
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
        df <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
        if (remove_out == T) df <-  df %>% filter(language %!in% c('Panjabi'))
        plot_correlogram(df,plot_corr,'null',HB_correct=T,8,22,18)
          ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_null_',collection,suffix,'_',iters,plot_corr_suffix,out_suffix,'.pdf')),device = cairo_pdf)
      })
    } else {
      length_def <- 'characters'
      suffix       <- paste0("_",length_def)
      df <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1]
      if (remove_out == T) df <-  df %>% filter(language %!in% c('Abkhazian','Panjabi'))
      plot_correlogram(df,plot_corr,'null',HB_correct=T,8,22,18)
        ggsave(here(paste0('figures',folder_suffix),paste0('corrplot_null_',collection,suffix,'_',iters,plot_corr_suffix,out_suffix,'.pdf')),device = cairo_pdf)
    }
  })
})


# E[scores] vs Lmin
collection <- 'cv'
rows_cv <- lapply(length_defs, function(length_def) {
  suffix       <- paste0("_",length_def)
  df <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
  df <- df %>% mutate(`Lmin/Lrand` = Lmin/Lrand) %>% dplyr::select(language,Lmin,psi,omega) %>% 
    rename(`E[psi]`=psi, `E[omega]`=omega) %>% mutate(length_def = length_def)
})
df <- do.call(rbind,rows_cv)
reshape2::melt(df, id.vars=c('language','Lmin','length_def')) %>% 
  mutate(color = ifelse(language %in% c('Vietnamese','Panjabi','Abkhazian','Dhivehi'),'undersampled','other')) %>% 
  ggplot(aes(x=`Lmin`,y=value,label=language,color=color)) + geom_text_repel(size=2)+
  geom_abline(slope=1,intercept=0,color='purple')+ 
  geom_point() + geom_hline(yintercept = 0,color='purple') +
  facet_grid(cols=vars(length_def),rows=vars(variable),scales = 'free')
ggsave(here(paste0('figures',folder_suffix),paste0('correlation_scores_Lmin.pdf')))


# E[eta] vs Lmin/E[L]
rows <- lapply(COLLS, function(collection) {
  if (collection == 'pud') {
    length_def <- 'characters'
    suffix       <- paste0("_",length_def)
    df <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
    plot_etaVSlowerbound(df)
    ggsave(here(paste0('figures',folder_suffix),paste0('E[eta]_LminLr_',collection,suffix,'.pdf')),device = cairo_pdf)
  } else {
    lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      df <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] 
      plot_etaVSlowerbound(df)
      ggsave(here(paste0('figures',folder_suffix),paste0('E[eta]_LminLr_',collection,suffix,'.pdf')),device = cairo_pdf)
    })
  }
})


# UNDERSAMPLED LANGS
collection <- 'cv'
suffix <- '_medianDuration'
out_langs <- c('Abkhazian','Dhivehi','Panjabi','Vietnamese')

opt_df  <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
  select(-family,-script)
df_null <- read.csv(here(paste0('results',folder_suffix),paste0('null_hypothesis_',collection,suffix,'_',iters,corr_suffix,'.csv')))[-1] %>% 
  select(language,omega) %>% rename(exp_omega=omega)

merged  <- merge(opt_df,df_null, by = c('language'))%>% 
  mutate(exp_tau = exp_omega*corr_min, `Lmin/Lr`=Lmin/Lrand)


# boxplots: Lr_diff, tau, tau_min, L_min
par(mfrow=c(2,2))
pdf(here(paste0('figures',folder_suffix),'usl_all_boxplots.pdf'))
lapply(c('E[tau]','tau_min','Lmin_dur','Lmin_dur/Lr'), function(i) {
  merged$var <- switch(i, 'E[tau]'=merged$exp_tau,'Lmin_dur'=merged$Lmin,
                       'Lmin_dur/Lr'=merged$`Lmin/Lr`, 'tau_min'=merged$corr_min)
  outs <- merged$var[merged$language %in% out_langs]
  boxplot(merged$var)
  abline(h=outs,col='red')
  text(x=1, y=outs, labels=out_langs)
  title(paste0('Boxplot of ',i))
})
dev.off()






# PUD and CV languages  --------------------------------------------------------
sorted_df <- langs_df_cv %>% arrange(desc(X.tokens))
common_langs <- sorted_df[sorted_df$language %in% langs_df_pud$language,]$language

dfs <- lapply(COLLS, function(collection) {
  suffix <- '_characters'
  dfs <- lapply(c('psi','omega'), function(score) {
    df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      filter(language %in% common_langs)
    df <- if (score == 'psi') select(df,language,psi) %>% mutate(score = 'psi') else select(df,language,omega) %>% mutate(score = 'omega')
    if (score == 'psi') rename(df,value=psi)  else rename(df,value=omega)
    })
  df <- do.call(rbind.data.frame,dfs)
  if (collection == 'cv') rename(df, cv=value ) else rename(df, pud=value )
})

df_common <- merge(dfs[[1]],dfs[[2]],by=c('language','score'))
df_common <- merge(df_common, langs_df_cv[,c('language','family','script')], by ='language') %>% 
  mutate(score = factor(score,levels=c('psi','omega')))
ggplot(df_common,aes(pud,cv,label=language)) + geom_abline(intercept = 0,slope=1,color='purple')+
  geom_point(aes(color=family,shape=script)) + geom_text_repel(size=3) + 
  theme(legend.position = 'bottom')+ guides(color=guide_legend(nrow=2,byrow=TRUE))+
  facet_wrap(~score,scales='free',labeller = labeller(score=scores_labs))
ggsave(here(paste0('figures',folder_suffix),paste0('cvVSpud_groups.pdf')), device = cairo_pdf)


# + ranking correlation
cors <- lapply(2:13, function(k) {
  dfs <- lapply(COLLS, function(collection) {
    suffix <- '_characters'
    dfs <- lapply(c('psi','omega'), function(score) {
      df <- read.csv(here(paste0('results',folder_suffix),paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
        filter(language %in% common_langs[1:k])
      df <- if (score == 'psi') select(df,language,psi) %>% mutate(score = 'psi') else select(df,language,omega) %>% mutate(score = 'omega')
      if (score == 'psi') rename(df,value=psi)  else rename(df,value=omega)
    })
    df <- do.call(rbind.data.frame,dfs)
    if (collection == 'cv') rename(df, cv=value ) else rename(df, pud=value )
  })
  df_common <- merge(dfs[[1]],dfs[[2]],by=c('language','score'))
  cors <- lapply(c('psi','omega'), function(scoree) {
    score_df <- df_common %>% filter(score == scoree) %>% arrange(pud) %>% select(pud,cv)
    cor  <- cor(score_df,method='kendall')[2,1]
    pval <- cor_pmat(score_df,method='kendall')[2,1]
    l <- list('cor'=cor, 'pval'=pval)
    names(l) <- c(paste0('cor_',scoree),paste0('pval_',scoree))
    l
  })
  do.call(c,cors)
})
df_k <- do.call(rbind.data.frame,cors) %>% mutate(k = 2:13, min_size =sapply(sorted_df$X.tokens[2:13],min))
print(xtable(df_k, type = "latex"), 
  file = here('latex_tables','cvVSpud_k.tex'),
  include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(df_k)))



# PUD scores after removing vowels ---------------------------------------------

# - 1 - Significance of word lengths
print('begin to compute tau correlations')
tau_df <- compute_corr("pud", remove_vowels = TRUE)
# Chinese
df      <- read_language("Chinese-pinyin",'pud',TRUE)
df$word <- gsub("[aeiouāáǎàōóǒòēéěèīíǐìūúǔùǖǘǚǜäöüůåąı]","",df$word)
df$length <- nchar(df$word)   
df  <- df %>% mutate(rank=1:nrow(.))
res <- cor.test(df$frequency,df$length, method=corr_type, alternative = "less")
tau_df$corr[tau_df$language=="Chinese-pinyin"] <- res$estimate
tau_df$pvalue[tau_df$language=="Chinese-pinyin"] <- res$p.value
tau_df <- tau_df%>% 
  arrange(pvalue) %>% mutate(index=1:nrow(.)) %>%                                                     # Holm-Bonferroni correction
  mutate(hb_pvalue = pvalue*(nrow(.)+1-index), index = NULL)   

write.csv(tau_df, here('results',paste0('correlation_pud_remove_vowels_kendall.csv')))

# - 2 - Compute scores
print('begin to compute optimality scores')
opt_df <- compute_optimality_scores_coll("pud", remove_vowels = TRUE)
# Chinese
N_types    <- nrow(df)
p          <- df$frequency/sum(df$frequency)
Lmin       <- sum(sort(df$length)*p)                                                # min baseline
L          <- sum(df$length*p)                                                      # real value (weight by freq)
Lrand      <- sum(df$length)/N_types                                                # random baseline (unweighted)
corr_min   <- cor.fk(df$frequency, sort(df$length))
corr       <- tau_df$corr[tau_df$language=="Chinese-pinyin"]
opt_df$Lmin[opt_df$language=="Chinese-pinyin"]  <- Lmin
opt_df$L[opt_df$language=="Chinese-pinyin"]     <- L
opt_df$Lrand[opt_df$language=="Chinese-pinyin"] <- Lrand
opt_df$corr_min[opt_df$language=="Chinese-pinyin"]  <- corr_min
opt_df$eta[opt_df$language=="Chinese-pinyin"]   <- Lmin/L
opt_df$psi[opt_df$language=="Chinese-pinyin"]   <- (Lrand-L)/(Lrand-Lmin)
opt_df$omega[opt_df$language=="Chinese-pinyin"] <- corr/corr_min 

write.csv(opt_df, here('results',paste0('optimality_scores_pud_remove_vowels_kendall.csv')))

# plot comparison
df_remove <- read.csv(here('results',paste0('optimality_scores_pud_remove_vowels_kendall.csv')))
df_noremove <- read.csv(here('results',paste0('optimality_scores_pud_characters.csv')))
df <- merge(df_noremove[c("language","psi","omega","eta")],df_remove[c("language","psi","omega","eta")],by="language")



# effect of FILTERING  ---------------------------------------------------------
collection <- 'cv'
lapply(COLLS, function(collection) {
  lapply(length_defs, function(length_def) {
    length_def <- if (collection=='pud') 'characters' else length_def
    get_filtered_ori_df(collection,length_def) %>% 
      ggplot(aes(original,filtered,label=language)) + 
        geom_abline(intercept = 0,slope=1,color='purple') + geom_point() + geom_text_repel(size=3) + 
        theme(legend.position = 'bottom') + guides(color=guide_legend(nrow=2,byrow=TRUE))+
        facet_wrap(~score,scales='free',labeller = labeller(score=scores_labs))
    ggsave(here(paste0('figures',folder_suffix),paste0('filteredVSoriginal_',collection,'_',length_def,'.pdf')), device = cairo_pdf)
  })
})





