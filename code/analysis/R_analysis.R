
Sys.setlocale("LC_ALL","English")
source('code/analysis/R_functions.R', encoding="utf-8")

# ARGUMENTS: filter
# where:
  # filter = (T,F) [default is T]


args = commandArgs(trailingOnly=TRUE)
filter <- if (length(args) == 1) as.logical(args[[1]]) else F


# GLOBALS  --------------------------------------------------------
langs_df_pud <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/pud.csv"))
langs_df_cv <- read.csv(paste0(which_folder('data',filter),"/descriptive_tables/common_voice.csv")) %>% 
  shorten_names()



# COLLECTIONS SUMMARIES --------------------------------------------------------

# + filter alphabet with k-means
print('file: alphabets')
res <- lapply(COLLS,function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  lapply(langs_df$language, function(language) {
    iso_code    <- langs_df$iso_code[langs_df$language==language]
    alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
    str_suffix  <- ifelse (is.null(alternative),'',paste0('-',alternative))
    df <- read.csv(paste0('data/non_filtered/alphabets/',collection,'/',iso_code,str_suffix,'-character.csv')) %>% 
      mutate(Freq=log10(frequency)) %>% arrange(desc(Freq))
    df$group_opt <- Ckmeans.1d.dp(df$Freq, 2)$cluster
    df <- filter(df,group_opt == 2)
    alphabet <- df[,c(1,2,3)]
    print(paste0('data/filtered/alphabets/',collection,'/',iso_code,str_suffix,'-character.csv'))
    write.csv(alphabet, paste0('data/filtered/alphabets/',collection,'/',iso_code,str_suffix,'-character.csv'),row.names = FALSE)
  })
})

# + alphabet sizes 
print('file: alphabets sizes')
res <- lapply(COLLS,function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  parameters <- lapply(langs_df$language, function(language) {
    iso_code    <- langs_df$iso_code[langs_df$language==language]
    alternative <- if (stringr::str_detect(language,'-')) sub(".*-","",language) else NULL
    str_suffix  <- ifelse (is.null(alternative),'',paste0('-',alternative))
    file <- paste0(iso_code,str_suffix,'-character.csv')
    df   <- read.csv(paste0(which_folder('data',filter),'/alphabets/',collection,'/',file))
    alphabet_size <- nrow(df)
    list("language"=language, 'A'=alphabet_size)
  })
  df = do.call(rbind.data.frame,parameters)
  write.csv(df, paste0(which_folder('results',filter),'/alphabet_sisez_',collection,'.csv'))
})


# + collections summary 
print('tables: collection summaries')
res <- lapply(COLLS, function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  sum_coll <- langs_df %>% mutate(iso_code = NULL) %>% rename(T = X.tokens, n = X.types)
  A_coll   <- read.csv(paste0(which_folder('results',filter),'/alphabet_sisez_',collection,'.csv'))
  sum_coll$A <- A_coll$A
  sum_coll <- sum_coll[,c('language','family','script','A','n','T')] %>% 
    arrange(family,script,language)
  write.csv(sum_coll,paste0(which_folder('results',filter),'/coll_summary_',collection,'.csv'))
  print(xtable(sum_coll, type = "latex"), 
        file = paste0(which_folder('latex_tables',filter),'/coll_summary_',collection,".tex"),
        include.rownames=FALSE,include.colnames=FALSE, only.contents = TRUE,
        hline.after = c(nrow(sum_coll)))
})






# EFFECT OF FILTERING  ---------------------------------------------------------
# assumes that optimality scores have been computed on both collections 
# with and without filtering

print('files: scores with filtered vs non filtered data')
res <- lapply(COLLS, function(collection) {
  lapply(length_defs, function(length_def) {
    length_def <- if (collection=='pud') 'characters' else length_def
    get_filtered_ori_df(collection,length_def) %>% 
      ggplot(aes(original,filtered,label=language)) + 
      geom_abline(intercept = 0,slope=1,color='purple') + geom_point() + geom_text_repel(size=3) + 
      theme(legend.position = 'bottom') + guides(color=guide_legend(nrow=2,byrow=TRUE))+
      facet_wrap(~score,scales='free',labeller = labeller(score=scores_labs))
    ggsave(paste0('figures/non_filtered/filteredVSoriginal_',collection,'_',length_def,'.pdf'), device = cairo_pdf)
  })
})



# CORRELATION SIGNIFICANCE --------------------------------------------------------
print('figures: correlation significance')
res <- lapply(c('kendall','pearson'), function(corr_type) {
  # cv
  collection <- 'cv'
  rows_cv <- lapply(length_defs, function(length_def) {
    df <- read_file('corr',collection,filter,length_def,corr_type)
    df$length_def <- ifelse(length_def=='medianDuration','duration',length_def)
    df
  })
  do.call(rbind,rows_cv) %>% plot_corr_significance(corr_type)
  ggsave(paste0(which_folder('figures',filter),'/corr_significance_',collection,'_',corr_type,'.pdf'))
  
  # pud
  collection <- 'pud'
  df <- read_file('corr',collection,filter,corr_type=corr_type)
  plot_corr_significance(df, corr_type)
  ggsave(paste0(which_folder('figures',filter),'/corr_significance_',collection,'_',corr_type,'.pdf'))
})


# MEAN AND RANDOM L ------------------------------------------------------
res <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    lapply(length_defs, function(length_def) {
      df      <- read_file('opt',collection,filter,length_def)
      ggplot(df,aes(Lrand,L,label=language)) + geom_point() + labs(x=bquote(L[r])) +
        geom_abline(slope=1,intercept=0,linetype='dashed',color='purple') +
        geom_text_repel(size=4,color='black',box.padding=0.3) + standart_theme
      ggsave(paste0(which_folder('figures',filter),'/mean_vs_random_L_',collection,"_",length_def,'.pdf'))
    })
  } else {
    df <- read_file('opt',collection,filter,'characters')
    ggplot(df,aes(Lrand,L,label=language)) + geom_point() + labs(x=bquote(L[r])) +
      geom_abline(slope=1,intercept=0,linetype='dashed',color='purple') +
      geom_text_repel(size=4,color='black',box.padding=0.3) + standart_theme
    ggsave(paste0(which_folder('figures',filter),'/mean_vs_random_L_',collection,'_characters.pdf'))
  }
})


# OPTIMALITY SCORES ------------------------------------------------------
# + scores for each language, collection, length_def
print('tables: optimality scores')
res <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      opt_df <- read_file('opt',collection,filter,length_def) %>% select(language,L,Lrand,tau,tau_pval,r,r_pval)
      opt_df <- merge(opt_df, select(langs_df_cv,language,family,script))
      opt_df <- opt_df[,c('language', 'family', 'script', 'L', 'Lrand', 'tau', 'tau_pval', 'r', 'r_pval')]
      print(xtable(opt_df,type = "latex",digits=c(0,0,0,0,2,2,2,-2,2,-2)),math.style.exponents = TRUE, 
            file = paste0(which_folder('latex_tables',filter),'/',collection,"_opt_scores",suffix,".tex"),
            include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(opt_df)))
      })
  } else {
    opt_df <- read_file('opt',collection,filter) %>% select(language,L,Lrand,tau,tau_pval,r,r_pval)
    opt_df <- merge(opt_df, select(langs_df_pud,language,family,script))
    opt_df <- opt_df[,c('language', 'family', 'script', 'L', 'Lrand', 'tau', 'tau_pval', 'r', 'r_pval')]
    print(xtable(opt_df,type = "latex",digits=c(0,0,0,0,2,2,2,-2,2,-2)),math.style.exponents = TRUE,
          file = paste0(which_folder('latex_tables',filter),'/',collection,"_opt_scores_characters.tex"),
          include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(opt_df)))
  }
})


# + summary of optimality scores
print('tables: optimality scores summaries')
res <- lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(9,1,2,3,4,5,6,7,8)]
  print(xtable(summ, type = "latex"),
        file = paste0(which_folder('latex_tables',filter),"/opt_scores_summary_",score,corr_suffix,".tex"),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,
        only.contents = TRUE,hline.after = c(nrow(summ)))
})


# + correlogram of optimality scores 
print('figures: correlogram between optimality scores')
res <- lapply(c('kendall','pearson'), function(plot_corr) {
  plot_corr_suffix <- paste0('_',plot_corr)
  rows <- lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        df <- read_file('opt',collection,filter,length_def) %>% select(L,eta,psi,omega)
        plot_correlogram(df,plot_corr,'scores',HB_correct = T,10,22,25)
        ggsave(paste0(which_folder('figures',filter),'/corrplot_',collection,"_",length_def,plot_corr_suffix,'.pdf'),device = cairo_pdf)
      })
    } else {
      length_def <- 'characters'
      df <- read_file('opt',collection,filter,length_def) %>% 
        filter(language %!in% c('Chinese-strokes','Japanese-strokes')) %>% 
        select(L,eta,psi,omega)
      plot_correlogram(df,plot_corr,'scores',HB_correct = T,10,22,25)
      ggsave(paste0(which_folder('figures',filter),'/corrplot_',collection,"_",length_def,plot_corr_suffix,'.pdf'),device = cairo_pdf)
    }
    })
})





# SCORES DISTRIBUTION ---------------------------------------------------------
# + density plots
print('figures: scores density plots')
rows <- lapply(COLLS, function(collection) {
  if (collection =='cv') {
    rows <- lapply(length_defs, function(length) {
      length_lab <- ifelse(length=='medianDuration','duration',length)
      read_file('opt',collection,filter,length) %>% 
        select(language,eta,psi,omega) %>% mutate(collection = toupper(collection), `length definition` = length_lab)
    })
    do.call(rbind.data.frame,rows)
  } else if (collection =='pud') {
    read_file('opt',collection,filter,'characters') %>% 
      select(language,eta,psi,omega) %>% mutate(collection = toupper(collection), `length definition` = 'characters') %>%
      filter(language %!in% non_imm_langs)
  }
})
df <- do.call(rbind.data.frame,rows) %>% mutate(collection=factor(collection, levels=c('PUD','CV'))) %>% 
  reshape2::melt(id.vars = c('language','collection','length definition'))
means <- df %>% group_by(collection,`length definition`,variable) %>% summarise(meanvalue = mean(value))
ggplot(df,aes(x=value,color = `length definition`, fill = `length definition`)) + geom_density(alpha = 0.2) + 
  facet_grid(rows = vars(collection), cols = vars(variable),
             labeller = labeller(variable=scores_labs)) + standart_theme +
  geom_vline(data=means, aes(xintercept=meanvalue, color = `length definition`),linetype='dashed') +
  theme(legend.position = 'bottom') + theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=0.5))
ggsave(paste0(which_folder('figures',filter),'/opt_scores_density',corr_suffix,'.pdf'), 
       device = cairo_pdf, width = 8, height = 5)


# + Psi values and composition
print('figures: psi values and composition')
score <- 'psi'
length_def   <- 'characters'
opt_df <- read_file('opt','pud',filter,length_def)
# plot 1
plot_score(score,opt_df)
ggsave(paste0(which_folder('figures',filter),'/',score,'_pud_',length_def,'.pdf'), 
       device = cairo_pdf, width = 4, height = 5)
# plot 2
plot_score_composition(score,opt_df)
ggsave(paste0(which_folder('figures',filter),'/',score,'_composition_pud_',length_def,'.pdf'),
       width = 4, height = 5)



# + kendall vs spearman table
print('tables: kendall vs spearman in pud')
# compute spearman
corrs_spearman <- compute_corr('pud','spearman','characters',F,filter) %>% select(-pvalue)
opt_spearman  <- compute_optimality_scores_coll(corrs_spearman,'pud','characters',F,filter) %>% 
  rename(omega_ro=omega) %>% select(language, omega_ro)
corrs_spearman <- rename(corrs_spearman, ro = corr, ro_min = corr_min)
# read kendall
opt_kendall  <- read_file('opt','pud',filter,'characters','kendall') %>% 
  rename(omega_tau=omega) %>% select(language, tau, tau_min, omega_tau)
# merge
df <- merge(corrs_spearman,opt_spearman, by ='language') %>% merge(opt_kendall) %>% 
  merge(langs_df_pud[,c('language','family','script')])
df <- df[,c('language','family','script','tau','ro','tau_min','ro_min','omega_tau','omega_ro')] %>% 
  arrange(family,script,language)
# save
print(xtable(df,type = "latex"), 
      file = paste0(which_folder('latex_tables',filter),"/omega_tau_omega_ro_pud.tex"),
      include.rownames=FALSE, include.colnames=FALSE, only.contents = TRUE,hline.after = c(nrow(df)))




# FINDING THE BEST SCORE  --------------------------------------------------------------
## + correlation of scores with basic parameters (n tokens, n types, alphabet)
print('figures: correlograms of scores and basic language parameters')
res <- lapply(COLLS, function(collection) {
  params_df <-   read.csv(paste0(which_folder('results',filter),'/coll_summary_',collection,'.csv')) %>% 
    select(-X,-family,-script)
  lapply(c('kendall','pearson'), function(plot_corr) {
    plot_corr_suffix <- paste0('_',plot_corr)
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        # data
        opt_df <- read_file('opt',collection,filter,length_def) %>% select(language,eta,psi,omega)
        df <- merge(params_df,opt_df, by='language') %>% select(-language)
        # plot
        plot_correlogram(df,plot_corr,'params',HB_correct=T,8,21,18)
        ggsave(paste0(which_folder('figures',filter),'/corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf'), 
               device = cairo_pdf, width = 6, height = 6)
      })
    } else {
      length_def <- 'characters'
      # data
      params_df <- params_df %>% 
        filter(language %!in% c('Japanese-strokes','Chinese-strokes','Chinese-pinyin','Japanese-romaji'))
      opt_df <- read_file('opt',collection,filter,length_def) %>% select(language,eta,psi,omega)
      df <- merge(params_df,opt_df, by='language') %>%  select(-language)
      # plot
      plot_correlogram(df,plot_corr,'params',HB_correct=T,8,21,18)
      ggsave(paste0(which_folder('figures',filter),'/corrplot_params_',collection,'_',length_def,plot_corr_suffix,'.pdf'),
             device = cairo_pdf, width = 6, height = 6)
    }
  })
})


## + convergence analysis 
print('figures: convergence of scores')
sample_sizes <- c(2^seq(3,23))
rows <- lapply(COLLS, function(collection) {
  languages <- if (collection=='pud') langs_df_pud$language else langs_df_cv$language
  languages <- sort(languages)
  if (collection == 'cv') {
    print(collection)
    lapply(length_defs, function(length_def) {
      suffix     <- paste0("_",length_def)
      # df
      scores_df <- read.csv(paste0(which_folder('results',filter),'/scores_convergence_',collection,suffix,'.csv'))[-1]  %>% 
        mutate(t = rep(sample_sizes,length(languages))) %>% 
        reshape2::melt(id.vars=c('language','t')) %>% 
        rename(score = value) %>% na.omit()
      # plot
      plot_convergence(scores_df)
       ggsave(paste0(which_folder('figures',filter),'/convergence_',collection,suffix,'.pdf'),
             device = cairo_pdf, width = 11, height = 12)
    })
  } else if (collection == 'pud') {
    print(collection)
    suffix <- '_characters'
    # df
    scores_df <- read.csv(paste0(which_folder('results',filter),'/scores_convergence_',collection,suffix,'.csv'))[-1]  %>% 
      mutate(t = rep(sample_sizes,length(languages))) %>% 
      reshape2::melt(id.vars=c('language','t')) %>% 
      rename(score = value) %>% na.omit()
    # plot
    plot_convergence(scores_df)
    ggsave(paste0(which_folder('figures',filter),'/convergence_',collection,suffix,'.pdf'),
           device = cairo_pdf, width = 10, height = 6)
  }
})





# DURATION VERSUS CHARACTERS (weak recoding) ------------------------------------------------------
score <- 'psi'
print(paste0('figures: ',score,' in duration versus characters'))
rows_cv <- lapply(c('medianDuration','meanDuration'), function(length_def) {
  plot_timeVSspace(score,length_def,filter,robust=T)
  ggsave(paste0(which_folder('figures',filter),'/',score,'_timeVSspace_',length_def,'.pdf'),device = cairo_pdf)
})


## correlation between rankings
psi_values <- lapply(length_defs, function(length_def) {
  df <- read_file('opt','cv',filter,length_def)
  df$psi
})
df_psi <- do.call(cbind,psi_values) 
cor  <- cor(df_psi,method='kendall')[2,1]
pval <- cor_pmat(df_psi,method='kendall')[2,1]
print(paste0('relation between psi in characters and in duration: ',cor, 
             'with pvalue:',pval))




# VOWELS REMOVAL (weak recoding) --------------------------------------------------------------
print('files: vowel removal analysis')
# - 1 - Significance of word lengths
tau_df <- compute_corr("pud", corr_type = "kendall", remove_vowels = TRUE, filter)
write.csv(tau_df, paste0(which_folder('results',filter),'/correlation_pud_remove_vowels.csv'))

# - 2 - Compute scores
opt_df <- compute_optimality_scores_coll("pud", corr_type = "kendall", remove_vowels = TRUE, filter)
write.csv(opt_df, paste0(which_folder('results',filter),'/optimality_scores_pud_remove_vowels.csv'))

# plot comparison
print('figures: vowel removal analysis')
res <- lapply(c('eta','omega','psi'), function(score){
  df <- form_table(score,filter)
  plot_score_comparison(score,df)    
  ggsave(paste0(which_folder('figures',filter),'/scores_comparison_pud_',score,'.pdf'), 
         width = 5, height = 7, device = cairo_pdf)
})





# NULL HYPOTHESYS --------------------------------------------------------------

iters <- 1e+06

## + summary opt scores null
print('tables: summary of scores expected values')
res <- lapply(c('omega','eta','psi'), function(score) {
  summ <- opt_score_summary(score,null=T,iters = iters) %>% mutate(empty = rep('',3)) 
  summ <- summ[,c(9,1,2,3,4,5,6,7,8)]
  print(xtable(summ, type = "latex",digits=3), 
        file = paste0(which_folder('latex_tables',filter),"/opt_scores_summary_null_",score,corr_suffix,".tex"),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,
        only.contents = TRUE,hline.after = c(nrow(summ)))
})


## correlation wit Lmin, Lr, and Lmin/Lr
print('figures: correlograms of scores expectations with baselines')
res <- lapply(c(1e+06), function(iters) {
  lapply(c('kendall','pearson'), function(plot_corr) {
    plot_corr_suffix <- paste0('_',plot_corr)
    rows <- lapply(COLLS, function(collection) {
      if (collection == 'cv') {
        lapply(length_defs, function(length_def) {
          df <- read_file('null',collection,length_def,filter,iters)
          plot_correlogram(df,plot_corr,'null',HB_correct=T,8,22,18)
            ggsave(paste0(which_folder('figures',filter),'/corrplot_null_',collection,'_',length_def,'_',iters,plot_corr_suffix,'.pdf'), 
                   device = cairo_pdf, width = 6, height = 6)
        })
      } else {
        length_def <- 'characters'
        suffix       <- paste0("_",length_def)
        df <- read_file('null',collection,length_def,filter,iters) 
        plot_correlogram(df,plot_corr,'null',HB_correct=T,8,22,18)
          ggsave(paste0(which_folder('figures',filter),'/corrplot_null_',collection,suffix,'_',iters,plot_corr_suffix,'.pdf'), 
                 device = cairo_pdf, width = 6, height = 6)
      }
    })
  })
})


## evolution of correlation 
print('figures: evolution of correlation over number of randomizations')
## + cv
res <- lapply(length_defs, function(length_def) {
  dfs <- lapply(c(1000,10000,1e+05,1e+06), function(iters) {
    dfs <- lapply(c('kendall','pearson'), function(plot_corr) {
      read_file('null','cv',filter=filter,length=length_def,iters=iters) %>% 
        long_corr_df(plot_corr,HB_correct = T)
    })
    do.call(rbind,dfs) %>% mutate(randomizations=iters)
  })
  p <- do.call(rbind,dfs) %>% plot_corr_evolution()
  ggsave(paste0(which_folder('figures',filter),'/corr_evolution_cv_',length_def,'.pdf'), 
         device = cairo_pdf, width = 9, height = 5)
})

## + pud
dfs <- lapply(c(1000,10000,1e+05,1e+06), function(iters) {
  dfs <- lapply(c('kendall','pearson'), function(plot_corr) {
    read_file('null','pud',filter=filter,length='characters',iters=iters) %>% 
      long_corr_df(plot_corr,HB_correct = T)
  })
  do.call(rbind,dfs) %>% mutate(randomizations=iters)
})
df <- do.call(rbind,dfs)
plot_corr_evolution(df)
ggsave(paste0(which_folder('figures',filter),'/corr_evolution_pud_characters.pdf'), 
       device = cairo_pdf, width = 9, height = 5)



## E[scores] vs Lmin
print('figures: correlation between scores expectations and Lmin')
iters <- 1e+06
rows_cv <- lapply(length_defs, function(length_def) {
  df <- read_file('null','cv',filter,length_def,iters=iters)%>% 
    mutate(`Lmin/Lrand` = Lmin/Lrand) %>% select(language,Lmin,psi,omega) %>% 
    rename(`E[psi]`=psi, `E[omega]`=omega) %>% mutate(length_def = length_def) %>% 
    merge(langs_df_cv[,c('language','X.tokens')], by = 'language')
})
df <- do.call(rbind,rows_cv)
reshape2::melt(df, id.vars=c('language','Lmin','length_def','X.tokens')) %>% 
  ggplot(aes(x=`Lmin`,y=value,label=ifelse(log10(X.tokens)<=4,language,''),fill=log10(X.tokens))) + 
  geom_text_repel(size=3,color='black',box.padding=0.5)+
  geom_point(colour="black",pch=21,size=3) + geom_hline(yintercept = 0,color='purple',linetype='dashed') +
  facet_grid(cols=vars(length_def),rows=vars(variable),scales = 'free', 
             labeller = labeller(variable=exp_scores_labs, length_def=length_labs)) +
  scale_fill_gradient2(low = "red", mid = "yellow", high = "green", midpoint = 5) + 
  labs(fill=expression(paste(log[10],'T')), x=bquote(L[min]), y = 'expected score value') +
  scale_y_continuous(label=scientific_10) +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 13))
ggsave(paste0(which_folder('figures',filter),'/correlation_scores_Lmin_',iters,'.pdf'), 
       device = cairo_pdf)



## E[eta] vs theoretical lower bound
print('figures: E[eta] vs theoretical lower bound')
rows <- lapply(COLLS, function(collection) {
  if (collection == 'pud') {
    length_def <- 'characters'
    suffix       <- paste0("_",length_def)
    read_file('null',collection,filter,length_def,iters=iters) %>% plot_etaVSlowerbound()
    ggsave(paste0(which_folder('figures',filter),'/E_eta_LminLr_',collection,suffix,'.pdf'), 
           device = cairo_pdf, width = 5, height = 5)
  } else {
    lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      read_file('null',collection,filter,length_def,iters=filter) %>% plot_etaVSlowerbound()
      ggsave(paste0(which_folder('figures',filter),'/E_eta_LminLr_',collection,suffix,'.pdf'), 
             device = cairo_pdf, width = 5, height = 5)
    })
  }
})



# INGREDIENTS OF OPTIMALITY SCORES ---------------------------------------------
#Panel (a) for eta: L_min on the x axis and L on the y axis.
#Panel (b) for Psi: L_r - L on the x axis and L_r - L_min on the y axis.
#Panel (c) for Psi: \tau_min on the x axis and \tau on the y axis.


rows <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    lapply(length_defs, function(length_def) {
      plot_scores_ingredients(filter, collection, length_def)
    })
  } else {
    length_def <- 'characters'
    plot_scores_ingredients(filter, collection, length_def)
  }
})


