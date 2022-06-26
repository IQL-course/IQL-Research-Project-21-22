
source('R_functions.R')

# PLAN
# - 0 - Check data
    # + Filter cv based on standard deviation

# - 1 - Optimality of word length
    # + compute Omega, eta, L 
    # + correlation between the scores (can we replace them with omega?)

# - 2 - The significance of word lengths
    # + tau correlation test: law of abbreviation
    # + pearson correlation test: if ro is significantly low Omega is significantly large


# - 3 - Sorting languages by their degree of optimality
    # + plot language ranks?

# - 4 - Stability under the null hypothesis
# Ramon wrote:
#To show that that E[𝜂] is is not stable under the null hypothesis we estimate E[𝜂] and E[Ω] with a
#Monte Carlo procedure over 106 randomizations. Table ??? confirms numerically the estimated E[𝜂] is
#not constant and depends on 𝐿𝑚𝑖𝑛 and 𝐿𝑟 ; in contrast, the estimated E[Ω] converges to 0 while being
#independent from 𝐿𝑚𝑖𝑛 and 𝐿𝑟 
    # + compute Omega with random matching of lengths and frequencies




# RESULTS TO PRODUCE
# - X tables of optimality scores for every collection and length definition (prepare tables for Latex)
# - X density plot of omega 
# - X distribution of omega values
# - X tables of tau correlation with pvalues for every collection and length definition (plots)
# - O density plot of null hypothesis (Mengxue)
# - O how to show rankings by degree of optimality? (Mengxue)
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
# - O add pinyin and romaji to code logics


# NOTES FOR REPORT
# - criterion to remove types based on sd is based on the assumption of linear relation between sd and mean
# - use median, it's more robust (move mean to appendix)



# IMPLEMENTATION

# + collections summary 
lapply(COLLS, function(collection) {
  langs_df <- if (collection == 'pud') langs_df_pud else if (collection == 'cv') langs_df_cv
  sum_coll <- langs_df %>% mutate(dialect = NULL, iso_code = NULL) %>% rename(tokens = X.tokens, types = X.types)
  sum_coll <- sum_coll[,c('language','family','script','types','tokens')] %>% 
    filter(stringr::str_detect(language,'-') == F) %>% arrange(family,script,language)
  print(xtable(sum_coll, type = "latex"), 
        file = here('latex_tables',paste0('coll_summary_',collection,".tex")),
        include.rownames=FALSE,include.colnames=FALSE, only.contents = TRUE)
  
})




# - 1 - optimality scores ------------------------------------------------------
corr_type <- 'kendall'
corr_suffix <- paste0('_',corr_type)

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
  summ <- opt_score_summary(score,corr_type)
  print(xtable(summ, type = "latex"), 
        file = here('latex_tables',paste0("opt_scores_summary_",score,corr_suffix,".tex")),
        caption.placement = "top",include.rownames=FALSE,include.colnames=FALSE,only.contents = TRUE)
})


# + correlogram of opt scores (mean is missing)
rows <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    rows_cv <- lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
      df <- df %>% dplyr::select(L,eta,psi,omega)
      cors <- round(cor(df), 2)
      p.mat <- cor_pmat(df)
      ggcorrplot(cors, type = "lower", p.mat = p.mat, lab=T, lab_size = 10, tl.cex = 20, pch.cex = 20) + 
        labs(title=paste(collection,length_def,sep='-')) + theme(plot.title = element_text(size=22))
      ggsave(here('figures',paste0('corrplot_',collection,suffix,corr_suffix,'.pdf')))
    })
    do.call(rbind,rows_cv)
  } else {
    length_def <- 'characters'
    suffix       <- paste0("_",length_def)
    df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
    df <- df %>% dplyr::select(L,eta,psi,omega)
    cors <- round(cor(df), 2)
    p.mat <- cor_pmat(df)
    ggcorrplot(cors, type = "lower", p.mat = p.mat, lab=T, lab_size = 10, tl.cex = 20, pch.cex = 20) + 
      labs(title=paste(collection,length_def,sep='-')) + theme(plot.title = element_text(size=22))
    ggsave(here('figures',paste0('corrplot_',collection,suffix,corr_suffix,'.pdf')))
  }
})


# + Omega in time vs Omega in chars
lapply(c('omega','psi'), function(score) {
  plot_timeVSspace(score, corr_type)
  ggsave(here('figures',paste0(score,'_timeVSspace',corr_suffix,'.pdf')))
})


# - 2 - correlation significance --------------------------------------------------------
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
  scale_fill_gradient2(midpoint=0, low="blue",high = "red", mid = "white", na.value = "#b9d0ed")
ggsave(here('figures',paste0('corr_significance_',collection,corr_suffix,'.pdf')))

## pud
collection <- 'pud'
df <- read.csv(here('results',paste0('correlation_',collection,'_characters',corr_suffix,'.csv')))[-1] %>% 
  mutate(length_def = 'characters') %>% assign_stars()
ggplot(df,aes(y=language, x=length_def, fill=corr)) + 
  labs(x="length definition", y="language", title=paste0(corr_type,' correlation - ',collection)) +
  geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
  scale_fill_gradient2(midpoint=0, low="blue",high = "red", mid = "white", na.value = "#b9d0ed")
ggsave(here('figures',paste0('corr_significance_',collection,corr_suffix,'.pdf')))



# - 3 ?? - Sorting languages by their degree of optimality
#rank_omega <- get_ranked_langs(opt_df,'omega')$language
#rank_eta   <- get_ranked_langs(opt_df,'eta')$language
#plotRanks(rank_eta, rank_omega, labels.offset=0.35, title="Eta  -  Omega")







# PLOTS ------------------------------

# - DISTRIBUTION OF FREQUENCY VS LENGTH

## CV
#res <- lapply(ISO_cv[1:6], function(iso) read_language(iso,'cv') %>% select(-stDevDuration))
#all_df <- do.call(rbind,res) %>% 
#  melt(id.vars = c('frequency','word','language')) %>% 
#  rename(length_type = variable)  
#ggplot(all_df,aes(x=frequency, y=value, color=length_type)) + labs(y='length') +
#  geom_point() + #facet_wrap(~language,labeller = labeller(language=labs_cv)) + 
#  facet_grid(rows=vars(length_type), cols=vars(language),labeller = labeller(language=labs_cv)) +
#  scale_x_log10() + scale_y_log10()
#ggsave(here('figures','lengthVSfrequency_cv.pdf'))
#
### PUD
#res <- lapply(ISO_pud[1:6], function(iso) read_language(iso,'pud') %>% mutate(language = iso))
#all_df <- do.call(rbind,res) 
#ggplot(all_df,aes(x=frequency, y=length)) +
#  geom_point(color='blue') + facet_wrap(~language,labeller = labeller(language=labs_pud)) + 
#  scale_x_log10() + scale_y_log10()
#ggsave(here('figures','lengthVSfrequency_pud.pdf'))
#



# DENSITY PLOT OF OMEGA PSI and ETA
rows <- lapply(COLLS, function(collection) {
  rows <- lapply(length_defs, function(length) {
    length <- ifelse(collection == 'pud','characters',length)
    suffix <- paste0("_",length)
    read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1] %>% 
      select(language,eta,psi,omega) %>% mutate(collection = collection, `length definition` = length) %>%
      filter(language %!in% c('Chinese-strokes','Japanese-strokes'))
  })
  do.call(rbind.data.frame,rows)
})
df <- do.call(rbind.data.frame,rows) %>%
  melt(id.vars = c('language','collection','length definition'))
means <- df %>% group_by(collection,`length definition`,variable) %>% summarise(meanvalue = mean(value))
ggplot(df) + geom_density(aes(x=value,color = `length definition`, fill = `length definition`),alpha = 0.2) + facet_grid(rows = vars(collection), cols = vars(variable)) +
  geom_vline(data=means, aes(xintercept=meanvalue, color = `length definition`),linetype='dashed') +
  theme(legend.position = 'bottom') + theme(axis.text.x = element_text(angle = 60, vjust = 0, hjust=0))
ggsave(here('figures',paste0('opt_scores_density',corr_suffix,'.pdf')))



# Score BARS
rows <- lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    rows_cv <- lapply(length_defs, function(length_def) {
      suffix     <- paste0("_",length_def)
      plot_title <- paste0(collection,' - ',length_def)
      opt_df  <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
      corr_df <- read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]      # to remove if add tau and tau_min before
      opt_df  <- merge(opt_df,corr_df, by = c('language','family','script')) %>%                         # to remove if add tau and tau_min before
        select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)  # to remove if add tau and tau_min before
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
    opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,corr_suffix,'.csv')))[-1]
    corr_df  <- read.csv(here('results',paste0('correlation_',collection,suffix,corr_suffix,'.csv')))[-1]      # to remove if add tau and tau_min before
    opt_df  <- merge(opt_df,corr_df, by = c('language','family','script')) %>%                                 # to remove if add tau and tau_min before
      select(-pvalue,-hb_pvalue) %>% mutate(corr_min = corr/omega) %>% arrange(family,script,language)         # to remove if add tau and tau_min before
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



# kendall and spearman tables (language, family, script, tau, tau_min, ro, ro_min, Omega_tau, Omega_ro)
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




# correlation of scores with basic parameters (n tokens, n types, alphabet, L, eta, psi, omega)

lapply(c('kendall','pearson'), function(corr_type) {
  corr_suffix <- paste0('_',corr_type)
  lapply(COLLS, function(collection) {
    if (collection == 'cv') {
      lapply(length_defs, function(length_def) {
        plot_corrplot_params(collection,length_def,corr_type)
        ggsave(here('figures',paste0('corrplot_params_',collection,'_',length_def,corr_suffix,'.pdf')))
      })
    } else {
      length_def <- 'characters'
      plot_corrplot_params(collection,length_def,corr_type)
      ggsave(here('figures',paste0('corrplot_params_',collection,'_',length_def,corr_suffix,'.pdf')))
    }
  })
})



# CONVERGENCE of scores!
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




# shuffle len col, fre col: desc

languages <- langs_df_pud$language
scores_null <- lapply(languages, function(iso_code) {
  lang_scores <- compute_expectation_scores_lang(iso_code,'pud','characters') %>%
      select(language,eta,psi,L,omega)
  do.call(rbind.data.frame,lang_scores)
})

