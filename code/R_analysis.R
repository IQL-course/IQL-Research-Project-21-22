
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
# - O omega bar plots (left value of omega, right composition in bars) (Sonia)
# - O correlogram of opt scores (Sonia)
# - O plot of Omega_time vs Omega_chars with 45 diagonal (Sonia)


# NOTES FOR ANALYSIS
# - For Japanese and Chinese have both length in strokes and in characters
# - for Romansh mixe both dialects


# NOTES FOR REPORT
# - criterion to remove types based on sd is based on the assumption of linear relation between sd and mean
# - use median, it's more robust
# - in opt scores tables: sort by family, writing system, language name (retreive family info from Glottolog)
# - for Romansh we mixed both dialects, specify each dialect in materials, remove trom latex tables



# IMPLEMENTATION

# - 0 - Checks on data
# SD AND MEAN
df <- read.csv(here('results','coefficient_variation.csv'))
  # is relation between sd and mean linear?
#ggplot(sample_n(df, 10000)) + geom_point(aes(reorder(meanDuration,stDevDuration),stDevDuration))
  # coefficient of variation
cutoffs <- seq(0,0.3,0.03)
share_retained_data <- sapply(cutoffs, function(cutoff) round(nrow(filter(df,coeff_var<=cutoff))/nrow(df),3) )
data.frame(cutoffs,share_retained_data) %>%
ggplot(aes(share_retained_data,cutoffs)) + geom_point() + geom_line()
ggsave('figures/coefficient_variation.pdf')


# collections summary
langs_df_cv



# - 1 - optimality scores 

# scores for each language, collection, length_def
lapply(COLLS, function(collection) {
  if (collection == 'cv') {
    lapply(length_defs, function(length_def) {
      suffix       <- paste0("_",length_def)
      label_length <- switch(length_def,"meanDuration" = 'mean duration', 
                             "medianDuration" = 'median duration', 'n_chars' = 'number of characters')
      opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,suffix,'.csv')))[-1]
      
      print(xtable(opt_df, caption = paste0("CV: Optimality scores. Length is defined as ",label_length),
                   label = paste0("tab:opt_scores_",collection,suffix),type = "latex"), 
            file = here('latex_tables',paste0(collection,"_opt_scores",suffix,".tex")),
            caption.placement = "top",include.rownames=FALSE,
            table.placement = getOption("xtable.table.placement", "H"))
      })
  } else {
    opt_df <- read.csv(here('results',paste0('optimality_scores_',collection,'.csv')))[-1] %>% select(-dialect)
    print(xtable(opt_df, caption = "PUD: Optimality scores.",label = paste0("tab:opt_scores_",collection),type = "latex"), 
          file = here('latex_tables',paste0(collection,"_opt_scores.tex")),
          caption.placement = "top",include.rownames=FALSE,
          table.placement = getOption("xtable.table.placement", "H"))
  }
})

# summary opt scores
lapply(c('omega','eta'), function(score) {
  summ <- opt_score_summary(score)
  latex_score <- ifelse(score == 'omega','$\\Omega$','$\\eta$')
  print(xtable(summ, caption = paste0("Summary statistics of ",latex_score," for each collection and definition of length"),
               label = paste0("tab:opt_scores_summary_",score), type = "latex"), 
        file = here('latex_tables',paste0("opt_scores_summary_",score,".tex")),
        caption.placement = "top",include.rownames=FALSE,
        table.placement = getOption("xtable.table.placement", "H"), sanitize.text.function = function(x) {x})
})





# - 2 - tau correlation TO DO
## cv
collection <- 'cv'
rows_cv <- lapply(length_defs, function(length_def) {
  suffix <- paste0("_",length_def)
  read.csv(here('results',paste0('tau_correlation_',collection,suffix,'.csv')))[-1] %>% 
    mutate(length_def = length_def, language = paste0(language,ifelse(dialect=='','','-'),dialect))
})
df <- do.call(rbind,rows_cv) %>% assign_stars()
ggplot(df,aes(y=language, x=length_def, fill=tau)) + 
  labs(x="length definition", y="language", title=paste0('Tau correlation - ',collection)) +
  geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
  scale_fill_gradient2(midpoint=0, low="blue",high = "red", mid = "white", na.value = "#b9d0ed")
ggsave(here('figures',paste0('tau_significance_',collection,'.pdf')))

## pud
collection <- 'pud'
df <- read.csv(here('results',paste0('tau_correlation_',collection,'.csv')))[-1] %>% 
  mutate(length_def = 'n_chars') %>% assign_stars()
ggplot(df,aes(y=language, x=length_def, fill=tau)) + 
  labs(x="length definition", y="language", title=paste0('Tau correlation - ',collection)) +
  geom_tile() + scale_y_discrete(labels=labs) + geom_text(aes(label=stars)) +
  scale_fill_gradient2(midpoint=0, low="blue",high = "red", mid = "white", na.value = "#b9d0ed")
ggsave(here('figures',paste0('tau_significance_',collection,'.pdf')))



df %>% filter(language %in% c('vi','dv'))

# - 3 ?? - Sorting languages by their degree of optimality
#rank_omega <- get_ranked_langs(opt_df,'omega')$language
#rank_eta   <- get_ranked_langs(opt_df,'eta')$language
#plotRanks(rank_eta, rank_omega, labels.offset=0.35, title="Eta  -  Omega")







# PLOTS ------------------------------

# - DISTRIBUTION OF FREQUENCY VS LENGTH

## CV
# ISO_cv[1:6]
res <- lapply(c('vi','dv'), function(iso) read_language(iso,'cv') %>% select(-stDevDuration))
all_df <- do.call(rbind,res) %>% 
  melt(id.vars = c('frequency','word','language')) %>% 
  rename(length_type = variable)  
ggplot(all_df,aes(x=frequency, y=value, color=length_type)) + labs(y='length') +
  geom_point() + #facet_wrap(~language,labeller = labeller(language=labs_cv)) + 
  facet_grid(rows=vars(length_type), cols=vars(language),labeller = labeller(language=labs_cv)) +
  scale_x_log10() + scale_y_log10()
ggsave(here('figures','lengthVSfrequency_cv.pdf'))

## PUD
res <- lapply(ISO_pud[1:6], function(iso) read_language(iso,'pud') %>% mutate(language = iso))
all_df <- do.call(rbind,res) 
ggplot(all_df,aes(x=frequency, y=length)) +
  geom_point(color='blue') + facet_wrap(~language,labeller = labeller(language=labs_pud)) + 
  scale_x_log10() + scale_y_log10()
ggsave(here('figures','lengthVSfrequency_pud.pdf'))




# DENSITY PLOT OF OMEGA AND ETA
rows <- lapply(COLLS, function(collection) {
  rows <- lapply(length_defs, function(length) {
    suffix <- ifelse(collection == 'pud','',paste0("_",length))
    length <- ifelse(collection == 'pud','n_chars',length)
    read.csv(here('results',paste0('optimality_scores_',collection,suffix,'.csv')))[-1] %>% 
      select(language,eta,omega) %>% mutate(collection = collection, length_def = length) 
  })
  do.call(rbind.data.frame,rows)
})
df <- do.call(rbind.data.frame,rows) %>%
  melt(id.vars = c('language','collection','length_def'))
means <- df %>% group_by(collection,length_def,variable) %>% summarise(meanvalue = mean(value))
ggplot(df) + geom_density(aes(x=value,color = length_def)) + facet_grid(rows = vars(collection), cols = vars(variable)) +
  geom_vline(data=means, aes(xintercept=meanvalue, color = length_def),linetype='dashed')
ggsave(here('figures','opt_scores_density.pdf'))

