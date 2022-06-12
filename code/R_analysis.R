
source('code/R_functions.R')

# PLAN
# - 1 - Optimality of word length
    # + compute Omega, eta, L 

# - 2 - The significance of word lengths
    # + pearson/tau (?) correlation test

# - 3 - Sorting languages by their degree of optimality
    # + plot language ranks?

# - 4 - Stability under the null hypothesis
    # + compute Omega with random matching of lengths and frequencies




# RESULTS TO PRODUCE
# - X tables of optimality scores for every collection and length definition (prepare tables for Latex)
# - X density plot of omega 
# - O tables of tau correlation with pvalues for every collection and length definition (prepare tables for Latex)
# - O density plot of null hypothesis (Mengxue)
# - O how to show rankings by degree of optimality?







# CHECKS ON DATA
# sd with respect to mean value?
df <- read.csv(here('results','coefficient_variation.csv'))
cutoffs <- seq(0,0.3,0.03)
share_retained_data <- sapply(cutoffs, function(cutoff) round(nrow(filter(df,coeff_var<=cutoff))/nrow(df),3) )
data.frame(cutoffs,share_retained_data) %>%
ggplot(aes(share_retained_data,cutoffs)) + geom_point() + geom_line()
ggsave('figures/coefficient_variation.pdf')




# IMPLEMENTATION

# - 3 - Sorting languages by their degree of optimality
rank_omega <- get_ranked_langs(opt_df,'omega')$language
rank_eta   <- get_ranked_langs(opt_df,'eta')$language
plotRanks(rank_eta, rank_omega, labels.offset=0.35, title="Eta  -  Omega")







# PLOTS ------------------------------

# - DISTRIBUTION OF FREQUENCY VS LENGTH
# bind all dfs and add language
res <- lapply(langs, function(lang) read_df(lang,'pud')[-1])
all_df <- do.call(rbind,res) %>% 
  mutate(index = 1:nrow(.), language = rep(langs,sapply(res, nrow))) %>%
  filter(length != 0)

# identify obs to mark (sample top 90% percentile of freq and length)
top_freq_id <- all_df %>% group_by(language) %>%
  group_modify(~head(.x,2)) %>% dplyr::select(index)

top_length_id <- all_df %>% arrange(desc(length)) %>% 
  group_by(language) %>% group_modify(~head(.x,2)) %>% dplyr::select(index)

all_df$lab <- ifelse(all_df$index %in% c(top_freq_id$index,top_length_id$index),
                     all_df$word,"")

lang_to_use <- subset(all_df, language %in% c("Turkish","Czech","Chinese","Russian","Japanese","German"))
plot <- ggplot(lang_to_use,aes(x=frequency,y=length,label = lab)) + 
  geom_point(color = ifelse(lang_to_use$lab != "",  "red","grey50")) + facet_wrap(~language) + 
  geom_text_repel(max.overlaps = 10) + 
  scale_y_log10() + scale_x_log10()



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
do.call(rbind.data.frame,rows) %>%
melt(id.vars = c('language','collection','length_def')) %>%
  ggplot() + geom_density(aes(x=value,color = length_def)) + facet_grid(rows = vars(collection), cols = vars(variable))
ggsave('figures/opt_scores_density.pdf')

