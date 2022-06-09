
source('R_functions.R')

# PLAN
# - 1 - Optimality of word length
    # + compute Omega, eta, L 

# - 2 - The significance of word lengths
    # + pearson/tau (?) correlation test

# - 3 - Sorting languages by their degree of optimality
    # + plot language ranks?

# - 4 - Stability under the null hypothesis
    # + compute Omega with random matching of lengths and frequencies



# IMPLEMENTATION
collection <- 'pud'

# - 1 - Compute Omega
opt_df <- compute_optimality_scores(collection)


# - 2 - Significance of word lengths
tau_df <- compute_tau_corr(collection)

# - 3 - Sorting languages by their degree of optimality
rank_omega <- get_ranked_langs(opt_df,'omega')$language
rank_eta   <- get_ranked_langs(opt_df,'eta')$language
plotRanks(rank_eta, rank_omega, labels.offset=0.35, title="Eta  -  Omega")







# PLOTS ------------------------------

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




