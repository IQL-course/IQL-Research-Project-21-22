

# PLAN

# - 1 - Optimality of word length

# - 2 - The significance of word lengths

# - 3 - Sorting languages by their degree of optimality

# - 4 - Stability under the null hypothesis





# PLOTS ------------------------------

# bind all dfs and add language
res <- lapply(langs, function(lang) read_df(lang,'pud')[-1])
all_df <- do.call(rbind,res) %>% 
  mutate(index=1:nrow(.),language = rep(langs,sapply(res, nrow))) %>%
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




# - 1 - Compute Omega

opt_df <- compute_optimality_scores('pud')


