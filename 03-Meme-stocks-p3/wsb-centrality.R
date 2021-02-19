library(tidyverse)
library(tidygraph)

reddit <- readRDS("../01-Meme-stocks-p1/data/reddit.RDS") %>% 
  as_tibble()

user_postcount <- reddit %>% 
  group_by(user) %>% 
  count() %>% 
  arrange(-n)

library(RedditExtractoR)

# net <- user_network(reddit)
# 
# net_tidy <- net$df %>%
#   transmute(from = sender, to = receiver) %>%
#   as_tbl_graph()
# net_tidy %>% saveRDS("data/net_tidy.RDS")
net_tidy <- readRDS("data/net_tidy.RDS")


user_centralities <- net_tidy %>% 
  mutate(centrality = centrality_eigen()) %>% 
  as_tibble()

user_centralities <- user_centralities %>% 
  arrange(-centrality)

user_postcount %>% 
  left_join(user_centralities, 
            by = c("user" = "name")) %>% 
  arrange(-centrality) %>% 
  print(n = 20)

reddit_mentions_sentiment <- readRDS("../02-Meme-stocks-p2/data/reddit_mentions_sentiment.RDS")

reddit_sentiment_counts_wgt <- reddit_mentions_sentiment %>% 
  left_join(user_centralities,
            by = c("user" = "name")) %>% 
  mutate(sentiment_wgt = sentiment*centrality) %>% 
  group_by(comm_date, stock_mention) %>% 
  summarise(sentiment_wgt = mean(sentiment_wgt),
            sentiment = mean(sentiment),
            n = n())

top5 <- reddit_sentiment_counts_wgt %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  head(5) %>% 
  pull(stock_mention)

reddit_sentiment_counts_wgt %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = comm_date, y = sentiment_wgt, color = stock_mention)) +
  # geom_line() +
  geom_smooth(se = F) +
  theme_classic()

reddit_sentiment_counts_wgt %>% 
  filter(stock_mention %in% top5) %>% 
  # ggplot(aes(x = comm_date, y = sentiment_wgt, color = stock_mention)) +
  ggplot(aes(x = comm_date, y = sentiment, color = stock_mention)) +
  # geom_line() +
  geom_smooth(se = F) +
  theme_classic()
