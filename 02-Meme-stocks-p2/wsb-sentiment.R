library(tidyverse)
library(vader)

# We found out which stocks get mentioned on WSB,
# but some mentions might be negative.
reddit_mentions <- readRDS("../01-Meme-stocks-p1/data/reddit_mentions.RDS")

# Vader is a lexicon and rule-based sentiment analysis tool that is specifically 
# attuned to sentiments expressed in social media (...).

get_vader("I like this stock")
get_vader("I really like this stock")
get_vader("would never buy this trash")
get_vader("abolute dog shit sucks")

# Download package source here:
# https://cran.r-project.org/web/packages/vader/index.html

load("data/vader/R/sysdata.rda")

vaderLexicon %>% 
  as_tibble()

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "yolo")

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "retard")

vaderLexicon %>% 
  as_tibble() %>% 
  filter(V1 == "call")

# let's add some words 
wsbLexicon <- bind_rows(tibble(V1 = c("retard", "retarded", "fuck", "fucking", "autist", "fag", "gay", "stonk"), V2 = 0, V3 = 0.5), # neutral 
                        tibble(V1 = c("bull", "bullish", "tendie", "tendies", "call", "long", "buy", "moon", "hold",              # positive
                                      "diamond", "hands", "yolo", "yoloed", "free", "btfd", "rocket", "elon", "gain",
                                      "420", "calls", "longs", "sky", "space", "roof", "squeeze", "balls"), V2 = 1.5, V3 = 0.5),                     
                        tibble(V1 = c("bear", "sell", "put", "short", "shorts", "puts", "bagholder", "wife", "boyfriend",         # negative
                                      "shorting", "citron", "hedge", "fake"), V2 = -1.5, V3 = 0.5))

# add back to lexicon
vaderLexiconWSB <- vaderLexicon %>% 
  as_tibble() %>% 
  # anti_join(wsbLexicon, by = "V1") %>% 
  filter(!(V1 %in% wsbLexicon$V1)) %>% 
  bind_rows(wsbLexicon) %>% 
  as.data.frame()

vaderLexicon <- vaderLexiconWSB

save(vaderLexicon, file = "data/vader/R/sysdata.rda")

# remove the vader package and reinstall (maybe restart R session after removing also)
detach("package:vader", unload = T)
remove.packages("vader")

install.packages("data/vader/", repos = NULL, type = "source")

library(vader)

get_vader("just yoloed GME, will not sell before 420 fucking diamond hands")
get_vader("SLV is a fake squeeze, hate hedge funds")

# comments_sentiment <- reddit_mentions %>%
#   select(comment) %>%
#   distinct() %>%
#   mutate(comment_clean = str_replace_all(comment, "\\\\", " ")) %>%
#   mutate(sentiment = vader_df(comment_clean)$compound)
# 
# comments_sentiment %>% saveRDS("data/comments_sentiment.RDS")
comments_sentiment <- readRDS("data/comments_sentiment.RDS")

reddit_mentions_sentiment <- reddit_mentions %>% 
  left_join(comments_sentiment %>% select(-comment_clean),
            by = "comment")

reddit_sentiment_counts <- reddit_mentions_sentiment %>% 
  group_by(comm_date, stock_mention) %>% 
  summarise(sentiment = mean(sentiment),
            n = n())

reddit_sentiment_counts %>% 
  filter(stock_mention == "TSLA") %>% 
  ggplot(aes(x = comm_date, y = sentiment)) +
  geom_line() +
  theme_classic()

top5 <- reddit_sentiment_counts %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  head(10) %>% 
  pull(stock_mention)

reddit_sentiment_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = comm_date, y = sentiment, color = stock_mention)) +
  # geom_line() +
  geom_smooth(se = F) +
  theme_classic()



reddit_mentions_sentiment %>% 
  saveRDS("data/reddit_mentions_sentiment.RDS")

