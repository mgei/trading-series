library(tidyverse)
# library(tidyquant)
library(RedditExtractoR)

# download Reddit: there's a wait time of 2 seconds between pages
# so this will take at least 200 seconds
# reddit <- get_reddit(subreddit = "wallstreetbets", page_threshold = 100)
# reddit1 <- get_reddit(subreddit = "wallstreetbets", page_threshold = 1, cn_threshold = 100)
# 
# reddit %>% saveRDS("data/reddit.RDS") 
reddit <- readRDS("data/reddit.RDS") %>% 
  as_tibble() %>% 
  mutate_at(vars(contains("date")), as.Date)

# download stock list from here: https://www.nasdaq.com/market-activity/stocks/screener
# (previously one could do that with tq_exchange() but this does not work anymore)
stocks <- read_csv("data/nasdaq_screener_1613631247346.csv")

stocks %>% 
  filter(Symbol == "GME")

stocks %>% 
  filter(Symbol == "AAPL")

reg_expression <- regex(paste0("\\b(?:",
                               paste(stocks$Symbol, collapse = "|"),
                               ")\\b"))

# reddit_mentions <- reddit %>%
#   mutate(stock_mention = str_extract_all(comment, reg_expression)) %>%
#   unnest(stock_mention)
# 
# reddit_mentions %>% saveRDS("data/reddit_mentions.RDS")
reddit_mentions <- readRDS("data/reddit_mentions.RDS")

reddit_mention_counts <- reddit_mentions %>% 
  group_by(post_date, stock_mention) %>% 
  count()

# false positives (non-stock related):
fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL", "USA", "IT",
        LETTERS)

top5 <- reddit_mention_counts %>% 
  group_by(stock_mention) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(-n) %>%
  filter(!(stock_mention %in% fp)) %>% 
  head(5) %>% 
  pull(stock_mention)
  
reddit_mention_counts %>% 
  filter(stock_mention %in% top5) %>% 
  ggplot(aes(x = post_date, y = n, color = stock_mention)) +
  geom_line() +
  theme_classic()

reddit_mentions %>% 
  filter(!(stock_mention %in% fp)) %>% 
  group_by(stock_mention) %>% 
  count() %>% 
  arrange(-n) %>% 
  print(n = 20)


reddit_mentions %>% 
  filter(!(stock_mention %in% fp)) %>% 
  saveRDS("data/reddit_mentions.RDS")
