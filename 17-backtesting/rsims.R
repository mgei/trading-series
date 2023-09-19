library(rsims)
library(tidyverse)
library(tidyquant)

prices <- tq_get(c("SPY", "GLD", "IWM"))
prices_m <- prices %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  mutate(date = as.numeric(date)) %>% 
  as.matrix()

w_m <- prices_m
w_m[,2:4] <- 1/3

out <- cash_backtest(prices = prices_m, theo_weights = w_m, 
                     initial_cash = 1000, 
                     trade_buffer = 0.1, 
                     capitalise_profits = T)

out %>% 
  ggplot(aes(x = Date, y = Value, color = ticker)) +
  geom_line()
