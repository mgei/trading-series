library(tidyverse)
library(rsims)


data(backtest_df_long)

backtest_df_long %>%
  filter(ticker %in% c("BTC", "ETH", "DOGE", "XRP", "LTC")) %>%
  mutate(log_price = log(price)) %>%
  select(-price) %>%
  pivot_longer(c(-date, -ticker), names_to = "param", values_to = "value") %>%
  ggplot(aes(x = date, y = value, colour = ticker)) +
  geom_line() +
  facet_wrap(~param, scale = "free_y", ncol = 1) +
  labs(
    x = "Date",
    y = "Value",
    title = "Log prices and target weights",
    colour = "Ticker"
  )


backtest_df_long <- backtest_df_long %>%
  # double check that the df is arranged by date
  arrange(date) %>%
  # lag each ticker separately - otherwise we lag based on row order
  group_by(ticker) %>%
  # lag - specify dplyr::lag as there is also base::lag
  mutate(lagged_weight = dplyr::lag(weight, 1))

backtest_df_wide <- backtest_df_long %>% 
  tidyr::pivot_wider(id_cols = date, names_from = ticker, values_from = c(price, weight)) 

# get weights as a wide matrix
# note that date column will get converted to unix timestamp
backtest_theo_weights <- backtest_df_wide %>% 
  # select only weight columns
  select(date, starts_with("weight_")) %>% 
  # remove the weight_ bit from the column names
  rename_with(.cols = -date, .fn = ~ str_remove(., "weight_")) %>%
  data.matrix()

# get prices as a wide matrix
# note that date column will get converted to unix timestamp
backtest_prices <- backtest_df %>% 
  # select only price columns
  select(date, starts_with("price_")) %>% 
  # remove the price_usd_ bit from the column names
  rename_with(.cols = -date, .fn = ~ str_remove(., "price_usd_")) %>%
  data.matrix()


# simulation parameters
initial_cash <- 10000
capitalise_profits <- FALSE  # remain fully invested?
trade_buffer <- 0.
commission_pct <- 0.

# simulation
results_df <- rsims::fixed_commission_backtest(
  backtest_prices, 
  backtest_theo_weights, 
  trade_buffer, 
  initial_cash, 
  commission_pct, 
  capitalise_profits
)

head(results_df, 10)
