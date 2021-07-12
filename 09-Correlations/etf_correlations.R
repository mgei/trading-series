library(tidyverse)
library(tidyquant)
library(scales)

stocks_chart <- function(data) {
  chart <- data %>% 
    group_by(name) %>% 
    mutate(p = cumprod(1+r)*1000) %>% 
    ggplot(aes(x = date, y = p, color = name)) +
    geom_line() +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    theme(legend.position = "top")
  return(chart)
}

# # let's get a few ETFs that could form a diversified portfolio
# tickers <- c("SPY", "GLD", "AGG", "EEM", "RWR", "USO", "TLT", "DBC", "VTV", "VNQ", "IJR")
# 
# pricedata <- tq_get(tickers, from = as.Date("1900-01-01"))
# 
# # we have data since 2006-05-02
# pricedata %>% 
#   group_by(symbol) %>% 
#   summarise(min = min(date)) %>% 
#   arrange(min)
# 
# returns <- pricedata %>% 
#   filter(date >= as.Date("2006-05-02")) %>% 
#   group_by(symbol) %>% 
#   transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
#   filter(!is.na(r))
# 
# returns_wide <- returns %>% 
#   pivot_wider(names_from = symbol, values_from  = r)

returns_wide <- readRDS("../08-Diversification/data/returns_wide.RDS")

library(ggcorrplot)

return_cor <- returns_wide %>% 
  select(-date) %>% 
  cor()

# return_cor

ggcorrplot(return_cor, type = "upper", lab = T)

# but correlations are not constant over time!

rolling_correlation <- returns_wide %>% 
  pivot_longer(cols = -c(date, SPY)) %>% 
  arrange(name, date) %>% 
  group_by(name) %>% 
  tq_mutate_xy(x = SPY, 
                  y = value,
                  mutate_fun = runCor,
                  n = 100,
                  col_rename = "rolling_correlation")

rolling_correlation %>% 
  filter(name %in% c("IJR", "GLD")) %>%
  # filter(name %in% c("RWR", "TLT")) %>%
  ggplot(aes(x = date, y = rolling_correlation, color = name)) +
  geom_line() +
  labs(x = "", y = "", color = "") +
  theme_classic()

rolling_correlation %>%
  # filter(name %in% c("EEM", "VTV", "IJR")) %>%
  # filter(name %in% c("TLT", "AGG")) %>%
  group_by(date) %>% 
  summarise(mean_rolling_correlation = mean(rolling_correlation),
            SPY = mean(SPY)) %>% 
  ggplot(aes(x = date, y = mean_rolling_correlation)) +
  geom_line() +
  geom_col(aes(y = SPY), color = "red") +
  labs(x = "", y = "", color = "") +
  theme_classic()

# Key Take-aways:
# 1. Diversification works because of low or even negative correlation
# 2. Correlation is not constant over time.
# 3. Correlation is likely to turn unfavorable in turbulent markets, making diversification less effective
