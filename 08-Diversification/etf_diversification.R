# How diversification works? 
# Why is the portfolio return is the average of the components' returns,
# but the risk is always below the average risk of the components.

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

# let's get a few ETFs that could form a diversified portfolio
# SPY: S&P 500
# GLD: Gold
# AGG: Bond
# EEM: Emerging Markets
# RWR: REIT
# TLT: 20+ Year US Treasuries
# DBC: Commodities
# VTV: Value Stocks
# VTQ: Real Estate
# IJR: Small Cap
tickers <- c("SPY", "GLD", "AGG", "EEM", "RWR", "TLT", "DBC", "VTV", "VNQ", "IJR")
pricedata <- tq_get(tickers, from = as.Date("1900-01-01"))

# we have data since 2006-02-06
pricedata %>% 
  group_by(symbol) %>% 
  summarise(min = min(date)) %>% 
  arrange(min)

returns <- pricedata %>% 
  filter(date >= as.Date("2006-02-06")) %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r))

returns_wide <- returns %>% 
  pivot_wider(names_from = symbol, values_from  = r)

# returns_wide %>%
#   saveRDS("data/returns_wide.RDS")

portfolios <- returns_wide %>% 
  select(date, SPY, GLD, AGG, EEM, RWR, TLT, DBC, VTV, VNQ, IJR) %>% 
  mutate(portfolio1 = rowMeans(.[2:3]),
         portfolio2 = rowMeans(.[2:4]),
         portfolio3 = rowMeans(.[2:5]),
         portfolio4 = rowMeans(.[2:6]),
         portfolio5 = rowMeans(.[2:7]),
         portfolio6 = rowMeans(.[2:8]),
         portfolio7 = rowMeans(.[2:9]),
         portfolio8 = rowMeans(.[2:10]),
         portfolio9 = rowMeans(.[2:11]))

portfolios_long <- portfolios %>% 
  pivot_longer(cols = -date, values_to = "r")

# portfolio 1
spy_gld_portfolio <- portfolios_long %>% 
  filter(name %in% c("SPY", "GLD", "portfolio1"))

spy_gld_portfolio %>% 
  group_by(name) %>% 
  summarise(annual_return = (1+mean(r))^252 - 1,
            annual_volatility = sd(r)*sqrt(252),
            return_per_vola = annual_return/annual_volatility) %>% 
  mutate_at(vars(starts_with("annual")), percent)

spy_gld_portfolio %>% 
  stocks_chart()

# why? the correlation plays a role 
# https://www.google.com/search?q=portfolio+variance&source=lnms&tbm=isch

# portfolio 2
spy_gld_agg_portfolio <- portfolios_long %>% 
  filter(name %in% c("SPY", "GLD", "AGG", "portfolio2"))

spy_gld_agg_portfolio %>% 
  group_by(name) %>% 
  summarise(annual_return = (1+mean(r))^252 - 1,
            annual_volatility = sd(r)*sqrt(252),
            return_per_vola = annual_return/annual_volatility) %>% 
  mutate_at(vars(starts_with("annual")), percent)

spy_gld_agg_portfolio %>% 
  stocks_chart()


# portfolio 3
spy_gld_agg_eem_portfolio <- portfolios_long %>% 
  filter(name %in% c("SPY", "GLD", "AGG", "EEM", "portfolio3"))

spy_gld_agg_eem_portfolio %>% 
  group_by(name) %>% 
  summarise(annual_return = (1+mean(r))^252 - 1,
            annual_volatility = sd(r)*sqrt(252),
            return_per_vola = annual_return/annual_volatility) %>% 
  mutate_at(vars(starts_with("annual")), percent)

spy_gld_agg_eem_portfolio %>% 
  stocks_chart()

portfolios_long %>% 
  filter(str_detect(name, "SPY|portfolio")) %>% 
  group_by(name) %>% 
  summarise(annual_return = (1+mean(r))^252 - 1,
            annual_volatility = sd(r)*sqrt(252),
            return_per_vola = annual_return/annual_volatility) %>% 
  mutate(portfolio = str_remove(name, "portfolio") %>% replace_na("SPY")) %>% 
  ggplot(aes(x = annual_volatility, y = annual_return)) +
  geom_label(aes(label = portfolio)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  theme_classic()

# optimize? Markowitz

