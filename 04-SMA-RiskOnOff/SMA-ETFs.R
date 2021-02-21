library(tidyverse)
library(tidyquant)
library(RcppRoll)
library(lubridate)
library(scales)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

ETF <- tq_get(c("SPY", "SHY"), from = as.Date("2000-01-01"))
ETF_returns <- ETF %>% 
  select(symbol, date, adjusted) %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  pivot_wider(names_from = symbol, values_from = r)

ETF %>% 
  group_by(symbol) %>% 
  summarise(inception = min(date))

ETF_SMA <- ETF %>% 
  filter(symbol == "SPY") %>% 
  select(symbol, date, close) %>% 
  mutate(SMA200 = roll_mean(close, n = 200, fill = NA_real_, align = "right"),
         above_SMA = close>SMA200,
         crossing = above_SMA != lag(above_SMA)) %>% 
  filter(date >= as.Date("2004-01-01"))

# be in the market when market traded above its 200-d moving average, else be in 1-m treasuries (rf)
strategy <- ETF_SMA %>%
  left_join(ETF_returns, by = "date") %>% 
  mutate(strategy = if_else(lag(above_SMA), SPY, SHY, missing = SPY))

strategy_performance <- strategy %>% 
  select(date, SPY, SHY, strategy) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000)

strategy_performance %>% 
  ggplot(aes(x = date, y = p, color = name)) +
  geom_line() +
  scale_y_log10(labels = number_format(accuracy = 1), breaks = c(1000, 2000, 3000, 4000, 5000)) +
  labs(x = "", y = "", color = "", title = paste0("Growth of $1000 invested in ", format(min(strategy_performance$date), "%B %Y"))) +
  theme_classic()

# Q: Wait, the strategy did not even outperform?
# A: No but you have to look at risk-adjusted performance.
strategy_performance %>% 
  summarise(annual_return = (1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252), 
            min = min(p), max = max(p),
            from = min(date), to = max(date))

calc_drawdown <- function(Ra) {
  # same calculation formula as PerformanceAnalytics::Drawdowns geometric = FALSE
  p <- 1 + cumprod(1 + Ra) - 1
  m <- cummax(c(1, p))[-1]
  d <- p/m - 1
  
  return(d)
}

strategy_performance %>% 
  filter(name != "SHY") %>% 
  mutate(drawdown = calc_drawdown(value)) %>% 
  ggplot(aes(x = date, y = drawdown, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent, n.breaks = 10) +
  labs(x = "", color = "",
       title = "Drawdown Market vs 200-d SMA Rotation Strategy") +
  theme_classic()

# Last thought: apply leverage to amplify returns when above SMA.
