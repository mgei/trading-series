library(tidyverse)
library(tidyquant)
library(RcppRoll)
library(lubridate)
library(scales)
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# SIMPLE MOVING AVERAGE (SMA)
# Rotation Strategy

# Fama/French 3 Factors [Daily] --> get CSV
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
ff_data <- read_csv("data/F-F_Research_Data_Factors_daily.CSV",
                    skip = 3) %>% 
  transmute(date = ymd(X1), mkt = (`Mkt-RF`+RF)/100, rf = RF/100) %>% 
  filter(!is.na(mkt))

performance <- ff_data %>% 
  filter(date >= as.Date("1928-01-01")) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000)

performance %>% 
  ggplot(aes(x = date, y = p, color = name)) +
  geom_line() +
  scale_y_log10(labels = number_format(accuracy = 1), breaks = c(1000, 10000, 100000, 1000000, 10000000)) +
  labs(x = "", y = "", color = "", title = paste0("Growth of $1000 invested in ", format(min(performance$date), "%B %Y"))) +
  theme_classic()

# stats
performance %>% 
  summarise(annual_return = (1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252), 
            min = min(p), max = max(p),
            from = min(date), to = max(date))

# Can we come up with a trading rule that gives a better performance?
ff_SMA <- ff_data %>% 
  mutate(p_mkt = cumprod(1+mkt)*1000,
         SMA200 = roll_mean(p_mkt, n = 200, fill = NA_real_, align = "right"),
         above_SMA = p_mkt>SMA200,
         crossing = above_SMA != lag(above_SMA)) %>% 
  filter(date >= as.Date("1928-01-01"))

ff_SMA %>% 
  # filter(between(date, as.Date("1990-01-01"), as.Date("2010-12-31"))) %>%
  ggplot() +
  geom_line(aes(x = date, y = p_mkt), color = "red") +
  geom_line(aes(x = date, y = SMA200), color = "black") +
  geom_line(aes(x = date, y = p_mkt, color = above_SMA, group = 1)) +
  scale_y_log10(labels = number_format(accuracy = 1), breaks = c(1000, 10000, 100000, 1000000, 10000000)) +
  labs(x = "", y = "") +
  theme_classic()

# be in the market when market traded above its 200-d moving average, else be in 1-m treasuries (rf)
strategy <- ff_SMA %>% 
  mutate(strategy = if_else(lag(above_SMA), mkt, rf, missing = mkt))

strategy_performance <- strategy %>% 
  select(date, mkt, strategy) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000)

strategy_performance %>% 
  ggplot(aes(x = date, y = p, color = name)) +
  geom_line() +
  scale_y_log10(labels = number_format(accuracy = 1), breaks = c(1000, 10000, 100000, 1000000, 10000000)) +
  labs(x = "", y = "", color = "", title = paste0("Growth of $1000 invested in ", format(min(performance$date), "%B %Y"))) +
  theme_classic()

# stats
strategy_performance %>% 
  summarise(annual_return = (1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252), 
            min = min(p), max = max(p),
            from = min(date), to = max(date))

# how many trades on average per year?
strategy %>% 
  group_by(year = year(date)) %>% 
  summarise(trades = sum(crossing)) %>% print(n = 100)
  summarise(avg_trades = mean(trades),
            max_trades = max(trades),
            min_traded = min(trades))


calc_drawdown <- function(Ra) {
  # same calculation formula as PerformanceAnalytics::Drawdowns geometric = FALSE
  
  p <- 1 + cumprod(1 + Ra) - 1
  m <- cummax(c(1, p))[-1]
  d <- p/m - 1
  
  return(d)
}

strategy_performance %>% 
  mutate(drawdown = calc_drawdown(value)) %>% 
  ggplot(aes(x = date, y = drawdown, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  labs(x = "", color = "",
       title = "Drawdown Market vs 200-d SMA Rotation Strategy") +
  theme_classic()


# This is so simple, why does this strategy still works?
# 1. Investors go with the flow, momentum is a thing in all markets. Money attracts money.
# 2. The strategy can be counter-intuitive: you get FOMO when going out of the market and don't want to return during armageddon.
# 3. Objections: It just a coincidence, the US market was exceptional over the last 100 years. Unlikely to work going forward.
