library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)

# Relative Strength: Crude Oil vs Gold

# Crude Oil
# https://www.quandl.com/data/CHRIS/CME_CL18-Crude-Oil-Futures-Continuous-Contract-18-CL18
crude <- tq_get("CHRIS/CME_CL18", get = "quandl", from = as.Date("1900-01-01")) %>% 
  mutate(settle = na.locf0(settle)) %>% 
  transmute(date, settle,
            r = settle/lag(settle)-1) %>% 
  filter(date >= as.Date("1960-12-01"))

# Gold
# https://www.quandl.com/data/LBMA/GOLD-Gold-Price-London-Fixing
gold <- tq_get("LBMA/GOLD", get = "quandl", from = as.Date("1900-01-01")) %>% 
  mutate(settle = na.locf0(`usd.pm.`)) %>% 
  transmute(date, `usd.pm.`,
            r = `usd.pm.`/lag(`usd.pm.`)-1) %>%
  filter(date >= as.Date("1960-12-01"))

# Market and Risk-Free
# Fama/French 3 Factors [Daily] --> get CSV
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
ff_data <- read_csv("../04-SMA-RiskOnOff/data/F-F_Research_Data_Factors_daily.CSV",
                    skip = 3) %>% 
  transmute(date = ymd(X1), mkt = (`Mkt-RF`+RF)/100, rf = RF/100) %>% 
  filter(!is.na(mkt),
         date >= as.Date("1960-12-01"))

signal <- left_join(crude %>% 
            group_by(month = floor_date(date, "months")) %>% 
            summarise(r.crude = prod(1+r, na.rm = T)-1),
          gold %>% 
            group_by(month = floor_date(date, "months")) %>% 
            summarise(r.gold = prod(1+r, na.rm = T)-1),
          by = "month") %>% 
  # crude outperforming Gold is a bullish signal
  mutate(crude_outperforming = r.crude > r.gold,
         crude_outperforming_lag = lag(crude_outperforming))

strategy <- ff_data %>% 
  full_join(signal %>% select(month, crude_outperforming_lag),
            by = c("date" = "month")) %>% 
  arrange(date) %>% 
  mutate(crude_outperforming_lag_locf = na.locf0(crude_outperforming_lag)) %>% 
  filter(!is.na(crude_outperforming_lag_locf),
         !is.na(mkt)) %>% 
  # trading strategy
  mutate(strategy = ifelse(crude_outperforming_lag_locf, mkt, rf))

strategy %>% 
  select(date, mkt, strategy) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000) %>%
  ggplot(aes(x = date, y = p, color = name)) +
  geom_line() +
  scale_y_log10(labels = number_format(accuracy = 1), breaks = c(1000, 3000, 10000, 100000, 30000, 60000, 10000000)) +
  labs(x = "", y = "", color = "", title = paste0("Growth of $1000 invested in ", format(min(strategy$date), "%B %Y"))) +
  theme_classic()


# stats
strategy %>% 
  select(date, mkt, strategy) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  summarise(annual_return = (1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252), 
            from = min(date), to = max(date))


# drawdown
calc_drawdown <- function(Ra) {
  # same calculation formula as PerformanceAnalytics::Drawdowns geometric = FALSE
  
  p <- 1 + cumprod(1 + Ra) - 1
  m <- cummax(c(1, p))[-1]
  d <- p/m - 1
  
  return(d)
}

strategy %>% 
  select(date, mkt, strategy) %>% 
  pivot_longer(cols = -date) %>% 
  group_by(name) %>% 
  mutate(drawdown = calc_drawdown(value)) %>% 
  ggplot(aes(x = date, y = drawdown, color = name)) +
  geom_line() +
  scale_y_continuous(labels = percent) +
  labs(x = "", color = "",
       title = "Drawdown Market vs Rel. Strength Crude-Gold Rotation Strategy") +
  theme_classic()

# Take-aways:
# 1. Rising crude oil prices is a bullish signal, rising gold prices is a bearish signal.
# 2. This is just an example, there are many other signals: 
#   bullish: copper, lumber, many other commodities, cyclical stocks, emerging markets, long term yields etc.
#   bearish: gold, short term treasuries, defensive stocks, consumer staples, healthcare etc.
# 3. Many ways to play it out: rotation assets, timeframe etc. 


  