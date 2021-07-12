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

# How to select an ETF? How to know if a certain ETF fits in your portfolio?

# 1. Risk (volatility and beta)
# 2. Exposure
# 3. Tracking Error

# Exposure: Does the ETF really cover a different/uncorrelated return stream than what you already have.

qqq <- tq_get("QQQ")
spy <- tq_get("SPY")


# BOTZ	Global X Robotics & Artificial Intelligence Thematic ETF
botz <- tq_get("BOTZ")

bind_rows(qqq, spy, botz) %>% 
  group_by(symbol) %>% 
  mutate(r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  summarise(annual_return = (1+mean(r))^252-1,
            annual_vola = sd(r)*sqrt(252),
            return_per_risk = annual_return/annual_vola)

bind_rows(qqq, spy, botz) %>% 
  group_by(symbol) %>% 
  mutate(r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  ggplot(aes(x = r, fill = symbol)) +
  geom_histogram(bins = 40) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~symbol, ncol = 1) +
  theme_linedraw() +
  theme(legend.position = "none")

# 2. Exposure
qqq_spy_botz <- bind_rows(qqq, spy, botz) %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  pivot_wider(names_from = symbol, values_from = r)


model1 <- lm(data = qqq_spy_botz,
            formula = BOTZ ~ SPY)
model2 <- lm(data = qqq_spy_botz,
            formula = BOTZ ~ QQQ)
model3 <- lm(data = qqq_spy_botz,
            formula = BOTZ ~ SPY + QQQ)

stargazer(model1, model2, model3, type = "text")

# TAN	Invesco Solar ETF
# 1. Risk
tan <- tq_get("TAN")

bind_rows(qqq, spy, tan) %>% 
  group_by(symbol) %>% 
  mutate(r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  summarise(annual_return = (1+mean(r))^252-1,
            annual_vola = sd(r)*sqrt(252),
            return_per_risk = annual_return/annual_vola)

bind_rows(qqq, spy, tan) %>% 
  group_by(symbol) %>% 
  mutate(r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  ggplot(aes(x = r, fill = symbol)) +
  geom_histogram(bins = 40) +
  scale_x_continuous(labels = percent) +
  facet_wrap(~symbol, ncol = 1) +
  theme_linedraw() +
  theme(legend.position = "none")

# 2. Exposure
qqq_spy_tan <- bind_rows(qqq, spy, tan) %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  group_by(symbol, date = floor_date(date, "weeks")) %>% 
  summarise(r = prod(1+r)-1) %>% 
  pivot_wider(names_from = symbol, values_from = r)

model1 <- lm(data = qqq_spy_tan,
             formula = TAN ~ SPY)
model2 <- lm(data = qqq_spy_tan,
             formula = TAN ~ QQQ)
model3 <- lm(data = qqq_spy_tan,
             formula = TAN ~ SPY + QQQ)

library(stargazer)
stargazer(model1, model2, model3, type = "text")

# Colombia ETFs

