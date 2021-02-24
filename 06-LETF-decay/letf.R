library(tidyverse)
library(tidyquant)
library(RcppRoll)
library(scales)

# Leveraged ETFs aim to replicate a multiple of the DAILY return of an underlying index
# The problems of LETFs are:
# 1. Compounding issue
# 2. Higher fees
# 3. Leverage premium

leveraged <- tq_get(c("SPY", "UPRO"))
leveraged %>% 
  group_by(symbol) %>% 
  mutate(p = adjusted/adjusted[1L]*1000) %>% 
  ggplot(aes(x = date, y = p, color = symbol)) +
  geom_line() +
  geom_label(aes(label = ifelse(date == max(date), number(p, accuracy = 1), NA))) +
  labs(x = "", y = "", color = "",
       title = paste0("Growth of initial investment of $1000 since ", min(leveraged$date))) +
  scale_y_log10() +
  theme_classic()

# 1. Compounding
# sample_returns <- tibble(r = c(0.05, 0.05, 0.05))
# sample_returns <- tibble(r = c(0.05, 0.05, -0.05))
# sample_returns <- tibble(r = c(0.05, -0.05, -0.05))
# sample_returns <- tibble(r = c(-0.05, -0.05, -0.05))

sample_returns %>% 
  mutate(r2 = r*2,
         r3 = r*3) %>% 
  summarise(performance = prod(1+r)-1,
            performance2 = prod(1+r2)-1,
            performance3 = prod(1+r3)-1) %>% 
  mutate(twice_performance = 2*performance,
         trice_performance = 3*performance) %>% 
  mutate(decay2 = performance2-twice_performance,
         decay3 = performance3-trice_performance)

# Findings:
# 1. seesawing zig-zag results in a decay
# 2. streaks of positive or negative returns are desirable

spx <- tq_get("^SP500TR", from = as.Date("1990-01-01"))

spx_r <- spx %>% 
  transmute(date, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r))

spx_leveraged <- spx_r %>% 
  mutate(r2 = r*2,
         r3 = r*3,
         p = cumprod(1+r),
         p2 = cumprod(1+r2),
         p3 = cumprod(1+r3))

spx_leveraged %>% 
  summarise(performance = prod(1+r)-1,
            performance2 = prod(1+r2)-1,
            performance3 = prod(1+r3)-1)

spx_leveraged %>% 
  select(date, starts_with("p")) %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "", y = "", color = "") +
  theme_classic()

n <- 252

decays <- spx_r %>% 
  mutate(r2 = r*2,
         r3 = r*3) %>% 
  mutate(performance_roll = roll_prod(1+r, n = n, align = "right", fill = NA)-1,
         performance2_roll = roll_prod(1+r2, n = n, align = "right", fill = NA)-1,
         performance3_roll = roll_prod(1+r3, n = n, align = "right", fill = NA)-1,
         decay2_roll = performance2_roll-(2*performance_roll),
         decay3_roll = performance3_roll-(3*performance_roll),
         vola_spx = roll_sd(r, n = n, align = "right", fill = NA)*sqrt(252))

mean(decays$decay2_roll, na.rm = T)

decays %>% 
  ggplot(aes(x = date, y = decay2_roll)) +
  geom_line() +
  geom_line(aes(y = vola_spx), color = "red") +
  geom_hline(yintercept = mean(decays$decay2_roll, na.rm = T), color = "green") +
  theme_classic()

decays %>% 
  ggplot(aes(x = date, y = decay3_roll)) +
  geom_line() +
  geom_line(aes(y = vola_spx), color = "red") +
  geom_hline(yintercept = mean(decays$decay3_roll, na.rm = T), color = "green") +
  theme_classic()


# does this work with LETFs?
letf_tickers <- c("SPY", "SSO", "UPRO")
letf <- tq_get(letf_tickers, from = as.Date("2006-07-01"))
letf_r <- letf %>% 
  group_by(symbol) %>% 
  transmute(date, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = symbol, values_from = r)

letf_r %>% 
  filter(!is.na(UPRO)) %>% 
  left_join(spx_r %>% rename(SPX = r), by = "date") %>% 
  mutate(SPX2 = 2*SPX,
         SPX3 = 3*SPX) %>% 
  summarise(performance_SPX = prod(1+SPX)-1,
            performance_SPX2 = prod(1+SPX2)-1,
            performance_SPX3 = prod(1+SPX3)-1,
            performance_SPY = prod(1+SPY)-1,
            performance_SSO = prod(1+SSO)-1,
            performance_URPO = prod(1+UPRO)-1)

# less favorable period
letf_r %>% 
  filter(between(date, as.Date("2018-01-01"), as.Date("2019-06-30"))) %>% 
  left_join(spx_r %>% rename(SPX = r), by = "date") %>% 
  mutate(SPX2 = 2*SPX,
         SPX3 = 3*SPX) %>% 
  summarise(performance_SPX = prod(1+SPX)-1,
            performance_SPX2 = prod(1+SPX2)-1,
            performance_SPX3 = prod(1+SPX3)-1,
            performance_SPY = prod(1+SPY)-1,
            performance_SSO = prod(1+SSO)-1,
            performance_URPO = prod(1+UPRO)-1)

# key take-aways:
# - compounding loss occurs in high vola markets with seesawing price action
# - LETFs are not only for the short-term trader (at least in markets with a positive expected return)
# - LETFs do a poor job still, not explained by the compounding loss (reason: fees/tracking error, leverage premium)


