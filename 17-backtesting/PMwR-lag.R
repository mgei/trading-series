library(tidyverse)
library(tidyquant)
library(PMwR)
library(RcppRoll)

p <- tq_get(c("SPY", "GLD"), from = as.Date("2020-01-01"), to = as.Date("2020-01-10"))

p_m <- p %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  select(-date) %>% 
  as.matrix()

s_m <- matrix(data = 1,
              nrow = nrow(p_m),
              ncol = ncol(p_m))

s_m %>% clipr::write_clip()
s_m <- clipr::read_clip_tbl() %>% 
  as.matrix()

p_m %>% clipr::write_clip()
p_m <- clipr::read_clip_tbl() %>% 
  as.matrix()

signal_fun <- function(s) {
  w <- s[Time(),]
  if (sum(w) == 0) {
    w
  } else {
    w <- w/sum(w)
  }
  cat(Time(), w, "\n")
  w
}

nrow(p_m)
nrow(s_m)

rm(bt)
bt <- btest(prices = list(p_m), 
            signal = signal_fun,
            convert.weights = T,
            initial.cash = 100, 
            b = 1,
            # lag = 0,
            tol.p = 0.1,
            s = s_m)

journal(bt)

bt$position

p <- tq_get(c("SPY", "GLD"), from = as.Date("2006-01-01"))

p_m <- p %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  arrange(date) %>% 
  select(-date) %>% 
  as.matrix()

s_m <- p %>% 
  filter(symbol == "SPY") %>% 
  transmute(date, aboveMA = adjusted>lag(roll_mean(adjusted, n = 100, align = "right", fill = NA_real_))) %>% 
  mutate(SPY = ifelse(aboveMA, 1, 0) %>% replace_na(0),
         GLD = ifelse(aboveMA, 0, 1) %>% replace_na(0)) %>% 
  select(-aboveMA) %>% 
  arrange(date) %>% 
  select(-date) %>% 
  as.matrix()

bt_ma <- btest(prices = list(p_m), 
               signal = signal_fun,
               convert.weights = T,
               initial.cash = 100, 
               # b = 1,
               lag = 0,
               # tol.p = 0.8,
               s = s_m)

tibble(t = bt_ma$timestamp, w = bt_ma$wealth) %>% 
  ggplot(aes(x = t, y = w)) + 
  geom_line() +
  theme_minimal()

strategy <- as_tibble(p_m) %>% 
  bind_cols(as_tibble(s_m) %>% rename(posSPY = SPY, posGLD = GLD)) %>% 
  mutate(rSPY = SPY/lag(SPY)-1,
         rGLD = GLD/lag(GLD)-1) %>% 
  mutate(strategy = rSPY*lag(posSPY) + rGLD*lag(posGLD),
         wealth = cumprod(1+replace_na(strategy, 0))*100)


bind_rows(tibble(t = bt_ma$timestamp, w = bt_ma$wealth, source = "PWmR"),
          strategy %>% transmute(t = row_number(), w = wealth, source = "convent.")) %>% 
  ggplot(aes(x = t, y = w, color = source)) + 
  geom_line() +
  theme_minimal()
