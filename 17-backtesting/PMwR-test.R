library(tidyverse)
library(tidyquant)
library(RcppRoll)

as_mat <- function(x) {
  x %>% 
    as.data.frame() %>% 
    column_to_rownames("date") %>% 
    as.matrix()
}

prices <- tq_get(c("SPY", "TLT"), from = as.Date("2005-01-01"))

rets <- prices %>% 
  group_by(symbol) %>% 
  mutate(r = adjusted/lag(adjusted)-1)

rets_m <- rets %>% 
  select(symbol, date, r) %>% 
  pivot_wider(names_from = symbol, values_from = r) %>% 
  drop_na() %>% 
  arrange(date)

prices_m <- prices %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  drop_na() %>% 
  arrange(date)

ma_signal_m <- prices %>% 
  group_by(symbol) %>% 
  mutate(ma100 = lag(roll_mean(adjusted, n = 100, fill = NA_real_, partial = F, align = "right")),
         above_ma = adjusted>ma100) %>% 
  select(symbol, date, above_ma) %>% 
  pivot_wider(names_from = symbol, values_from = above_ma) %>% 
  arrange(date)

strategy <- rets_m %>% 
  left_join(ma_signal_m %>% 
              select(date, SPY_signal = SPY),
            by = "date") %>% 
  mutate(SPY_signal = lag(SPY_signal),
         strategy = case_when(is.na(SPY_signal) ~ 0,
                              SPY_signal ~ SPY, 
                              !SPY_signal ~ TLT,
                              T ~ 0))

strategy %>% 
  filter(date >= as.Date("2005-10-01"))

library(PMwR)

signal_fun <- function(ma_signal_m, 
                       rdates) {
  d <- as.character(Timestamp())
  if (is.na(ma_signal_m[d,])) {
    w <- c(0, 0)
  } else if (ma_signal_m[d,]) {
    w <- c(1, 0)
  } else {
    w <- c(0, 1)
  }
  w
}

timestamps <- prices_m$date

bt <- btest(prices = list(prices_m %>% as_mat),
            signal = signal_fun,
            # do.rebalance = T,
            # do.signal = T,
            b = 1,
            timestamp = timestamps,
            initial.cash = 1000,
            convert.weights	= T,
            ma_signal_m = ma_signal_m %>% select(date, SPY) %>% as_mat(),
            rdates = timestamps)

bt$journal %>% 
  as_tibble()

ma_signal %>% 
  filter(date >= as.Date("2005-10-01"))


tibble(date = timestamps, cash = bt$cash) %>%  
  ggplot(aes(x = date, y = cash)) +
  geom_line()

bind_rows(tibble(date = timestamps, strategy = bt$wealth, calc = "PMwR"),
          strategy %>% 
            mutate(strategy = cumprod(1+strategy)*1000,
                   calc = "classic")) %>% 
  ggplot(aes(x = date, y = strategy, color = calc)) +
  geom_line()



strategy %>% 
  mutate(x = cumprod(1+strategy)*1000) %>% 
  filter(strategy != 0)

new_r <- returns(prices_m %>% as_mat(),
        # rebalance.when = timestamps,
        weights = ma_signal_m %>% 
          mutate_if(is.numeric, replace_na, 0) %>% 
          mutate(TLT = ifelse(SPY, 1, 0),
                 SPY = as.numeric(SPY)) %>% 
          as_mat())

new_r$
  as.data.frame() %>% 
  rownames_to_column("date") %>% 
  as_tibble()


set.seed(1)
p_m <- tibble(date = 1:10,
              A = cumprod(1+rnorm(10, sd = 0.1/sqrt(252)))*10,
              B = cumprod(1+rnorm(10, mean = 0.01, sd = 1/sqrt(252)))*10) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  as_mat()

s_m <- p_m
s_m[,"A"] <- c(1,1,1,1,1,0,0,0,0,0)
s_m[,"B"] <- c(0,0,0,0,0,1,1,1,1,1)

p_m <- structure(c(9.96, 9.97, 9.92, 10.02, 10.04, 9.99, 10.02, 10.07, 
                   10.1, 10.08, 11.05, 11.43, 11.1, 9.66, 10.44, 10.52, 10.61, 11.35, 
                   12.05, 12.62), .Dim = c(10L, 2L), 
                 .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                                  c("A", "B")))

s_m <- structure(c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 
                   1, 1, 1), .Dim = c(10L, 2L), 
                 .Dimnames = list(c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), 
                                  c("A", "B")))

signal_fun2 <- function(signal_w) {
  # (Cash(0)/Close(0))
  # r <- Cash()/Close()
  # print(r*signal_w[Timestamp(0),])
  # 
  # signal_w[Timestamp(0),]
}

bt2 <- btest(list(p_m), 
             signal_fun2, 
             convert.weights = T,
             initial.cash = 1000,
             # tol.p = 0.1,
             # lag = 1,
             signal_w = s_m)

library(tidyverse)
bt2$journal %>% 
  as_tibble() %>% 
  left_join(tibble(timestamp = 1:10, cash = bt2$cash), by = "timestamp")


bt2$position

bt2$cash %>% scales::number()



p1 <- rep(100, 5)
p2 <- p1 + seq(from = 0, to = 20, length.out = length(p1))
P <- cbind(p1, p2)
P

bt <- btest(list(P),
            signal = function() c(0.5, 0.5),
            convert.weights = TRUE,
            initial.cash = 100)

journal(bt)

spy_streaks <- streaks(spy$adjusted)

spy_streaks_tbl <- spy_streaks %>% 
  as_tibble() %>% 
  filter(!is.na(state)) %>% 
  left_join(spy %>% 
              transmute(date_start = date, start = row_number()),
            by = "start") %>% 
  left_join(spy %>% 
              transmute(date_end = date, end = row_number()),
            by = "end")


spy %>% 
  ggplot() +
  geom_line(aes(x = date, y = adjusted)) +
  # scale_y_log10() +
  geom_rect(data = spy_streaks_tbl, 
            aes(xmin = date_start, xmax = date_end,
                ymin = -Inf, ymax = Inf, fill = factor(state)),
            alpha = 0.2)






stocks <- data.frame(A = c(100, 120, 130, 130, 140, 140, 150, 155),
                     B = c(100, 105, 100, 95, 90, 90, 85, 80))

sig <- as.matrix(data.frame(buy = c("A", "A", "A", "A", "B", "B", "B", "A")))

signal_fun3 <- function(sig) {
  if (sig[Time(0)] == "A") {
    c(1, 0)
  } else {
    c(0,1)
  }
}

bt <- btest(prices = list(as.matrix(stocks)),
            signal = signal_fun3, 
            convert.weights = T, 
            lag = 0,
            # do.rebalance = function() Time(0) == 2,
            # b = 0,
            sig = sig,
            timestamp = 1:8,
            initial.cash = 100)

bt$journal

pl(bt)
