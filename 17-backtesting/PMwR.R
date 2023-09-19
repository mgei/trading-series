library(PMwR)

library(tidyquant)
library(RcppRoll)
prices <- tq_get(c("SPY", "GLD", "IWM"))
prices_m <- prices %>% 
  select(date, symbol, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  select(-date) %>% 
  as.matrix()

signal_m <- prices %>% 
  select(date, symbol, adjusted) %>% 
  group_by(symbol) %>% 
  mutate(ma = roll_mean(adjusted, n = 100, fill = NA_real_, align = "right")) %>% 
  mutate(signal = replace_na(ifelse(ma>adjusted, 1, 0), 0)) %>% 
  ungroup() %>% 
  select(-c(ma, adjusted)) %>% 
  pivot_wider(names_from = symbol, values_from = signal) %>% 
  select(-date) %>% 
  as.matrix()

signal <- function() {
  if (any(Close() > 135))
    c(1,1,1)
  else
    c(0,0,0)
}

dim(prices_m)
dim(signal_m)

bt <- btest(prices = list(p = prices_m), signal = signal, initial.cash = 10000, b = 3)

trade_details <- function(solution, prices) {
  tibble(price    = prices,
         suggest  = solution$suggested.position,
         position = solution$position, ## do not unname
         wealth   = solution$wealth,
         cash     = solution$cash)
}

journal(bt)

trade_details(bt, prices_m)

trade_detail(bt, prices_m)


# https://stackoverflow.com/questions/61227215/create-stock-portfolio-based-on-probabilities-with-rebalancing-in-r/61230412#61230412
library(NMOF)
library(PMwR)

P <- French(dest.dir = "data/",
            "12_Industry_Portfolios_daily_CSV.zip",
            price.series = TRUE, na.rm = TRUE)
timestamp <- as.Date(row.names(P))

# reb.dates <- timestamp[seq(from = 5, to = length(timestamp), by = 10)]
reb.dates <- timestamp[seq(from = 5, to = length(timestamp), by = 252)]
# reb.dates <- seq(from = min(timestamp), to = max(timestamp), by = "years")
best.stocks <- t(replicate(length(reb.dates),
                           sample(c(rep(TRUE, 5), rep(FALSE, 7)))))
colnames(best.stocks) <- colnames(P)
head(data.frame(reb.dates, best.stocks))

signal <- function(best.stocks, reb.dates) {
  w <- numeric(ncol(Close()))    
  w[best.stocks[Timestamp(0) == reb.dates]]  <- 1
  w <- w/sum(w)
  w
}

bt <- btest(prices = list(as.matrix(P)),
            timestamp = timestamp,
            signal = signal,
            do.signal = reb.dates,
            initial.cash = 100,
            convert.weights = TRUE,
            best.stocks = best.stocks,
            reb.dates = reb.dates)

## see a performance summary 
summary(as.NAVseries(bt))

bt$position %>% head()


positions <- as_tibble(bt$position) %>% 
  mutate(timestamp = timestamp) %>% 
  select(timestamp, everything())

head(positions)
length(timestamp)

## see the trades
journal(bt)
