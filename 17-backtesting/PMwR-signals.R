library(tidyverse)
library(tidyquant)
library(PMwR)

df <- tq_get(c("GLD", "SPY", "TLT"))

df_w <- df %>% 
  select(date, symbol, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  as.data.frame() %>% 
  column_to_rownames("date") %>% 
  as.matrix()

timestamps <- df$date[1:nrow(df_w)]

signal_fun <- function() {
  if (Cash() > min(Close())) {
    w <- Cash()/length(Close())/Close()
    Globals$entries <- Close(0)
  } else {
    if (max(Portfolio()*Close()) > (0.5*Wealth())) {
      # rebalance
      w <- Wealth()/length(Close())/Close()
    } else {
      w <- Portfolio()
    }
  }
  
  if (Time() == 100) {
    cat(Time(), "\n", Timestamp(n = 2), "\n")
    
    # p <- Close(n=10)
    # pastret <- p[nrow(p),]/p[1L,]-1
    # cat(pastret, "\n")
  }
  
  w
}

bt <- btest(prices = list(df_w),
            signal = signal_fun, 
            initial.cash = 1000, 
            timestamp = timestamps,
            convert.weights = F)

position(bt) %>% 
  as.tibble() %>% 
  tail()

head(df_w, 10)
