add_first <- function(x, ...) {
  dots <- list(...)
  
  lengths <- unlist(lapply(dots, length))
  if (any(lengths != length(x))) {
    stop("vectors must be of same length: ", length(x))
  }
    
  for (i in 1:length(dots)) {
    temp <- dots[[i]]
    temp[which(!is.na(x))] <- NA
    x <- pmax(x, temp, na.rm = T)
  }
  return(x)
}

binary_inverse <- function(x) {
  if (min(x, na.rm = T) < 0 | max(x, na.rm = T) > 0) {
    stop("input must be between 0 and 1")
  }
  
  x <- abs(x-1)
  return(x)
}

signal_fun <- function(n, entry_ret_lag, stop_loss, stop_gain, stop_time) {
  if (Time() == n) {
    Globals$entry_prices <- rep(NA_real_, ncol(Close(0)))
    Globals$entry_times <- rep(NA_integer_, ncol(Close(0)))
  }
  
  cl <- Close(n = n)
  spy <- cl[,ncol(cl)]
  spy_ret <- spy[length(spy)]/spy[1L]-1
  
  etf_ret <- cl[nrow(cl),]/cl[1L,]-1
  
  w_current  <- pmin(SuggestedPortfolio(1), 1) # convert to binary 1/0 using pmin
  w_new      <- pmax(as.numeric(etf_ret-spy_ret<entry_ret_lag) - w_current, 0)
  
  w_removeSL <- as.numeric((Close()/Globals$entry_prices)<(1+stop_loss))
  w_removeSG <- as.numeric((Close()/Globals$entry_prices)>(1+stop_gain))
  w_removeTM <- as.numeric((Time()-Globals$entry_times)>stop_time)
  
  w_remove <- replace_na(w_removeSL, 0) + replace_na(w_removeSG, 0) + replace_na(w_removeTM, 0)
  
  w <- pmax(w_new, w_current, na.rm = T) - w_remove
  
  w_shares <- replace_na(w/ncol(Close())/Close(), 0)*Wealth()

  Globals$entry_prices <- add_first(Globals$entry_prices, w_new*Close())
  Globals$entry_times <- add_first(Globals$entry_times, w_new*Time())
  Globals$entry_prices[which(w == 0)] <- NA_real_
  Globals$entry_times[which(w == 0)] <- NA_integer_
  
  # if (Time() == 5524) {
  #   cat("w_current: ", w_current, "\n")
  #   cat("w_new: ", w_new, "\n")
  #   cat("w_remove: ", w_remove, "\n")
  #   cat("w: ", w, "\n")
  # }
  w_shares
}

n <- 100
entry_ret_lag <- -0.25
stop_loss     <- -0.2
stop_gain     <- 0.4
stop_time     <- 250

tests <- tibble()
for (n in c(150, 200, 250, 300)) {
  for (entry_ret_lag in c(-0.2, -0.25, -0.3)) {
    for (stop_loss in c(-0.15, -0.25)) {
      for (stop_gain in c(0.25, 0.4, 0.6, 0.75)) {
        for (stop_time in c(Inf)) {
          bt0 <- btest(prices = list(cbind(prices_m, spy_m)), 
                       signal = signal_fun, 
                       initial.cash = 10000, 
                       convert.weights = F, 
                       b = n,
                       timestamp = timestamps, 
                       lag = 1, 
                       n = n,
                       entry_ret_lag = entry_ret_lag,
                       stop_loss = stop_loss,
                       stop_gain = stop_gain,
                       stop_time = stop_time)
          
          tests <- bind_rows(tests,
                             tibble(n = n,
                                    entry_ret_lag = entry_ret_lag,
                                    stop_loss = stop_loss,
                                    stop_gain = stop_gain,
                                    stop_time = stop_time,
                                    ret = last(prod(1+returns(bt0$wealth), na.rm = T)-1),
                                    sd = sd(returns(bt0$wealth), na.rm = T),
                                    low = min(bt0$wealth, na.rm = T)/bt0$initial.wealth-1))
        }
      }
    }
  }
  print(n)
}

p <- tests %>% 
  ggplot(aes(x = ret, y = sd, color = n, alpha = n)) +
  geom_point()

plotly::ggplotly(p)

tests %>% 
  arrange(-ret)

n             <- 250
entry_ret_lag <- -0.2
stop_loss     <- -0.1
stop_gain     <- 0.75
stop_time     <- Inf

bt0 <- btest(prices = list(cbind(prices_m, spy_m)), 
             signal = signal_fun, 
             initial.cash = 10000, 
             convert.weights = F, 
             b = n,
             timestamp = timestamps, 
             lag = 1, 
             n = n, 
             # tol = 3,
             fraction = 0.9,
             entry_ret_lag = entry_ret_lag,
             stop_loss = stop_loss,
             stop_gain = stop_gain,
             stop_time = stop_time)

tibble(date = timestamps,
       wealth = bt0$wealth, 
       positions = bt0[["position"]]) %>% 
  # filter(date %>% between(as.Date("2008-09-20"), as.Date("2008-12-31"))) %>%
  # mutate(r = wealth/lag(wealth)-1,
  #        r_perc = percent(r)) %>%
  # arrange(r)
  ggplot(aes(x = date, y = wealth)) +
  geom_line()

pl(bt0)
