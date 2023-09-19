library(tidyverse)
library(QuantTools)
library(tidyquant)
library(RcppRoll)

stock <- c(1, 0.9, 0.8, 0.9, 1)

back_test(enter = c(T,F,F,F,F),
          exit = c(F,F,F,F,F,T), 
          price = c(stock, stock), 
          stop_loss = -0.03)

back_testy <- function(df, entry_col = "entry", exit_col = "exit", price_col = "adjusted", date_col = "date") {
  dates <- tibble(date = df[[date_col]],
                  id = 1:length(date))
  
  back_test(enter = df[[entry_col]],
            exit = df[[exit_col]],
            price = df[[price_col]]) %>% 
    as_tibble() %>% 
    left_join(dates %>% 
                rename(date_enter = date),
              by = c("id_enter" = "id")) %>% 
    left_join(dates %>% 
                rename(date_exit = date),
              by = c("id_exit" = "id")) %>% 
    mutate(duration = id_exit - id_enter)
}

spy %>% 
  mutate(ma100 = roll_mean(adjusted, n = 100, fill = NA_real_, partial = F, align = "right")) %>% 
  drop_na() %>% 
  mutate(invested = ma100>adjusted,
         entry = invested > lag(invested),
         exit = invested < lag(invested)) %>% 
  drop_na() %>% 
  back_testy()
  summarise(strategy = prod(1+pnl_trade)-1)
  
spy %>% 
  mutate(ma100 = roll_mean(adjusted, n = 100, fill = NA_real_, partial = F, align = "right")) %>% 
  drop_na() %>% 
  mutate(invested = ma100>adjusted,
         entry = invested > lag(invested),
         exit = invested < lag(invested)) %>% 
  drop_na() %>% 
  mutate(strategy = replace_na((adjusted/lag(adjusted)-1)*lag(invested), 0)) %>% 
  group_by(grp = rleid(invested))
  
  
  
  summarise(strategy = prod(1+strategy)-1)


devtools::install_github("Robot-Wealth/rsims")

library(rsims)
library(tidyverse)

prices <- tq_get(c("SPY", "GLD", "IWM"))
prices_m <- prices %>% 
  select(symbol, date, adjusted) %>% 
  pivot_wider(names_from = symbol, values_from = adjusted) %>% 
  mutate(date = as.numeric(date)) %>% 
  as.matrix()

w_m <- prices_m
w_m[,2:4] <- 1/3

out <- cash_backtest(prices = prices_m, theo_weights = w_m, initial_cash = 1000, trade_buffer = 0.1, capitalise_profits = T)

out %>% 
  select(ticker, Date, Value) %>% 
  group_by(Date) %>% 
  mutate(Value_perc = Value/sum(Value)) %>% 
  ggplot(aes(x = Date, y = Value_perc, color = ticker)) + 
  geom_line()
  
  pivot_wider(names_from = ticker, values_from = Value)

tidyquant

install.packages("SIT")


adjusted <- spy %>% 
  tail(100) %>% 
  pull(adjusted)

library(curl)
curl_download('https://github.com/systematicinvestor/SIT/raw/master/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')

library(SIT)
# Run plota test
plota.test()



# Trades: 1467
Net profit: 406.89%
CAR: 32.97% (compound annual return)
RAR: 33.67% (realized annual return)
MDD: -43.52% (maximum draw down)
CAR/MDD: 0.76
Win Rate: 53.78%
Avg. P/L Per Trade = 12.72%
Avg Duration: 10.7 days.
Profit Factor: 1.68 (gross profit / gross loss)
Risk Adjusted Return: ...%

back_test(enter = c(T, rep(F, 40), T, rep(F, 48)),
          exit = c(F, T, rep(F, 97), T),
          price = adjusted) %>% 
  plot_dts()

path <- "~/Documents/Econovo/decoding-markets/03-institutional/data/csv_sf3_active_locf/"
files <- list.files(path)
for (f in files) {
  
}

library(tidyverse)

library(QuantTools)
# load ticks data set
data(ticks)
ticks %>% class()

ticks %>% 
  as_tibble()

library(strand)
data(sample_secref)
data(sample_pricing)
data(sample_inputs)

sample_secref %>% as_tibble()
sample_pricing %>% as_tibble()
sample_inputs %>% as_tibble()
