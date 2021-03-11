# How much Alpha has Cathie Wood generated in her ARK funds?

# ARKK	ARK Innovation ETF	Equity	               $20,167.10
# ARKG	ARK Genomic Revolution ETF	Equity         	$8,620.88
# ARKW	ARK Next Generation Internet ETF	Equity   	$6,498.44
# ARKF	ARK Fintech Innovation ETF	Equity	        $3,660.54
# ARKQ	ARK Industrial Innovation ETF	Equity      	$3,025.98
# PRNT	3D Printing ETF	Equity	                      $499.39 - passive
# IZRL	ARK Israel Innovative Technology ETF	Equity	$337.52 - passive
# # https://etfdb.com/etfs/issuers/ark-investment-management/

library(tidyverse)
library(tidyquant)
library(lubridate)
library(scales)

berkshire <- tq_get(c("BRK-A", "BRK-B"), from = as.Date("1900-01-01"))

berkshire %>% 
  group_by(symbol) %>% 
  summarise(inception = min(date),
            performance = percent(last(adjusted)/first(adjusted)-1)) %>% 
  arrange(inception)

spy <- tq_get("SPY", from = as.Date("1900-01-01"))

berkshire %>% 
  bind_rows(spy) %>% 
  filter(date >= as.Date("1996-05-09")) %>% 
  group_by(symbol) %>% 
  summarise(inception = min(date),
            performance = percent(last(adjusted)/first(adjusted)-1),
            annual_performance = (last(adjusted)/first(adjusted))^(252/n())-1,
            n = n()) %>% 
  arrange(inception)

by_100 <- function(x) {return(x/100)}

# Fama French factors
ff_factors <- read_csv("data/F-F_Research_Data_5_Factors_2x3_daily.CSV", skip = 2) %>% 
  mutate(date = ymd(X1)) %>% 
  select(date, 2:7) %>% 
  mutate_if(is.numeric, by_100) %>% 
  filter(!is.na(date))

# BRK returns
brk_returns <- berkshire %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r))

# CAPM
library(stargazer)

ff_factor_models <- brk_returns %>% 
  left_join(ff_factors,
            by = "date") %>% 
  mutate(`Stock-RF` = r-RF) %>% 
  do(model = lm(`Stock-RF` ~ `Mkt-RF` + SMB + HML + RMW + CMA , data = .))

ff_factor_models$model %>% stargazer::stargazer(type = "text", column.labels = ff_factor_models$symbol)



# Monthly returns
period_return <- function(x) {
  return(prod(1+x)-1)
}

ff_factor_models_monthly <- brk_returns %>% 
  left_join(ff_factors,
            by = "date") %>% 
  mutate(`Stock-RF` = r-RF) %>% 
  group_by(month = floor_date(date, "months"), symbol) %>% 
  summarise_if(is.numeric, period_return) %>% 
  group_by(symbol) %>% 
  do(model = lm(`Stock-RF` ~ `Mkt-RF` + SMB + HML + RMW + CMA , data = .))

ff_factor_models_monthly$model %>% stargazer::stargazer(type = "text", column.labels = ff_factor_models_monthly$symbol)

# Rolling Alpha ----
regr_fun <- function(d, col = "r", annualize = T, return = "alpha") {
  # https://www.r-bloggers.com/2017/04/tidyquant-0-5-0-select-rollapply-and-quandl/
  # https://stackoverflow.com/questions/55877110/pass-dynamically-variable-names-in-lm-formula-inside-a-function
  coefs <- lm(d[,col] ~ d[,"Mkt-RF"] + d[,"SMB"] + d[,"HML"] + d[,"RMW"] + d[,"CMA"]) %>%
    coef()
  if (annualize) {
    alpha = (1+coefs[1])^12-1
  } else {
    alpha = coefs[1]
  }
  if (return == "alpha") {
    return(alpha)
  } else if (return == "beta") {
    beta <- coefs[2]
    return(beta)
  }
}

alpha <- brk_returns %>% 
  left_join(ff_factors,
            by = "date") %>%
  mutate(`Stock-RF` = r - RF) %>% 
  group_by(date = floor_date(date, "months"), symbol) %>% 
  summarise_if(is.numeric, period_return) %>% 
  group_by(symbol) %>% 
  # ungroup() %>% 
  # filter(symbol == "ARKK") %>% 
  tq_mutate(mutate_fun = rollapply,
            width      = 12,
            FUN        = regr_fun,
            by.column  = FALSE,
            col = "r",
            annualize = T,
            col_rename = "alpha") %>%
  select(date, symbol, starts_with("alpha"))

alpha %>% 
  ggplot(aes(x = date, y = alpha, color = symbol)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.2) +
  scale_y_continuous(labels = percent, breaks = seq(from = -0.4, to = 1.5, by = 0.2)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = c(0,0)) +
  # scale_color_manual(values = c("#61a0d7", "#ee8336")) +
  labs(x = "", y = "", color = "", fill = "",
       title = "Rolling 252-day Alpha (over QQQ)") +
  theme_linedraw() +
  theme(legend.position = "top")

marketbeta <- brk_returns %>% 
  left_join(ff_factors,
            by = "date") %>%
  mutate(`Stock-RF` = r - RF) %>% 
  group_by(date = floor_date(date, "months"), symbol) %>% 
  summarise_if(is.numeric, period_return) %>% 
  group_by(symbol) %>% 
  # ungroup() %>% 
  # filter(symbol == "ARKK") %>% 
  tq_mutate(mutate_fun = rollapply,
            width      = 12,
            FUN        = regr_fun,
            by.column  = FALSE,
            col = "r",
            annualize = T,
            return = "beta",
            col_rename = "beta") %>%
  select(date, symbol, starts_with("beta"))

marketbeta %>% 
  ggplot(aes(x = date, y = beta, color = symbol)) +
  geom_line() +
  geom_hline(yintercept = 0, size = 0.2) +
  # scale_y_continuous(labels = percent, breaks = seq(from = -0.4, to = 1.5, by = 0.2)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = c(0,0)) +
  # scale_color_manual(values = c("#61a0d7", "#ee8336")) +
  labs(x = "", y = "", color = "", fill = "",
       title = "Rolling Market Beta from FF-5Facor model") +
  theme_linedraw() +
  theme(legend.position = "top")

# Key Take-Aways



