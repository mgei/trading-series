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
library(tiqyquant)
library(lubridate)

ark_etfs <- tq_get(c("ARKK", "ARKG", "ARKW", "ARKF", "ARKQ"), 
                   from = as.Date("2000-01-01"))

qqq <- tq_get("QQQ", from = as.Date("2014-09-30"))

ark_etfs %>% 
  full_join(qqq %>% select(date, qqq = adjusted), by = "date") %>% 
  group_by(symbol) %>% 
  summarise(inception = min(date),
            performance = percent(last(adjusted)/first(adjusted)-1),
            performanceQQQ = percent(last(qqq)/first(qqq)-1)) %>% 
  arrange(inception)

by_100 <- function(x) {return(x/100)}

# Fama French factors
ff_factors <- read_csv("data/F-F_Research_Data_5_Factors_2x3_daily.CSV", skip = 2) %>% 
  mutate(date = ymd(X1)) %>% 
  select(date, 2:7) %>% 
  mutate_if(is.numeric, by_100) %>% 
  filter(!is.na(date))

# Fama French industries
ff_industries12 <- read_csv("data/12_Industry_Portfolios_Daily.CSV", skip = 9) %>% 
  mutate(date = ymd(X1)) %>% 
  select(date, 2:13) %>% 
  mutate_if(is.numeric, by_100) %>% 
  filter(!is.na(date))

ff_industries48 <- read_csv("data/48_Industry_Portfolios_daily.CSV", skip = 9) %>% 
  mutate(date = ymd(X1)) %>% 
  select(date, 2:49) %>% 
  mutate_if(is.numeric, by_100) %>% 
  filter(!is.na(date))

ff_industries49 <- read_csv("data/49_Industry_Portfolios_Daily.CSV", skip = 9) %>% 
  mutate(date = ymd(X1)) %>% 
  select(date, 2:50) %>% 
  mutate_if(is.numeric, by_100) %>% 
  filter(!is.na(date))

# ARK returns
ark_returns <- ark_etfs %>% 
  group_by(symbol) %>% 
  transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r))

# CAPM
library(stargazer)

ff_factor_models <- ark_returns %>% 
  left_join(ff_factors,
            by = "date") %>% 
  mutate(`ETF-RF` = r-RF) %>% 
  do(model = lm(`ETF-RF` ~ `Mkt-RF` + SMB + HML + RMW + CMA , data = .))

ff_factor_models$model %>% stargazer::stargazer(type = "text", column.labels = ff_factor_models$symbol)

ff_industry12_models <- ark_returns %>% 
  left_join(ff_industries12,
            by = "date") %>% 
  do(model = lm(r ~ NoDur + Durbl + Manuf + Enrgy + Chems + BusEq + Telcm + Utils + Shops + Hlth +  Money + Other, data = .))

ff_industry12_models$model %>% stargazer::stargazer(type = "text", column.labels = ff_industry12_models$symbol)


ff_industry49_models <- ark_returns %>% 
  left_join(ff_industries49,
            by = "date") %>% 
  do(model = lm(r ~ Agric + Food + Soda + Beer + Smoke + Toys + Fun + Books + Hshld + 
                  Clths + Hlth + MedEq + Drugs + Chems + Rubbr + Txtls + BldMt + 
                  Cnstr + Steel + FabPr + Mach + ElcEq + Autos + Aero + Ships + 
                  Guns + Gold + Mines + Coal + Oil + Util + Telcm + PerSv + BusSv + 
                  Hardw + Softw + Chips + LabEq + Paper + Boxes + Trans + Whlsl + 
                  Rtail + Meals + Banks + Insur + RlEst + Fin + Other, data = .))

ff_industry49_models$model %>% stargazer::stargazer(type = "text", column.labels = ff_industry49_models$symbol)

model <- ff_industry49_models %>% 
  filter(symbol == "ARKK") %>% 
  .[["model"]] %>% 
  .[[1]]

tidy(model) %>% 
  filter(abs(estimate) > 0.1 | term == "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, fill = p.value)) +
  geom_col()

# Monthly returns
period_return <- function(x) {
  return(prod(1+x)-1)
}

ff_industry49_models_monthly <- ark_returns %>% 
  left_join(ff_industries49,
            by = "date") %>% 
  group_by(month = floor_date(date, "months"), symbol) %>% 
  summarise_if(is.numeric, period_return) %>% 
  group_by(symbol) %>% 
  do(model = lm(r ~ Agric + Food + Soda + Beer + Smoke + Toys + Fun + Books + Hshld + 
                  Clths + Hlth + MedEq + Drugs + Chems + Rubbr + Txtls + BldMt + 
                  Cnstr + Steel + FabPr + Mach + ElcEq + Autos + Aero + Ships + 
                  Guns + Gold + Mines + Coal + Oil + Util + Telcm + PerSv + BusSv + 
                  Hardw + Softw + Chips + LabEq + Paper + Boxes + Trans + Whlsl + 
                  Rtail + Meals + Banks + Insur + RlEst + Fin + Other, data = .))

ff_industry49_models_monthly$model %>% stargazer::stargazer(type = "text", column.labels = ff_industry49_models_monthly$symbol)

model_monthly <- ff_industry49_models_monthly %>% 
  filter(symbol == "ARKK") %>% 
  .[["model"]] %>% 
  .[[1]]

tidy(model_monthly) %>% 
  filter(abs(estimate) > 0.25 | term == "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, fill = p.value)) +
  geom_col()

# other ETFs
passive_etfs <- tq_get(c("QQQ", "MGK", "XLK", "SPY"))

passive_etfs_models_monthly <- ark_returns %>% 
  left_join(passive_etfs %>% 
              transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
              pivot_wider(names_from = symbol, values_from = r),
            by = "date") %>% 
  group_by(month = floor_date(date, "months"), symbol) %>% 
  summarise_if(is.numeric, period_return) %>% 
  group_by(symbol) %>% 
  do(model = lm(r ~ QQQ, data = .))

passive_etfs_models_monthly$model %>% stargazer::stargazer(type = "text", column.labels = passive_etfs_models_monthly$symbol)


# Rolling Alpha ----
regr_fun <- function(d, col = "r", annualize = T) {
  # https://www.r-bloggers.com/2017/04/tidyquant-0-5-0-select-rollapply-and-quandl/
  # https://stackoverflow.com/questions/55877110/pass-dynamically-variable-names-in-lm-formula-inside-a-function
  coefs <- lm(d[,col] ~ d[,"QQQ"]) %>%
    coef()
  if (annualize) {
    alpha = (1+coefs[1])^12-1
  } else {
    alpha = coefs[1]
  }
  return(alpha)
}

alpha <- ark_returns %>% 
  left_join(passive_etfs %>% 
              transmute(date, symbol, r = adjusted/lag(adjusted)-1) %>% 
              pivot_wider(names_from = symbol, values_from = r),
            by = "date") %>%
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
  scale_x_date(date_breaks = "1 years", date_labels = "%Y", expand = c(0,0)) +
  # scale_color_manual(values = c("#61a0d7", "#ee8336")) +
  labs(x = "", y = "", color = "", fill = "",
       title = "Rolling 252-day Alpha (over QQQ)") +
  theme_linedraw() +
  theme(legend.position = "top")

# Key Take-Aways
# 1. All ARK funds are highly correlated among them and to the NASDAQ Composite
# 2. By investing in an ARK fund you mostly buy beta
# 3. We see Alpha since mid 2020 (an previously mid-17 to mid-18). Past excess performance might not mean much going forward.


