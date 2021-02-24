# We have found:
# 1. daily leverage can result in a compounding decay (but does not have to)
# 2. LETF do a rather poor job at delivering the leveraged daily return
# Both 1 and 2 affect LETF performance negatively especially in volatile markets

# Q: Can we make money on this?

# Idea: Short the leveraged ETF and hedge the position.
# strategy1: short SSO (2x SPX) and long SPY
# strategy2: short UPRO (3x) and long SPY
letf_tickers <- c("SPY", "SSO", "UPRO")
letf <- tq_get(letf_tickers, from = as.Date("2006-07-01"))
letf_r <- letf %>% 
  group_by(symbol) %>% 
  transmute(date, r = adjusted/lag(adjusted)-1) %>% 
  filter(!is.na(r)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = symbol, values_from = r)


strategy1 <- letf_r %>% 
  mutate(strategy1 = 2*SPY - SSO)

strategy1_long <- strategy1 %>% 
  select(-UPRO) %>% 
  pivot_longer(cols = -date) 

strategy1_long %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000) %>% 
  ggplot(aes(x = date, y = p, color = name)) +
  scale_y_log10() +
  geom_line() +
  theme_classic()

strategy1_long %>% 
  group_by(name) %>% 
  summarise(annual_return = prod(1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252)) %>% 
  mutate(SR_rf0 = annual_return/annual_vola)



strategy3 <- letf_r %>% 
  filter(!is.na(UPRO)) %>% 
  mutate(strategy3 = 3*SPY - UPRO)

strategy3_long <- strategy3 %>% 
  select(-SSO) %>% 
  pivot_longer(cols = -date) 

strategy3_long %>% 
  group_by(name) %>% 
  mutate(p = cumprod(1+value)*1000) %>% 
  ggplot(aes(x = date, y = p, color = name)) +
  geom_line() +
  scale_y_log10() +
  theme_classic()

strategy2_long %>% 
  group_by(name) %>% 
  summarise(annual_return = prod(1+mean(value))^252-1,
            annual_vola = sd(value)*sqrt(252)) %>% 
  mutate(SR_rf0 = annual_return/annual_vola)

# key take-aways
# - the strategy to exploit the LETF performance lag looks promising (on paper!)
# - volatile markets favor the performance of such a strategy
# - there are other ways to play it, for example also including inverse leveraged ETFs
# - BUT: it's not so easy in reality (requires rebalancing and cost of shorting)


