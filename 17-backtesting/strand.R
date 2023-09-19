library(strand)

# Load up sample data
data(sample_secref)
data(sample_pricing)
data(sample_inputs)

# Load sample configuration file
config <- example_strategy_config()
config$from
config$to
config$solver

config$strategies$strategy_1$in_var
config$strategies$strategy_1$strategy_capital
config$strategies$strategy_1$ideal_long_weight
config$strategies$strategy_1$ideal_short_weight
config$strategies$strategy_1$position_limit_pct_lmv
config$strategies$strategy_1$position_limit_pct_smv
config$strategies$strategy_1$position_limit_pct_adv
config$strategies$strategy_1$trading_limit_pct_adv

config$strategies$strategy_1$constraints$size$type
config$strategies$strategy_1$constraints$size$in_var
config$strategies$strategy_1$constraints$size$upper_bound
config$strategies$strategy_1$constraints$size$lower_bound

config$strategies$strategy_1$constraints$sector$type
config$strategies$strategy_1$constraints$sector$in_var
config$strategies$strategy_1$constraints$sector$upper_bound
config$strategies$strategy_1$constraints$sector$lower_bound

config$turnover_limit
config$target_weight_policy

config$simulator$add_detail_columns
config$simulator$fill_rate_pct_vol
config$simulator$transaction_cost_pct
config$simulator$financing_cost_pct
config$simulator$calculate_exposures$category_vars
config$simulator$calculate_exposures$factor_vars

config$simulator$input_data$type

config$simulator$pricing_data$type
config$simulator$pricing_data$columns$close_price
config$simulator$pricing_data$columns$prior_close_price
config$simulator$pricing_data$columns$adjustment_ratio
config$simulator$pricing_data$columns$volume
config$simulator$pricing_data$columns$dividend
config$simulator$pricing_data$columns$distribution

config$simulator$secref_data$type

stocks <- tq_get(c("SPY", "GLD", "IWM"))
dividends <- tq_get(c("SPY", "GLD", "IWM"), get = "dividend")

pricing <- stocks %>% 
  left_join(dividends %>% 
              rename(dividend = value),
            by = c("symbol", "date")) %>% 
  group_by(id = symbol) %>% 
  transmute(date, id, price_unadj = close, prior_close_unadj = lag(close), 
            dividend_unadj = replace_na(dividend, 0), 
            distribution_unadj = 0, volume = volume, 
            adjustment_ratio = 1) %>% 
  ungroup() %>% 
  arrange(date, id) %>% 
  filter(!is.na(prior_close_unadj))

inputs <- stocks %>% 
  group_by(symbol) %>% 
  mutate(rc_vol = 1, 
         ma100 = lag(roll_mean(close, n = 100, fill = NA_real_, align = "right")),
         close_to_ma = close/ma100) %>% 
  filter(!is.na(ma100)) %>% 
  ungroup() %>% 
  select(date, id = symbol, rc_vol, ma100, close_to_ma)

secref <- pricing %>% 
  distinct(id) %>% 
  mutate(name = recode(id, "GLD" = "Gold", "SPY" = "SP500", "IWM" = "Small Cap"),
         symbol = id,
         sector = ifelse(id == "GLD", "Metals", "Stocks"))

config <- example_strategy_config()
config$from <- min(inputs$date)
config$to <- config$from + years(1) # max(inputs$date)
config$strategies$strategy_1$in_var <- "close_to_ma"
config$strategies$strategy_1$strategy_capital <- 50000
config$strategies$strategy_1$ideal_long_weight <- 1
config$strategies$strategy_1$ideal_short_weight <- 0 # no shorting?
config$strategies$strategy_1$position_limit_pct_lmv <- 1
config$strategies$strategy_1$position_limit_pct_smv <- 0 # no shorting?
config$strategies$strategy_1$position_limit_pct_adv <- Inf # position limit of volume, we don't care Inf
config$strategies$strategy_1$trading_limit_pct_adv <- Inf # order limit of volume, we don't care Inf

config$strategies$strategy_1 <- within(config$strategies$strategy_1, rm(constraints)) # unconstrained

config$turnover_limit <- Inf
config$target_weight_policy

config$simulator$add_detail_columns <- "close_to_ma"
config$simulator$fill_rate_pct_vol <- Inf
config$simulator$transaction_cost_pct <- 0
config$simulator$financing_cost_pct <- 0
# config$simulator$calculate_exposures$category_vars
# config$simulator$calculate_exposures$factor_vars

sim2 <- Simulation$new(config,                                           
                       raw_input_data = inputs,   # Signal, factor, and supplementary data
                       raw_pricing_data = pricing,# Pricing data 
                       security_reference_data = secref) # Security reference.
sim2$run()


# Create the Simulation object and run
sim <- Simulation$new(config,                                           
                      raw_input_data = sample_inputs %>% as_tibble(),   # Signal, factor, and supplementary data
                      raw_pricing_data = sample_pricing %>% as_tibble(),# Pricing data 
                      security_reference_data = sample_secref %>% as_tibble()) # Security reference.
sim$run()


# Print overall statistics
sim$overallStatsDf()
