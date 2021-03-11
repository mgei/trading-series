library(tidyverse)
library(tidyquant)
library(scales)

stocks_chart <- function(data) {
  chart <- data %>% 
    group_by(name) %>% 
    mutate(p = cumprod(1+r)*1000) %>% 
    ggplot(aes(x = date, y = p, color = name)) +
    geom_line() +
    labs(x = "", y = "", color = "") +
    theme_classic() +
    theme(legend.position = "top")
  return(chart)
}

# How to select an ETF? How to know if a certain ETF fits in your portfolio?

# 1. Exposure
# 2. Risk
# 3. Tracking Error

# Exposure: Does the ETF really cover a different/uncorrelated return stream than what you already have.

# Imagine we already have portfolio consisting of the following:
# 20% MSCI World
# 20% US Tech
# 20% Small Caps US
# 20% Gold
# 20% Long term treasuries

# TAN	Invesco Solar ETF
# BOTZ	Global X Robotics & Artificial Intelligence Thematic ETF
