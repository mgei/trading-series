library(tidyverse)
library(tidyquant)

yahoo_monthly_rets <- function(symbol) {
  tq_get(symbol) %>%
    group_by(m = floor_date(date, "months")) %>%
    filter(date == max(date)) %>% 
    ungroup() %>% 
    select(-m) %>% 
    transmute(symbol, date, r = adjusted/lag(adjusted)-1) %>% 
    filter(!is.na(r))
}

lookup <- read_lines("SNPE   Xtrackers S&P 500 ESG ETF
                      ESGU   iShares ESG Aware MSCI USA ETF
                      SUSA   iShares MSCI USA ESG Select ETF 
                      SUSL   iShares ESG MSCI USA Leaders ETF
                      USXF   iShares ESG Advanced MSCI USA ETF 
                      IQSU   IQ Candriam ESG US Equity ETF
                      NUEM   Nuveen ESG Emerging Markets Equity ETF
                      EMSG   Xtrackers MSCI Emerging Markets ESG Leaders Equity ETF
                      RESE   WisdomTree Emerging Markets ESG Fund
                      ESGD   iShares ESG Aware MSCI EAFE ETF
                      EASG   Xtrackers MSCI EAFE ESG Leaders Equity ETF
                      DMXF   iShares ESG Advanced MSCI EAFE Index ETF")

lookup <- tibble(raw = lookup %>% 
         str_trim()) %>% 
  separate(col = raw, into = c("symbol", "name"), sep = "  ") %>% 
  mutate(name = str_trim(name))

# SNPE   Xtrackers S&P 500 ESG ETF - 2019-07-31
# Benchmark: SPY
SNPE <- yahoo_monthly_rets("SNPE")
SPY  <- yahoo_monthly_rets("SPY")

SNPE <- SNPE %>% 
  inner_join(SPY %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)

# ESGU   iShares ESG Aware MSCI USA ETF - 2017-10-31
# SUSA   iShares MSCI USA ESG Select ETF - 2017-10-31
# SUSL   iShares ESG MSCI USA Leaders ETF - 2019-06-28
# USXF   iShares ESG Advanced MSCI USA ETF - 2020-07-31
# Benchmark: PBUS
ESGU <- yahoo_monthly_rets("ESGU")
SUSA <- yahoo_monthly_rets("SUSA")
SUSL <- yahoo_monthly_rets("SUSL")
USXF <- yahoo_monthly_rets("USXF")
PBUS <- yahoo_monthly_rets("PBUS")

ESGU <- ESGU %>% 
  inner_join(PBUS %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
SUSA <- SUSA %>% 
  inner_join(PBUS %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
SUSL <- SUSL %>% 
  inner_join(PBUS %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
USXF <- USXF %>% 
  inner_join(PBUS %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)


# IQSU   IQ Candriam ESG US Equity ETF - 2020-01-31
# Benchmark: VTI
IQSU <- yahoo_monthly_rets("IQSU")
VTI  <- yahoo_monthly_rets("VTI")

IQSU <- IQSU %>% 
  inner_join(VTI %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)

# NUEM   Nuveen ESG Emerging Markets Equity ETF - 2017-07-31
# EMSG   Xtrackers MSCI Emerging Markets ESG Leaders Equity ETF - 2019-01-31
# RESE   WisdomTree Emerging Markets ESG Fund - 2020-04-30
# Benchmark: EEM
NUEM <- yahoo_monthly_rets("NUEM")
EMSG <- yahoo_monthly_rets("EMSG")
RESE <- yahoo_monthly_rets("RESE")
EEM <- yahoo_monthly_rets("EEM")

NUEM <- NUEM %>% 
  inner_join(EEM %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
EMSG <- EMSG %>% 
  inner_join(EEM %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
RESE <- RESE %>% 
  inner_join(EEM %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)

# ESGD   iShares ESG Aware MSCI EAFE ETF - 2016-09-30
# EASG   Xtrackers MSCI EAFE ESG Leaders Equity ETF - 2018-12-31
# DMXF   iShares ESG Advanced MSCI EAFE Index ETF - 2020-07-31
# Benchmark: IEFA
ESGD <- yahoo_monthly_rets("ESGD")
EASG <- yahoo_monthly_rets("EASG")
DMXF <- yahoo_monthly_rets("DMXF")
IEFA <- yahoo_monthly_rets("IEFA")

ESGD <- ESGD %>% 
  inner_join(IEFA %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
EASG <- EASG %>% 
  inner_join(IEFA %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)
DMXF <- DMXF %>% 
  inner_join(IEFA %>% 
               select(date, bm = r),
             by = "date") %>% 
  mutate(r_excess = r - bm)


df_r <- full_join(SNPE %>% select(date, SNPE = r),
          ESGU %>% select(date, ESGU = r),
          by = "date") %>% 
  full_join(SUSA %>% select(date, SUSA = r),
            by = "date") %>% 
  full_join(SUSL %>% select(date, SUSL = r),
            by = "date") %>% 
  full_join(USXF %>% select(date, USXF = r),
            by = "date") %>% 
  full_join(IQSU %>% select(date, IQSU = r),
            by = "date") %>% 
  full_join(NUEM %>% select(date, NUEM = r),
            by = "date") %>% 
  full_join(EMSG %>% select(date, EMSG = r),
            by = "date") %>% 
  full_join(RESE %>% select(date, RESE = r),
            by = "date") %>% 
  full_join(ESGD %>% select(date, ESGD = r),
            by = "date") %>% 
  full_join(EASG %>% select(date, EASG = r),
            by = "date") %>% 
  full_join(DMXF %>% select(date, DMXF = r),
            by = "date") %>% 
  arrange(date) %>% 
  pivot_longer(-date, values_to = "r", names_to = "symbol") %>% 
  arrange(symbol, date)

df_excess_r <- full_join(SNPE %>% select(date, SNPE = r_excess),
                         ESGU %>% select(date, ESGU = r_excess),
                         by = "date") %>% 
  full_join(SUSA %>% select(date, SUSA = r_excess),
            by = "date") %>% 
  full_join(SUSL %>% select(date, SUSL = r_excess),
            by = "date") %>% 
  full_join(USXF %>% select(date, USXF = r_excess),
            by = "date") %>% 
  full_join(IQSU %>% select(date, IQSU = r_excess),
            by = "date") %>% 
  full_join(AVDG %>% select(date, AVDG = r_excess),
            by = "date") %>% 
  full_join(IVLC %>% select(date, IVLC = r_excess),
            by = "date") %>% 
  full_join(NUEM %>% select(date, NUEM = r_excess),
            by = "date") %>% 
  full_join(EMSG %>% select(date, EMSG = r_excess),
            by = "date") %>% 
  full_join(RESE %>% select(date, RESE = r_excess),
            by = "date") %>% 
  full_join(ESGD %>% select(date, ESGD = r_excess),
            by = "date") %>% 
  full_join(EASG %>% select(date, EASG = r_excess),
            by = "date") %>% 
  full_join(DMXF %>% select(date, DMXF = r_excess),
            by = "date") %>% 
  arrange(date) %>% 
  pivot_longer(-date, values_to = "r", names_to = "symbol") %>% 
  arrange(symbol, date)

df_r %>% 
  filter(date >= as.Date("2017-06-01")) %>% 
  ggplot(aes(x = date, y = r, fill = symbol)) +
  geom_col(position = "dodge") +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "none")

df_excess_r %>% 
  filter(date >= as.Date("2017-07-01")) %>% 
  ggplot(aes(x = date, y = r, fill = symbol)) +
  geom_col(position = "dodge") +
  labs(x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "none")

df_excess_r %>% 
  filter(date >= as.Date("2019-07-01")) %>% 
  arrange(date) %>% 
  left_join(lookup, by = "symbol") %>% 
  mutate(name = gsub('(.{18})\\s(.*)', '\\1\n\\2', name)) %>% 
  select(-symbol) %>% 
  group_by(name) %>% 
  filter(all(!is.na(r))) %>% 
  pivot_wider(names_from = name, values_from = r) %>% 
  select(-date) %>% 
  cor() %>% 
  corrplot::corrplot(diag = T, type = "lower", method = "number", tl.cex=0.9)
