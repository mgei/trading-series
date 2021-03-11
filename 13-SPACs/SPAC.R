library(tidyverse)
library(tidyquant)

# Download SPACS list from https://www.spactrax.com/spac-spreadsheet.html

spacs <- read_csv("data/SPAC Spreadsheet - Sort, filter, and export the SPACTRAX list.csv")

spacs %>% 
  glimpse()

spacs %>% 
  group_by(Stage, `Target Identified?`) %>% 
  count()

spacs %>% 
  group_by(Stage, `Approx. Age (in months)`) %>% 
  count() %>% 
  ggplot(aes(x = `Approx. Age (in months)`, y = n, fill = Stage)) +
  geom_col() +
  facet_wrap(~Stage) +
  theme_linedraw() +
  theme(legend.position = "none")

spacs %>% 
  group_by(Stage, `Target Identified?`) %>% 
  count()

spacs %>% 
  ggplot(aes(x = `IPO Date`)) + 
  geom_histogram() +
  theme_linedraw()

spacs %>% 
  group_by(has_IPO_date = !is.na(`IPO Date`)) %>% 
  count()

spacs %>% 
  group_by(Stage, `Target Identified?`, has_IPO_date = !is.na(`IPO Date`)) %>% 
  count()

# Let's look at some examples
# 1. Virgin Galactic (SPCE)
spacs %>% 
  filter(`Shares Symbol` == "SPCE") %>% 
  glimpse()

spce <- tq_get("SPCE")
spce %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  geom_vline(xintercept = spacs %>% filter(`Shares Symbol` == "SPCE") %>% pull(`Definitive Agreement`), color = "red") +
  geom_vline(xintercept = spacs %>% filter(`Shares Symbol` == "SPCE") %>% pull(`Merger Completion Date`), color = "blue") +
  theme_linedraw()

# 2. Nikola (NKLA)
spacs %>% 
  filter(`Shares Symbol` == "NKLA") %>% 
  glimpse()

nkla <- tq_get("NKLA")
nkla %>% 
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  geom_vline(xintercept = spacs %>% filter(`Shares Symbol` == "NKLA") %>% pull(`Definitive Agreement`), color = "red") +
  geom_vline(xintercept = spacs %>% filter(`Shares Symbol` == "NKLA") %>% pull(`Merger Completion Date`), color = "blue") +
  theme_linedraw()

# What are the merger complet ones trading at?
spacs %>% 
  filter(Stage == "5. Merger Complete") %>% 
  filter(!is.na(`Shares Last Close`)) %>% 
  ggplot(aes(y = reorder(paste0(`Target Name`, " (", `Shares Symbol`, ")"), `Shares Last Close`), x = `Shares Last Close`)) +
  geom_point() +
  geom_vline(xintercept = 10, color = "red") +
  labs(y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 7))

spacs %>% 
  filter(Stage == "3. Target Announced") %>% 
  filter(!is.na(`Shares Last Close`)) %>% 
  ggplot(aes(y = reorder(paste0(`Target Name`, " (", `Shares Symbol`, ")"), `Shares Last Close`), x = `Shares Last Close`)) +
  geom_point() +
  geom_vline(xintercept = 10, color = "blue") +
  labs(y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 7))

spacs %>% 
  filter(Stage == "2. Seeking Target") %>% 
  filter(!is.na(`Shares Last Close`)) %>% 
  arrange(desc(`Shares Last Close`)) %>% 
  head(nrow(.)/2) %>% 
  ggplot(aes(y = reorder(paste0(`Name`, " (", `Shares Symbol`, ")"), `Shares Last Close`), x = `Shares Last Close`)) +
  geom_point() +
  geom_vline(xintercept = 10, color = "blue") +
  labs(y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 7))

spacs %>% 
  filter(Stage == "2. Seeking Target") %>% 
  filter(!is.na(`Shares Last Close`)) %>% 
  arrange(desc(`Shares Last Close`)) %>% 
  tail(nrow(.)/2) %>% 
  ggplot(aes(y = reorder(paste0(`Name`, " (", `Shares Symbol`, ")"), `Shares Last Close`), x = `Shares Last Close`)) +
  geom_point() +
  geom_vline(xintercept = 10, color = "blue") +
  labs(y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 7))
  
