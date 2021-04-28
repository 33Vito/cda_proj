library(tidyverse)
library(rgdal)
library(sp)
source("R_functions.R")

NYC_MODZCTA_2010 <- readOGR( 
  dsn= "data/NYC_coronavirus-data-master/Geography-resources" , 
  layer="MODZCTA_2010",
  verbose=FALSE)

# Total case by MODZCTA --------------------------------------------------------
NYC_cases_by_MODZCTA <- read_csv("data/NYC_coronavirus-data-master/totals/data-by-modzcta.csv") %>% 
  rename(MODZCTA = MODIFIED_ZCTA)

NYC_cases_by_MODZCTA %>% 
  mutate(MODZCTA = fct_reorder(as.factor(MODZCTA), PERCENT_POSITIVE)) %>% 
  mutate(PERCENT_POSITIVE = round(PERCENT_POSITIVE/1,1)) %>%
  select(MODZCTA, PERCENT_POSITIVE) %>% 
  plot_map_TL(NYC_MODZCTA_2010, "MODZCTA", "PERCENT_POSITIVE", 
              "Total Covid-19 positive rate (%) by MODZCTA (NYC)", 
              show_count = T, label_size = 2, map_lp=c(.23, .85))

# Total anibody by MODZCTA --------------------------------------------------------
NYC_antibody_by_MODZCTA <- read_csv("data/NYC_coronavirus-data-master/totals/antibody-by-modzcta.csv") %>% 
  rename(MODZCTA = modzcta_first)

NYC_antibody_by_MODZCTA %>% 
  mutate(MODZCTA = fct_reorder(as.factor(MODZCTA), PERCENT_POSITIVE)) %>% 
  mutate(PERCENT_POSITIVE = round(PERCENT_POSITIVE/1,1)) %>%
  select(MODZCTA, PERCENT_POSITIVE) %>% 
  plot_map_TL(NYC_MODZCTA_2010, "MODZCTA", "PERCENT_POSITIVE", 
              "Total Covid-19 antibody rate (%) by MODZCTA (NYC)", 
              show_count = T, label_size = 2, map_lp=c(.23, .85))

# Weekly percentpositive by MODZCTA --------------------------------------------------------
NYC_percentpositive_by_MODZCTA_weekly <- read_csv(
  "data/NYC_coronavirus-data-master/trends/percentpositive-by-modzcta.csv") %>% 
  gather(MODZCTA, percentpositive, -week_ending ) %>% 
  mutate(week_ending = mdy(week_ending)) %>% 
  mutate(MODZCTA = str_extract(MODZCTA, "(?<=_).*"))

NYC_percentpositive_by_MODZCTA_weekly %>% 
  filter(week_ending == ymd("2021-04-17")) %>% 
  mutate(MODZCTA = fct_reorder(as.factor(MODZCTA), percentpositive)) %>% 
  mutate(PERCENT_POSITIVE = round(percentpositive/1,1)) %>%
  select(MODZCTA, percentpositive) %>% 
  plot_map_TL(NYC_MODZCTA_2010, "MODZCTA", "percentpositive", 
              "Total Covid-19 positive rate (%) by MODZCTA (NYC), week ending 2021-04-17", 
              show_count = T, label_size = 2, map_lp=c(.23, .85))
