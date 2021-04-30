library(dplyr)
library(readr)
library(stringr)
library(rebus)
library(leaflet)
library(rgdal)
library(rgeos)
library(igraph)
library(tidyr)
library(visNetwork)
library(purrr)
library(RColorBrewer)
library(scales)
library(psych)
library(ggplot2)
library(plotly)
library(ggrepel)
setwd("C:/Users/Fred/Downloads/code/cda_proj")

b_pattern = escape_special("(") %R%
  one_or_more(negated_char_class(escape_special(")"))) %R%
  escape_special(")")

SSC_2016_AUST <- read_csv("clustering_fy/Data/ssc/SSC_2016_AUST.csv")

SSC_2016_AUST_CLEAN = SSC_2016_AUST %>%
  filter(STATE_NAME_2016 == 'New South Wales') %>%
  mutate(SSC_NAME_2016 = str_trim(str_remove(SSC_NAME_2016, b_pattern))) %>%
  select(SSC_CODE_2016, SSC_NAME_2016, STATE_NAME_2016) %>%
  distinct() %>%
  dplyr::distinct(ss, .keep_all = TRUE)  %>%
  arrange(SSC_CODE_2016)

# Income distress by suburb
income_2016 = read_csv("clustering_fy/Data/census_data/2016_Household_income.csv")[, c(1, 4:5)]
colnames(income_2016) = c("ss", "low_income", "high_income")
income_2016 = income_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Income by suburb
income2_2016 = read_csv("clustering_fy/Data/census_data/2016_Median_weekly_incomes.csv")[, c(1:4)]
colnames(income2_2016) = c("ss", "p_income", "f_income", "h_income")
income2_2016 = income2_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Mortgage by suburb
mortgage_2016 = read_csv("clustering_fy/Data/census_data/2016_Mortgage_monthly_repayments.csv")[, c(1, 2, 7)]
colnames(mortgage_2016) = c("ss", "repay", "repay_distress")
mortgage_2016 = mortgage_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Language by suburb
language_top_responses_2016 = read_csv("clustering_fy/Data/census_data/2016_Language_top_responses5.csv")
language_2016 = language_top_responses_2016 %>%
  mutate(ss = str_trim(str_remove(X1, b_pattern))) %>%
  select(ss, Mandarin, `Mandarin (%)`, Cantonese, `Cantonese (%)`) %>%
  mutate(chinese_a = replace_na(Mandarin, 0) + replace_na(Cantonese, 0)) %>%
  mutate(chinese_p = replace_na(`Mandarin (%)`, 0)+replace_na(`Cantonese (%)`, 0)) %>%
  mutate(all_p = round(chinese_a*100/chinese_p, 0)) %>%
  select(ss, chinese_p) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Tenure by suburb
tenure_2016 = read_csv("clustering_fy/Data/census_data/2016_Tenure.csv") %>%
  select(c(1,7:9))
colnames(tenure_2016) = c("ss", "owner", "mortgage", "rent")
tenure_2016 = tenure_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Rent by suburb
rent_2016 = read_csv("clustering_fy/Data/census_data/2016_Rent_weekly_payments.csv") %>%
  select(c(1, 2, 7))
colnames(rent_2016) = c("ss", "rental", "rent_distress")
rent_2016 = rent_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Dwelling structure by suburb
dw_2016 = read_csv("clustering_fy/Data/census_data/2016_Occupied_private_dwellings.csv") %>%
  select(c(1, 6:8))
colnames(dw_2016) = c("ss", "house", "semi", "unit")
dw_2016 = dw_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(ss, .keep_all = TRUE)

dw_2016 %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# SSC to POA
ssc_poa = read_csv("clustering_fy/Data/POA to SSC mapping.csv") %>%
  select(POA_CODE_2016, SSC_CODE_2016)

# SSC mapping
ssc = read_csv("clustering_fy/Data/SSC_final.csv") %>%
  left_join(ssc_poa, by = "SSC_CODE_2016") %>%
  mutate(ss=SSC_2006_NAME, ssc=SSC_CODE_2016) %>%
  filter(!is.na(POA_CODE_2016)) %>%
  select(ss, ssc, POA_CODE_2016)

# COVID input
covid = read_csv("clustering_fy/Data/confirmed_cases_table4_location_likely_source.csv") %>%
  filter(substr(likely_source_of_infection, 1, 5) == 'Local') %>%
  select(notification_date, postcode, lga_name19)

# Suburb data together
all_table = income_2016 %>%
  inner_join(income2_2016, by = 'ss') %>%
  inner_join(language_2016, by = "ss") %>%
  inner_join(tenure_2016, by = "ss") %>%
  inner_join(rent_2016, by = "ss") %>%
  inner_join(mortgage_2016, by = "ss") %>%
  inner_join(dw_2016, by = "ss") %>%
  inner_join(ssc, by = "ss") %>%
  dplyr::distinct(ss, .keep_all = TRUE)

# Suburbs with cluster
all_table_cluster = bind_cols(all_table, all_table %>%
  select(-c("ss", "ssc", "POA_CODE_2016")) %>%
  helper_pc_convert() %>%
  helper_clustering() %>%
  select(cluster))

all_table_h_cluster = bind_cols(all_table %>% select(ss, ssc, POA_CODE_2016), all_table %>%
                                select(-c("ss", "ssc", "POA_CODE_2016")) %>%
                                helper_pc_convert() %>%
                                helper_h_clustering() %>%
                                select(cluster))

all_table_cluster = bind_cols(all_table %>% select(ss, ssc, POA_CODE_2016), all_table %>%
                                  select(-c("ss", "ssc", "POA_CODE_2016")) %>%
                                  helper_pc_convert() %>%
                                  helper_clustering() %>%
                                  select(cluster))



helper_effective(all_table_h_cluster, lockdon_n = 14)
helper_effective(all_table_cluster, lockdon_n = 14)
