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
library(rgdal)
library(rgeos)

setwd("C:/Users/Fred/Downloads/code/cda_proj")

b_pattern = escape_special("(") %R%
  one_or_more(negated_char_class(escape_special(")"))) %R%
  escape_special(")")

SSC_2016_AUST <- read_csv("clustering_fy/Data/ssc/SSC_2016_AUST.csv")

SSC_2016_AUST_CLEAN = SSC_2016_AUST %>%
  filter(STATE_NAME_2016 == 'New South Wales') %>%
  mutate(SSC_NAME_2016 = str_trim(str_remove(SSC_NAME_2016, b_pattern))) %>%
  select(SSC_CODE_2016, SSC_NAME_2016, STATE_NAME_2016) %>%
  dplyr::distinct() %>%
  dplyr::distinct(SSC_CODE_2016, .keep_all = TRUE)  %>%
  arrange(SSC_CODE_2016)

# Income distress by suburb
income_2016 = read_csv("clustering_fy/Data/census_data/2016_Household_income.csv")[, c(1, 4:6)]
colnames(income_2016) = c("ss", "low_income", "high_income", "SSC_CODE")
income_2016 = income_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Income by suburb
income2_2016 = read_csv("clustering_fy/Data/census_data/2016_Median_weekly_incomes.csv")[, c(1:4, 8)]
colnames(income2_2016) = c("ss", "p_income", "f_income", "h_income", "SSC_CODE")
income2_2016 = income2_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Mortgage by suburb
mortgage_2016 = read_csv("clustering_fy/Data/census_data/2016_Mortgage_monthly_repayments.csv")[, c(1, 2, 7, 8)]
colnames(mortgage_2016) = c("ss", "repay", "repay_distress", "SSC_CODE")
mortgage_2016 = mortgage_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Language by suburb
language_top_responses_2016 = read_csv("clustering_fy/Data/census_data/2016_Language_top_responses5.csv")
language_2016 = language_top_responses_2016 %>%
  mutate(ss = str_trim(str_remove(X1, b_pattern))) %>%
  select(ss, Mandarin, `Mandarin (%)`, Cantonese, `Cantonese (%)`, 
         `English only spoken at home`, `Households where a non English language is spoken`,
         SSC_CODE) %>%
  mutate(chinese_a = replace_na(Mandarin, 0) + replace_na(Cantonese, 0),
         english_only_n = `English only spoken at home`,
         non_english_n = `Households where a non English language is spoken`) %>%
  mutate(chinese_p = replace_na(`Mandarin (%)`, 0)+replace_na(`Cantonese (%)`, 0)) %>%
  mutate(all_p = round(chinese_a*100/chinese_p, 0)) %>%
  select(ss, chinese_p, english_only_n, non_english_n, SSC_CODE) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Tenure by suburb
tenure_2016 = read_csv("clustering_fy/Data/census_data/2016_Tenure.csv") %>%
  select(c(1,7:9), SSC_CODE)
colnames(tenure_2016) = c("ss", "owner", "mortgage", "rent", "SSC_CODE")
tenure_2016 = tenure_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Rent by suburb
rent_2016 = read_csv("clustering_fy/Data/census_data/2016_Rent_weekly_payments.csv") %>%
  select(c(1, 2, 7), SSC_CODE)
colnames(rent_2016) = c("ss", "rental", "rent_distress", "SSC_CODE")
rent_2016 = rent_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# Dwelling structure by suburb
dw_2016 = read_csv("clustering_fy/Data/census_data/2016_Occupied_private_dwellings.csv") %>%
  select(c(1, 6:8), SSC_CODE)
colnames(dw_2016) = c("ss", "house", "semi", "unit", "SSC_CODE")
dw_2016 = dw_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern))) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

# SSC to POA
ssc_poa = read_csv("clustering_fy/Data/POA to SSC mapping.csv") %>%
  select(POA_CODE_2016, SSC_CODE_2016, n_MB_CODE_2016)

# SSC mapping
ssc = read_csv("clustering_fy/Data/SSC_final.csv") %>%
  left_join(ssc_poa, by = "SSC_CODE_2016") %>%
  mutate(ss=SSC_2006_NAME, SSC_CODE=SSC_CODE_2016) %>%
  filter(!is.na(POA_CODE_2016)) %>%
  select(ss, SSC_CODE, POA_CODE_2016, n_MB_CODE_2016)

# COVID input
covid = read_csv("clustering_fy/Data/confirmed_cases_table4_location_likely_source.csv") %>%
  filter(substr(likely_source_of_infection, 1, 5) == 'Local') %>%
  select(notification_date, postcode, lga_name19)

# Suburb data together
all_table_raw = income_2016 %>%
  inner_join(income2_2016 %>% select(-ss), by = 'SSC_CODE') %>%
  inner_join(language_2016 %>% select(-ss), by = "SSC_CODE") %>%
  inner_join(tenure_2016 %>% select(-ss), by = "SSC_CODE") %>%
  inner_join(rent_2016 %>% select(-ss), by = "SSC_CODE") %>%
  inner_join(mortgage_2016 %>% select(-ss), by = "SSC_CODE") %>%
  inner_join(dw_2016 %>% select(-ss), by = "SSC_CODE") %>%
  inner_join(ssc %>% select(-ss), by = "SSC_CODE") %>%
  filter(!is.na(SSC_CODE)) %>%
  dplyr::distinct(SSC_CODE, .keep_all = TRUE)

helper_weighted_avg = function(feature) {
  return(sum(feature * n_MB_CODE_2016)/sum(n_MB_CODE_2016))
}

all_table = all_table_raw %>%
  select(-c(ss, SSC_CODE)) %>%
  group_by(POA_CODE_2016) %>%
  summarise(low_income = sum(low_income * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            high_income = sum(high_income * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            p_income = sum(p_income),
            f_income = sum(f_income),
            h_income = sum(h_income),
            chinese_p = sum(chinese_p * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            english_only_n = sum(english_only_n),
            non_english_n = sum(non_english_n),
            owner = sum(owner * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            rent = sum(rent * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            rental = sum(rental * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            rent_distress = sum(rent_distress * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            repay = sum(repay * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            repay_distress = sum(repay_distress * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            house = sum(house * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            semi = sum(semi * n_MB_CODE_2016)/sum(n_MB_CODE_2016),
            unit = sum(unit * n_MB_CODE_2016)/sum(n_MB_CODE_2016))
  

readr::write_csv(all_table, "clustering_fy/Data/census_feature_by_POA.csv")

# Suburbs with cluster

all_table_h_cluster = bind_cols(all_table %>% select(POA_CODE_2016), all_table %>%
                                  select(-c("POA_CODE_2016")) %>%
                                helper_pc_convert() %>%
                                helper_h_clustering(k=6) %>%
                                select(cluster))

all_table_cluster = bind_cols(all_table %>% select(POA_CODE_2016), all_table %>%
                                select(-c("POA_CODE_2016")) %>%
                                  helper_pc_convert() %>%
                                  helper_clustering(k=6) %>%
                                  select(cluster))

# Base scenario - assume government locks down neighbourhood suburbs

# Import shape file
suburbs = readRDS("gravity_covid_TL/data/SYD_POA.rds")
ss_col = suburbs$POA_NAME16
ss_adj = gTouches(suburbs, byid = T)
colnames(ss_adj) = ss_col
rownames(ss_adj) = ss_col
link = as.data.frame(as.table(ss_adj))


# Only relevant suburbs included
suburbs2 = subset(suburbs, suburbs$SSC_CODE16 %in% SSC_2016_AUST$SSC_CODE_2016)
ss_col = suburbs2$SSC_CODE16

ss_adj = gTouches(suburbs2, byid = T)

colnames(ss_adj) = ss_col
rownames(ss_adj) = ss_col

# Convert contingency table into relationship table
adj_suburb = as.data.frame(as.table(ss_adj))%>%
  filter(Freq==T)
colnames(link)


# Add suburbs view
adj_suburb = link %>%
  filter(Freq==T) %>%
  inner_join(ssc_poa %>% mutate(SSC_CODE_2016 = as.factor(SSC_CODE_2016)), by = c("Var1" = "SSC_CODE_2016")) %>%
  mutate(POA1 = POA_CODE_2016) %>%
  select(-c(Freq, POA_CODE_2016)) %>%
  inner_join(ssc_poa %>% mutate(SSC_CODE_2016 = as.factor(SSC_CODE_2016)), by = c("Var2" = "SSC_CODE_2016")) %>%
  mutate(POA2 = POA_CODE_2016) %>%
  filter(POA1 != POA2) %>%
  select(-POA_CODE_2016)

# Add cluster index for base scenario
adj_suburb_key = adj_suburb %>%
  bind_cols(data.frame(cluster=1:nrow(adj_suburb)))

# Form base scenario detection table
base_cluster = bind_rows(select(adj_suburb_key %>% mutate(POA_CODE_2016 = Var1), POA_CODE_2016, cluster), 
                         select(adj_suburb_key %>% mutate(POA_CODE_2016 = Var2), POA_CODE_2016, cluster)) %>%
  mutate(POA_CODE_2016 = as.numeric(as.character(POA_CODE_2016)))

helper_effective(base_cluster, lockdon_n = 14)
helper_effective(all_table_h_cluster, lockdon_n = 14)
helper_effective(all_table_cluster, lockdon_n = 14)
