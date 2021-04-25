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
  arrange(SSC_CODE_2016)

# Income distress by suburb
income_2016 = read_csv("clustering_fy/Data/census_data/2016_Household_income.csv")[, c(1, 4:5)]
colnames(income_2016) = c("ss", "low_income", "high_income")
income_2016 = income_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Income by suburb
income2_2016 = read_csv("clustering_fy/Data/census_data/2016_Median_weekly_incomes.csv")[, c(1:4)]
colnames(income2_2016) = c("ss", "p_income", "f_income", "h_income")
income2_2016 = income2_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Mortgage by suburb
mortgage_2016 = read_csv("clustering_fy/Data/census_data/2016_Mortgage_monthly_repayments.csv")[, c(1, 2, 7)]
colnames(mortgage_2016) = c("ss", "repay", "repay_distress")
mortgage_2016 = mortgage_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Language by suburb
language_top_responses_2016 = read_csv("clustering_fy/Data/census_data/2016_Language_top_responses5.csv")
language_2016 = language_top_responses_2016 %>%
  mutate(ss = str_trim(str_remove(X1, b_pattern))) %>%
  select(ss, Mandarin, `Mandarin (%)`, Cantonese, `Cantonese (%)`) %>%
  mutate(chinese_a = replace_na(Mandarin, 0) + replace_na(Cantonese, 0)) %>%
  mutate(chinese_p = replace_na(`Mandarin (%)`, 0)+replace_na(`Cantonese (%)`, 0)) %>%
  mutate(all_p = round(chinese_a*100/chinese_p, 0)) %>%
  filter(chinese_p >= 5.2) %>%
  select(ss, chinese_p)

# Tenure by suburb
tenure_2016 = read_csv("clustering_fy/Data/census_data/2016_Tenure.csv") %>%
  select(c(1,7:9))
colnames(tenure_2016) = c("ss", "owner", "mortgage", "rent")
tenure_2016 = tenure_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Rent by suburb
rent_2016 = read_csv("clustering_fy/Data/census_data/2016_Rent_weekly_payments.csv") %>%
  select(c(1, 2, 7))
colnames(rent_2016) = c("ss", "rental", "rent_distress")
rent_2016 = rent_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Dwelling structure by suburb
dw_2016 = read_csv("clustering_fy/Data/census_data/2016_Occupied_private_dwellings.csv") %>%
  select(c(1, 6:8))
colnames(dw_2016) = c("ss", "house", "semi", "unit")
dw_2016 = dw_2016 %>%
  mutate(ss = str_trim(str_remove(ss, b_pattern)))

# Suburb data together
all_table = income_2016 %>%
  inner_join(income2_2016, by = 'ss') %>%
  inner_join(language_2016, by = "ss") %>%
  inner_join(tenure_2016, by = "ss") %>%
  inner_join(rent_2016, by = "ss") %>%
  inner_join(mortgage_2016, by = "ss") %>%
  inner_join(dw_2016, by = "ss") 

all_table1 = all_table %>%
  select(-ss) %>%
  as.data.frame()

rownames(all_table1) = all_table$ss

pc = principal(scale(all_table1), nfactors = 3, rotate='varimax')
pc
plot(pc$values[1:4], type="b", main="Scree Plot")

k = kmeans(all_table1, 6)

all_table2 = cbind(all_table1, as.factor(k$cluster)) %>%
  mutate(ss = rownames(all_table1))

ggplot(as.data.frame(pc$scores), aes(x=RC1, y=RC2, color=as.factor(all_table2$`as.factor(k$cluster)`))) + 
  geom_point(size = 0.01) + theme_bw() + 
  # geom_text(aes(label=rownames(pc$scores)),hjust=0, vjust=0, size = 3) +
  ggtitle('First 2 prinipal components clustering w/ kmeans') + 
  xlab("Principal Component 1") + ylab("Principal Component 2")

