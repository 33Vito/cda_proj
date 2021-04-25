library(dplyr)
library(readr)
library(stringr)
library(rebus)
setwd("C:/Users/zhyu2/Downloads/code/census")

b_pattern = escape_special("(") %R%
  one_or_more(negated_char_class(escape_special(")"))) %R%
  escape_special(")")

SSC_2016_AUST <- read_csv("ssc/SSC_2016_AUST.csv")

SSC_2016_AUST_CLEAN = SSC_2016_AUST %>%
  filter(STATE_NAME_2016 == 'New South Wales') %>%
  mutate(SSC_NAME_2016 = str_trim(str_remove(SSC_NAME_2016, b_pattern))) %>%
  select(SSC_CODE_2016, SSC_NAME_2016, STATE_NAME_2016) %>%
  distinct() %>%
  arrange(SSC_CODE_2016)

SSC_2011_AUST <- read_csv("ssc/SSC_2011_AUST.csv")

SSC_2011_AUST_1 = SSC_2011_AUST %>%
  filter(grepl(escape_special("("),SSC_NAME_2011) & grepl("NSW",SSC_NAME_2011)) %>%
  mutate(SSC_NAME_2011 = str_trim(str_remove(SSC_NAME_2011, b_pattern))) %>%
  semi_join(SSC_2016_AUST_CLEAN, by = c("SSC_NAME_2011"="SSC_NAME_2016")) %>%
  select(SSC_CODE_2011, SSC_NAME_2011) %>%
  distinct() 

SSC_2011_AUST_2 = SSC_2011_AUST %>%
  filter(!grepl(escape_special("("),SSC_NAME_2011)) %>%
  mutate(SSC_NAME_2011 = str_trim(str_remove(SSC_NAME_2011, b_pattern))) %>%
  semi_join(SSC_2016_AUST_CLEAN, by = c("SSC_NAME_2011"="SSC_NAME_2016")) %>%
  select(SSC_CODE_2011, SSC_NAME_2011) %>%
  distinct() 

SSC_2011_AUST_CLEAN = dplyr::union(SSC_2011_AUST_1, SSC_2011_AUST_2) %>%
  arrange(SSC_CODE_2011)

SSC_2006_AUST <- read_csv("ssc/CP2006RA_2006SSC.TXT")
SSC_2006_AUST_CLEAN = SSC_2006_AUST %>%
  mutate(SSC_2006_NAME = str_trim(str_remove(SSC_2006_NAME, b_pattern))) %>%
  filter(RA_2006_CODE == 10) %>%
  semi_join(SSC_2011_AUST_CLEAN, by = c("SSC_2006_NAME" = "SSC_NAME_2011")) %>%
  select(SSC_2006_CODE, SSC_2006_NAME) %>%
  distinct() %>%
  arrange(SSC_2006_CODE)

SSC_2001_AUST <- read_csv("ssc/AUS_ssc_s01.csv")
SSC_2001_AUST_CLEAN = SSC_2001_AUST %>%
  mutate(SSC_2001_NAME = str_trim(str_remove(Suburb_Name, b_pattern))) %>%
  filter(State_Code == 1) %>%
  semi_join(SSC_2011_AUST_CLEAN, by = c("SSC_2001_NAME" = "SSC_NAME_2011")) %>%
  mutate(Suburb_Code = as.numeric(Suburb_Code)) %>%
  select(SSC_2001_NAME, Suburb_Code) %>%
  distinct() %>%
  arrange(Suburb_Code)

SSC_final = SSC_2006_AUST_CLEAN %>%
  inner_join(SSC_2011_AUST_CLEAN, by = c("SSC_2006_NAME" = "SSC_NAME_2011")) %>%
  inner_join(SSC_2016_AUST_CLEAN, by = c("SSC_2006_NAME" = "SSC_NAME_2016")) %>%
  inner_join(SSC_2001_AUST_CLEAN, by = c("SSC_2006_NAME" = "SSC_2001_NAME")) %>%
  group_by(SSC_2006_NAME) %>%
  summarise(SSC_CODE_2001 = max(Suburb_Code),
            SSC_CODE_2006 = max(SSC_2006_CODE),
            SSC_CODE_2011 = max(SSC_CODE_2011),
            SSC_CODE_2016 = max(SSC_CODE_2016))

# write.csv(SSC_final, "SSC_final.csv", row.names = F)
