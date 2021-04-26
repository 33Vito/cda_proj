
library(tidyverse)
library(rvest)
library(httr)
library(ggmap)

setwd("C:/Users/yanjliu/Google Drive/DAE/Imagine SYD/Travel time carnival/GIS files/Trains")

url = paste0("https://en.wikipedia.org/wiki/List_of_Sydney_Trains_railway_stations")

download.file(url, destfile = "SYD trains list wiki.html", quiet=TRUE)

station <- read_html("SYD trains list wiki.html") %>% 
  html_nodes("table") %>%
  .[[1]] %>% 
  html_table(fill=T)

loc <- geocode(paste0(station$Name," station NSW"))

write.csv(cbind(station, loc), "Trains.csv", row.names = F)



# metro ------------------------------
metro_df <- read_csv("data/SYD_geocodes/Trains/Metro.csv")

register_google(key = "AIzaSyALvIJNI54O02X5MxjtTHefEu8kyne7Wg8", write = TRUE)
metro_geocodes <- geocode(paste0(metro_df$Name," Metro Station Sydney"))

write.csv(cbind(metro_df, metro_geocodes), 
          "data/SYD_geocodes/Trains/Metro.csv", row.names = F)














