
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




















