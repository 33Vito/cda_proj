


library(tidyverse)
library(forcats)
library(stringr)
library(ggmap)

setwd("C:/Users/yanjliu/Google Drive/DAE/Imagine SYD/Travel time carnival/GIS files/Shopping centres/")

# Wikipedia list

dd <- readxl::read_excel("Shopping centres.xlsx")

loc <- geocode(paste0(dd$Name, "+",dd$Region))

write.csv(cbind(dd, loc), "Shopping centres.csv", row.names = F)


# yellowpages.com.au list

library(rvest)
library(httr)

output <- list()
for (i in 1:29) {
  # url = paste0("https://www.yellowpages.com.au/search/listings?clue=Supermarkets+%26+Grocery+Stores&locationClue=sydney&pageNumber="
  #              ,i,
  #              "&referredBy=www.yellowpages.com.au&&eventType=pagination")
  # download.file(url, destfile = paste0("page",i,".html"), quiet=TRUE)
  
  # name <- read_html(paste0("page",i,".html")) %>% 
  #   html_nodes(".right .listing-name") %>% 
  #   html_text()
  
  address <- read_html(paste0("page",i,".html")) %>% 
    html_nodes(".mappable-address-with-poi") %>% 
    html_text()
  
  output[[i]] <- address
}

result <- tibble(address = do.call(c,output))
loc_sg <- geocode(result$address)

write.csv(cbind(result, loc_sg), "Supermarket & grocery stores.csv", row.names = F)
