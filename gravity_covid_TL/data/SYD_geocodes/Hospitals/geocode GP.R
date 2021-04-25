


library(tidyverse)
library(forcats)
library(stringr)
library(ggmap)

setwd("C:/Users/tony/Google Drive/DAE/Imagine SYD/Travel time carnival/GIS files/Hospitals/")

# yellowpages.com.au list

library(rvest)
library(httr)

output <- list()
for (i in 1:29) {
  url = paste0("https://www.yellowpages.com.au/search/listings?clue=general+practitioners&locationClue=Sydney%2C+NSW+2000&pageNumber=", 
               i,
               "&referredBy=www.yellowpages.com.au&&eventType=pagination")
  download.file(url, destfile = paste0("page",i,".html"), quiet=TRUE)
  
  # name <- read_html(paste0("page",i,".html")) %>% 
  #   html_nodes(".right .listing-name") %>% 
  #   html_text()
  
  address <- read_html(paste0("page",i,".html")) %>% 
    html_nodes(".mappable-address-with-poi") %>% 
    html_text()
  
  output[[i]] <- address
}

result <- tibble(address = do.call(c,output)) %>% unique()
loc_sg <- geocode(result$address)

write.csv(cbind(result, loc_sg), "GP.csv", row.names = F)
