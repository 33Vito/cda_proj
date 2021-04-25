library(XML)
library(rvest)

url = 'https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/SSC13379?opendocument'

webpage = read_html(url)

tbls <- html_nodes(webpage, "table")

tbls_ls <- webpage %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

tbls_ls[[1]]
tbls_ls[[2]]
tbls_ls[[30]]
