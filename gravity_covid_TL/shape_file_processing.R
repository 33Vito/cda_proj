library(tidyverse)
library(rgdal)
library(sp)

# ABS SA2 2016-------------------------------------------------------
AU_SA2 <- readOGR( 
  dsn= "data/1270055001_sa2_2016_aust_shape" , 
  layer="SA2_2016_AUST",
  verbose=FALSE)
AU_SA2@proj4string <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
saveRDS(AU_SA2, file = "data/AU_SA2.rds")

SYD_SA2 <- AU_SA2[AU_SA2$GCC_CODE16 == "1GSYD",]
SYD_SA2 <- SYD_SA2[!(SYD_SA2$SA3_NAME16 %in% c("Wyong", 
                                               "Gosford", 
                                               "Hawkesbury",
                                               "Blue Mountains")),]
SYD_SA2 <- SYD_SA2[!(SYD_SA2$SA2_NAME16 %in% c("Blue Mountains - South", 
                                               "Bargo", 
                                               "Picton - Tahmoor - Buxton", 
                                               "Douglas Park - Appin", 
                                               "The Oaks - Oakdale", 
                                               "Bilpin - Colo - St Albans", 
                                               "Calga - Kulnura", 
                                               "Jilliby - Yarramalong", 
                                               "Blue Mountains - North", 
                                               "Blackheath - Megalong Valley")),]

# plot(SYD_SA2, main = "Sydney Metro SA2")
saveRDS(SYD_SA2, file = "data/SYD_SA2.rds")

# ABS POA 2016-------------------------------------------------------

# ABS Fact sheets: https://www.abs.gov.au/websitedbs/censushome.nsf/home/factsheetspoa?opendocument&navpos=450
# SYD full postcode: https://postcodez.com.au/postcodes/nsw/sydney
# SYD metro postcode: http://www.impactlists.com.au/ImpactLists/media/list-tools/Useful-Postcode-Ranges.pdf
AU_POA <- readOGR( 
  dsn= "data/1270055003_poa_2016_aust_shape" , 
  layer="POA_2016_AUST",
  verbose=FALSE)
AU_POA@proj4string <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
saveRDS(AU_POA, file = "data/AU_POA.rds")

# SYD_postcode_range <- read_csv("data/SYD_postcode_range.csv")
# SYD_postcode_range_metro <- SYD_postcode_range %>%
#   filter(
#     `Sydney Areas` %in% c(
#       "Eastern Suburbs",
#       # "Inner West",
#       # "Hills",
#       "Lower North Shore",
#       "Parramatta",
#       "Sydney City"
#       # "Upper North Shore"
#       # "Western Sydney"
#       # "Sutherland"
#     )
#   )

# list_SYD_postcode <- map2(SYD_postcode_range_metro$min_postcode, 
#                           SYD_postcode_range_metro$max_postcode, 
#                           ~seq(.x, .y, 1)) %>% reduce(c)

list_SYD_postcode <- c(seq(1000,2249,1), seq(2760, 2770), 
                       2747:2750,2759
                       # 2570, 2567, 2557, 2558, 2566
                       )

SYD_POA <- AU_POA[AU_POA$POA_CODE16 %in% as.character(list_SYD_postcode),]
# plot(SYD_POA, main = "Sydney Metro POA")
saveRDS(SYD_POA, file = "data/SYD_POA.rds")

# ABS LGA 2019-------------------------------------------------------

AU_LGA <- readOGR( 
  dsn= "data/1270055003_lga_2019_aust_shp" , 
  layer="LGA_2019_AUST",
  verbose=FALSE)
AU_LGA@proj4string <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
saveRDS(AU_SA2, file = "data/AU_LGA.rds")

# NSW_LGA <- AU_LGA[AU_LGA$STE_NAME16 == "New South Wales",]
# saveRDS(NSW_LGA, file = "data/NSW_LGA.rds")

# LGA in metropolitan SYD 
# source: https://en.wikipedia.org/wiki/Local_government_areas_of_New_South_Wales 
list_SYD_LGA <- c(
  "Bayside"
  ,"Blacktown"
  ,"Burwood"
  ,"Camden"
  ,"Campbelltown"
  ,"Canada Bay"
  ,"Canterbury-Bankstown"
  ,"Cumberland"
  ,"Fairfield"
  ,"Georges River"
  ,"Hornsby"
  ,"Hunters Hill"
  ,"Inner West"
  ,"Ku-ring-gai"
  ,"Lane Cove"
  ,"Liverpool"
  ,"Mosman"
  ,"North Sydney"
  ,"Northern Beaches"
  ,"Parramatta"
  ,"Penrith"
  ,"Randwick"
  ,"Ryde"
  ,"Strathfield"
  ,"Sutherland Shire"
  ,"Sydney"
  ,"The Hills Shire"
  ,"Waverley"
  ,"Willoughby"
  ,"Woollahra"
)

SYD_LGA <- AU_LGA[
  tolower(str_trim(str_extract(AU_LGA$LGA_NAME19, 
                               "[^\\()]+"))) %in% tolower(list_SYD_LGA) & AU_LGA$STE_NAME16 == "New South Wales",]
# plot(SYD_LGA, main = "Sydney Metro LGA")
saveRDS(SYD_LGA, file = "data/SYD_LGA.rds")

# Summary
# par(mfrow=c(1,3))
# plot(SYD_SA2, main = "Sydney Metro SA2")
# plot(SYD_POA, main = "Sydney Metro POA")
# plot(SYD_LGA, main = "Sydney Metro LGA")
# par(mfrow=c(1,1))

bind_rows(
  fortify(SYD_SA2) %>% mutate(Boundaries = "SA2"), 
  fortify(SYD_POA) %>% mutate(Boundaries = "POA"), 
  fortify(SYD_LGA) %>% mutate(Boundaries = "LGA")
) %>% 
  ggplot() +
  geom_path(aes(x = long, y = lat, group = group),
            color = 'grey33', fill = 'white', size = .2) + 
  facet_wrap(~Boundaries, labeller = "label_both") + 
  ggtitle("Sydney Metro Geography") + 
  theme_void(base_size = 15) + 
  theme( plot.title = element_text(family = "sans", size = 18, hjust = 0.5, 
                                   margin=margin(0,0,15,0)))



