source("R_functions.R")

# -------------SYD confirmed cases by postcode----------------
confirmed_cases <- read_csv("data/confirmed_cases_table1_location.csv")

# -------------SYD shp file data------------------------------
SYD_POA <- readRDS("data/SYD_POA.rds")
SYD_LGA <- readRDS("data/SYD_LGA.rds")
SYD_SA2 <- readRDS("data/SYD_SA2.rds")

#--------------AU covid timeline data ---------

AU_timeline <- readxl::read_excel("./data/AU_covid19_timeline.xlsx", 
                                  sheet = 1) %>% 
  mutate(date = as.Date(date))

#--------------Geocodes for places of interest in SYD---------
SYD_hospitals <- readxl::read_excel(
  "./data/SYD_geocodes/Hospitals/Latest data (points).xlsx", skip = 2)
SYD_hospitals$bed.approx <- fct_recode(SYD_hospitals$Beds, 
                                   "25" = "<50", 
                                   "75" = "20-99", 
                                   "100" = "100-199", 
                                   "350" = "200-500", 
                                   "750" = ">500") %>% 
  as.character() %>% 
  as.numeric()

SYD_hospitals$bed.approx[is.na(SYD_hospitals$bed.approx)] <- 25
# gp <- read_csv("./data/SYD_geocodes/Hospitals/GP.csv")

SYD_pschools <- readxl::read_excel(
  "./data/SYD_geocodes/Primary Schools/Primary Schools.xlsx") %>% 
  filter(!duplicated(poiname)) %>% 
  filter(Long >= 150, Long <= 152)

SYD_sschools <- readxl::read_excel(
  "./data/SYD_geocodes/Secondary Schools/Secondary Schools.xlsx") %>% 
  filter(!duplicated(poiname)) %>% 
  filter(Long >= 150, Long <= 152)

SYD_supermarkets <- read_csv(
  "./data/SYD_geocodes/Shopping centres/Supermarket & grocery stores.csv") %>% 
  filter(!duplicated(address)) %>% 
  filter(lon >= 150, lon <= 152) %>% 
  na.omit()

SYD_shops <- read_csv(
  "./data/SYD_geocodes/Shopping centres/Shopping centres.csv") %>% 
  filter(lon >= 150, lon <= 152) %>% 
  na.omit()

SYD_trains <- read_csv("./data/SYD_geocodes/Trains/Trains.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_ferries <- read_csv("./data/SYD_geocodes/Trains/Ferries.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_lightrails <- read_csv("./data/SYD_geocodes/Trains/LightRails.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_metro <- read_csv("./data/SYD_geocodes/Trains/Metro.csv") %>% 
  filter(lon >= 150, lon <= 152)

# ------------------Places of interest count ----------------------------------

point_over_polygon <- point_over_polygon <- function(x, y, NAME) {
  over(
    SpatialPoints(x, #SYD_shops[,3:4],
                  proj4string = CRS("+proj=longlat +ellps=GRS80 +no_defs")),
    y, #SYD_POA
    ) %>% count(POA_NAME16, name=NAME)
}

n_poi_by_POA <- list(
  point_over_polygon(SYD_hospitals[,12:11], SYD_POA, "n_hospitals"), 
  point_over_polygon(rbind(SYD_pschools[,2:1], 
                           SYD_sschools[,2:1]), SYD_POA, "n_schools"), 
  point_over_polygon(SYD_supermarkets[,2:3], SYD_POA, "n_supermarkets"), 
  point_over_polygon(SYD_shops[,3:4], SYD_POA, "n_shoppingCentres"), 
  point_over_polygon(rbind(SYD_trains[,10:11], 
                           SYD_ferries[,3:4], 
                           SYD_lightrails[,3:4], 
                           SYD_metro[,3:4]), SYD_POA, "n_publicTransports")
) %>% reduce(full_join, by="POA_NAME16") %>% 
  mutate(POA_NAME16 = ifelse(is.na(POA_NAME16), "Unknown", POA_NAME16)) %>% 
  map_dfc(replace_na, replace=0)

n_poi_by_POA <- n_poi_by_POA %>% 
  mutate(across(starts_with("n_"), function(x) (x - min(x))/(max(x)-min(x)), 
                .names = "normalised_{col}")) %>% 
  mutate(across(starts_with("n_"), function(x) (x - mean(x))/sd(x), 
                .names = "scaled_{col}"))

write_csv(n_poi_by_POA, "./data/count of amenities by postcode SYD.csv")

# -----------------Distance matrix----------------------------------

spDistsN1(as.matrix(SYD_shops[,3:4]), as.matrix(SYD_shops[1,3:4]))

SYD_POA_dist <- map_dfr(SYD_POA$POA_NAME16, 
                        function(x){
                          data.frame(
                            source_POA_NAME16 = SYD_POA$POA_NAME16, 
                            target_POA_NAME16 = x,
                            dist = spDistsN1(
                              pts = getSpPPolygonsLabptSlots(SYD_POA), 
                              pt = getSpPPolygonsLabptSlots(SYD_POA)[
                                match(x, SYD_POA$POA_NAME16),])
                            ) %>% 
                            as_tibble() %>% 
                            # mutate(source = pmin(source_POA_NAME16, 
                            #                     target_POA_NAME16), 
                            #        target = pmax(source_POA_NAME16, 
                            #                     target_POA_NAME16))
                            rename(source = source_POA_NAME16, 
                                   target = target_POA_NAME16)
                          }) %>% 
  # distinct(source, target, dist) %>% 
  filter(source != target)




















