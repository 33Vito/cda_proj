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


