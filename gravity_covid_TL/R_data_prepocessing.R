source("R_functions.R")

#-----------------------------Geocodes for places of interest in SYD------------------------------------------
SYD_hospitals <- readxl::read_excel("./data/SYD_geocodes/Hospitals/Latest data (points).xlsx", skip = 2)
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


SYD_pschools <- readxl::read_excel("./data/SYD_geocodes/Primary Schools/Primary Schools.xlsx") %>% 
  filter(!duplicated(poiname)) %>% 
  filter(Long >= 150, Long <= 152)

SYD_sschools <- readxl::read_excel("./data/SYD_geocodes/Secondary Schools/Secondary Schools.xlsx") %>% 
  filter(!duplicated(poiname)) %>% 
  filter(Long >= 150, Long <= 152)

SYD_shops <- read_csv("./data/SYD_geocodes/Shopping centres/Supermarket & grocery stores.csv") %>% 
  filter(!duplicated(address)) %>% 
  filter(lon >= 150, lon <= 152) %>% 
  na.omit()

SYD_trains <- read_csv("./data/SYD_geocodes/Trains/Trains.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_ferries <- read_csv("./data/SYD_geocodes/Trains/Ferries.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_lightrails <- read_csv("./data/SYD_geocodes/Trains/LightRails.csv") %>% 
  filter(lon >= 150, lon <= 152)
