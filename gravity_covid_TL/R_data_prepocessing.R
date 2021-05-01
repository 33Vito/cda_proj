source("R_functions.R")

# --------------import cluster from FY -----------------------------

census_feature_by_POA <- read_csv("data/census_feature_by_POA.csv")

census_clusters <- bind_cols(
  census_feature_by_POA[,-1] %>% helper_pc_convert() %>% 
    helper_clustering(k=5) %>% pull(cluster) %>% as.factor(), 
  census_feature_by_POA[,-1] %>% helper_pc_convert() %>% 
    helper_h_clustering(k=5) %>% pull(cluster) %>% as.factor()
) %>% 
  as_tibble() %>% 
  `colnames<-`(c("kcluster", "hcluster")) %>% 
  bind_cols(census_feature_by_POA[,1]) %>% 
  mutate(POA_NAME16 = as.character(POA_CODE_2016)) %>% 
  select(POA_NAME16, kcluster, hcluster)

# -------------SYD confirmed cases by postcode----------------
# confirmed_cases <- read_csv("data/confirmed_cases_table1_location.csv")
confirmed_cases <- read_csv(
  "data/confirmed_cases_table4_location_likely_source.csv") %>% 
  filter(str_detect(likely_source_of_infection, "Locally")) %>% 
  select(-likely_source_of_infection)

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

SYD_trains <- read_csv("./data/SYD_geocodes/Public transports/Trains.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_ferries <- read_csv("./data/SYD_geocodes/Public transports/Ferries.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_lightrails <- read_csv("./data/SYD_geocodes/Public transports/LightRails.csv") %>% 
  filter(lon >= 150, lon <= 152)

SYD_metro <- read_csv("./data/SYD_geocodes/Public transports/Metro.csv") %>% 
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
  map_dfc(replace_na, replace=0) %>% 
  arrange(POA_NAME16)

n_poi_by_POA <- n_poi_by_POA %>% 
  mutate(across(starts_with("n_"), function(x) (x - min(x))/(max(x)-min(x)), 
                .names = "normalised_{col}")) %>% 
  mutate(across(starts_with("n_"), function(x) (x - mean(x))/sd(x), 
                .names = "scaled_{col}"))

# write_csv(n_poi_by_POA, "./data/calculated_measures/count of amenities by postcode SYD.csv")

# -----------------Radial distance matrix----------------------------------

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
  filter(source != target) %>% 
  arrange(source, target)

# -----------------Travel distance matrix----------------------------------

library(ggmap)
register_google(key = "AIzaSyALvIJNI54O02X5MxjtTHefEu8kyne7Wg8", write = TRUE)
SYD_POA_centroids <- getSpPPolygonsLabptSlots(SYD_POA) %>%
  as.data.frame() %>%
  rename(lon=V1, lat=V2) %>%
  mutate(POA_NAME16 = SYD_POA$POA_NAME16)

##-------------deprecated due to poor error handling------------------------
# SYD_POA_mapdist_2026 <- mapdist(
#   rep(paste0("Sydney postcode ", 
#              SYD_POA_centroids$POA_NAME16[210:215], 
#              ", NSW, Australia"), 
#       each=1),
#   paste0("Sydney postcode ", 
#          # rep(c("2026", "2145", "2107"), nrow(SYD_POA_centroids)), 
#          "2026",
#          ", NSW, Australia")
#   )
  # SYD_POA_centroids[
  #   match(SYD_POA_dist$source, SYD_POA_centroids$POA_NAME16),1:2] %>% 
  #   mutate(coord = paste(lon, lat, sep=",")) %>% pull(coord) %>% .[1:2], 
  # SYD_POA_centroids[
  #   match(SYD_POA_dist$target, SYD_POA_centroids$POA_NAME16),1:2] %>% 
  #   mutate(coord = paste(lon, lat, sep=",")) %>% pull(coord) %>% .[1:2]
  # ) %>% 
  # mutate(from_POA_NAME16 = SYD_POA_centroids$POA_NAME16[
  #   match(SYD_POA_dist$source, SYD_POA_centroids$POA_NAME16)], 
  #        to_POA_NAME16 = SYD_POA_centroids$POA_NAME16[
  #   match(SYD_POA_dist$target, SYD_POA_centroids$POA_NAME16)]
  # )
  # mutate(rep(SYD_POA_centroids$POA_NAME16, each=3))

##-----commented out to load from csv (otherwise each api call cost money--------

# safe_mapdist <- safely(function(from, to) mapdist(from, to, mode = "transit"), otherwise = NA)
# 
# SYD_POA_mapdist_2026 <- list()
# for (i in 1:length(SYD_POA_centroids$POA_NAME16)) {
#   SYD_POA_mapdist_2026[[i]] <- safe_mapdist(
#     paste0("Sydney postcode ", SYD_POA_centroids$POA_NAME16[i], ", NSW, Australia"),
#     "Sydney postcode 2026, NSW, Australia"
#   )
# }
# SYD_POA_mapdist_2145 <- list()
# for (i in 1:length(SYD_POA_centroids$POA_NAME16)) {
#   SYD_POA_mapdist_2145[[i]] <- safe_mapdist(
#     paste0("Sydney postcode ", SYD_POA_centroids$POA_NAME16[i], ", NSW, Australia"),
#     "Sydney postcode 2145, NSW, Australia"
#   )
# }
# SYD_POA_mapdist_2107 <- list()
# for (i in 1:length(SYD_POA_centroids$POA_NAME16)) {
#   SYD_POA_mapdist_2107[[i]] <- safe_mapdist(
#     paste0("Sydney postcode ", SYD_POA_centroids$POA_NAME16[i], ", NSW, Australia"),
#     "Sydney postcode 2107, NSW, Australia"
#   )
# }
# 
# SYD_POA_mapdist <- bind_rows(
#   map_dfr(SYD_POA_mapdist_2026, function(x) {if (!is.na(x$result)) x$result}),
#   map_dfr(SYD_POA_mapdist_2145, function(x) {if (!is.na(x$result)) x$result}),
#   map_dfr(SYD_POA_mapdist_2107, function(x) {if (!is.na(x$result)) x$result})
# ) %>%
#   mutate(source = str_replace_all(from, "[Sydney postcode | , NSW, Australia]",
#                                 "") %>% str_trim() %>% as.character(),
#          target = str_replace_all(to, "[Sydney postcode | , NSW, Australia]",
#                               "") %>% str_trim() %>% as.character()
#          ) %>%
#   rename(dist=minutes) %>%
#   select(source, target, dist)
# 
# # write_csv(SYD_POA_mapdist,
# #           "./data/calculated_measures/full google map distence from 2026 2145 2107 by POA.csv")
# write_csv(SYD_POA_mapdist,
#           "./data/calculated_measures/google map distence minutes from 2026 2145 2107 by POA transit.csv")
SYD_POA_mapdist <- read_csv(
  "./data/calculated_measures/google map distence minutes from 2026 2145 2107 by POA.csv", 
  col_types = list(col_character(), col_character(), col_double())
)

# -----------------Gravity calculation--------------------------------
gravity_to_cluster_by_POA <- bind_rows(
list(target_key = c("2026", "2145", "2107"), 
     mass_var = c("n_hospitals", "n_schools", "n_supermarkets", 
                  "n_shoppingCentres", "n_publicTransports")) %>% 
  cross_df() %>% 
  mutate(gravity_df = map2(target_key, mass_var, 
                           ~calc_gravity_TL(n_poi_by_POA, .x, .y))) %>% 
  unnest(gravity_df) %>% 
  mutate(dist_spec = "radial_distance") %>% 
  relocate(mass_var, dist_spec), 
list(target_key = c("2026", "2145", "2107"), 
     mass_var = c("n_hospitals", "n_schools", "n_supermarkets", 
                  "n_shoppingCentres", "n_publicTransports")) %>% 
  cross_df() %>% 
  mutate(gravity_df = map2(target_key, mass_var, 
                           ~calc_gravity_TL(n_poi_by_POA, .x, .y, 
                                            dist_matrix = SYD_POA_mapdist %>% 
                                              mutate(dist = dist/1000)))) %>% 
  unnest(gravity_df) %>% 
  mutate(dist_spec = "travel_time") %>% 
  relocate(mass_var, dist_spec)
)

# write_csv(gravity_to_cluster_by_POA, 
#           "./data/calculated_measures/calculated gravity from 2026 2145 2107 by POA.csv")

# ----------------Pairwise gravity based on POI----------------------
n_poi_by_POA_mass <- n_poi_by_POA %>% 
  filter(POA_NAME16 != "Unknown") %>% 
  select(contains("normalised"))

SYD_POA_gravity <-
  (
  as.matrix(n_poi_by_POA_mass) %*% t(n_poi_by_POA_mass) # dot product of mass
  /
  as.matrix(SYD_POA_dist %>% 
              filter(source %in% n_poi_by_POA$POA_NAME16, 
                     target %in% n_poi_by_POA$POA_NAME16) %>% 
              spread(target, dist, fill=0) %>% 
              select(-source)) # distance matrix
  ) %>% as_tibble() %>% 
    mutate(source = head(n_poi_by_POA$POA_NAME16,-1)) %>% 
  gather(target, gravity, -source) %>% 
  filter(source != target)

# ----------------Pairwise gravity based on POI & census data----------------------
n_poi_by_POA_census_mass <- n_poi_by_POA %>% 
  filter(POA_NAME16 != "Unknown") %>% 
  select(POA_NAME16, contains("normalised")) %>% 
  left_join(
    census_feature_by_POA %>% 
      filter(!is.na(POA_CODE_2016)) %>% 
      mutate(POA_CODE_2016 = as.character(POA_CODE_2016)) %>% 
      mutate(across(-POA_CODE_2016, function(x) (x-min(x))/(max(x)-min(x))))
  , by = c("POA_NAME16"="POA_CODE_2016")) %>% 
  select(-POA_NAME16)

SYD_POA_gravity_census <-
  (
    as.matrix(n_poi_by_POA_census_mass) %*% t(n_poi_by_POA_census_mass) # dot product of mass
    /
      as.matrix(SYD_POA_dist %>% 
                  filter(source %in% n_poi_by_POA$POA_NAME16, 
                         target %in% n_poi_by_POA$POA_NAME16) %>% 
                  spread(target, dist, fill=0) %>% 
                  select(-source)) # distance matrix
  ) %>% as_tibble() %>% 
  mutate(source = head(n_poi_by_POA$POA_NAME16,-1)) %>% 
  gather(target, gravity, -source) %>% 
  filter(source != target) %>% 
  mutate(gravity = replace_na(gravity, 0))

# ----------------gravity added as feature in clustering--------------
combined_feature_by_POA <- SYD_POA_gravity %>% 
  group_by(source) %>% 
  summarise(gravity = sum(gravity)) %>% 
  mutate(POA_CODE_2016 = as.numeric(source)) %>% 
  right_join(census_feature_by_POA, by="POA_CODE_2016") %>% 
  filter(!is.na(gravity)) %>% 
  select(-source) %>% 
  relocate(POA_CODE_2016)

combined_clusters <- bind_cols(
  combined_feature_by_POA[,-1] %>% helper_pc_convert() %>% 
    helper_clustering(k=5) %>% pull(cluster) %>% as.factor(), 
  combined_feature_by_POA[,-1] %>% helper_pc_convert() %>% 
    helper_h_clustering(k=5) %>% pull(cluster) %>% as.factor()
) %>% 
  as_tibble() %>% 
  `colnames<-`(c("kcluster", "hcluster")) %>% 
  bind_cols(combined_feature_by_POA[,1]) %>% 
  mutate(POA_NAME16 = as.character(POA_CODE_2016)) %>% 
  select(POA_NAME16, kcluster, hcluster)

# ----------------Cluster based on gravity only------------------
gravity_clusters <- bind_cols(
  combined_feature_by_POA[,2] %>% 
    helper_clustering(k=5) %>% pull(cluster) %>% as.factor(), 
  combined_feature_by_POA[,2] %>% 
    helper_h_clustering(k=5) %>% pull(cluster) %>% as.factor()
) %>% 
  as_tibble() %>% 
  `colnames<-`(c("kcluster", "hcluster")) %>% 
  bind_cols(combined_feature_by_POA[,1]) %>% 
  mutate(POA_NAME16 = as.character(POA_CODE_2016)) %>% 
  select(POA_NAME16, kcluster, hcluster)

# ----------------mapping between POA and SSC-------------------------
## SSC is MORE granular than POA, so one POA will contain multiple SSC
## Used by `` function in `R_functions.R`

POA_2016_SYD <- read_csv("./data/POA_2016_AUST.csv") %>% 
  filter(POA_NAME_2016 %in% SYD_POA$POA_NAME16)
SSC_2016_SYD <- read_csv("./data/SSC_2016_AUST.csv") %>% 
  filter(MB_CODE_2016 %in% POA_2016_SYD$MB_CODE_2016)

POA_to_SSC_mapping <- POA_2016_SYD %>% 
  left_join(SSC_2016_SYD, by="MB_CODE_2016") %>% 
  select(MB_CODE_2016, POA_CODE_2016 , POA_NAME_2016, 
         SSC_CODE_2016, SSC_NAME_2016) %>% 
  count(POA_CODE_2016 , POA_NAME_2016, 
        SSC_CODE_2016, SSC_NAME_2016, name = "n_MB_CODE_2016") %>% 
  group_by(SSC_NAME_2016) %>% 
  mutate(share_MB_CODE_2016 = n_MB_CODE_2016/sum(n_MB_CODE_2016)) %>% 
  slice_max(share_MB_CODE_2016, n=1)
# write_csv(POA_to_SSC_mapping,
#           "./data/calculated_measures/POA to SSC mapping.csv")

# ---------------Adjacent postcode of each POA (shared boundries)--------------------

library(rgeos)
SYD_POA_link <- gTouches(SYD_POA, byid = T) %>% 
  as_tibble() %>% 
  `colnames<-`(SYD_POA$POA_NAME16) %>% 
  mutate(source = SYD_POA$POA_NAME16) %>% 
  gather(target, link, -source) %>% 
  filter(link)

SYD_POA_adjacency <- SYD_POA_link[,1:2] %>% 
  rename(postcode = source) %>% 
  nest(adjacent_postcode = target) %>% 
  mutate(adjacent_postcode = map(adjacent_postcode, ~.x[[1]])) %>% 
  mutate(n_adjacent_postcode = map_dbl(adjacent_postcode, length))

# ----------------Base line cluster and confirmed cases ----------------------

baseline_cases <- confirmed_cases %>%
  mutate(postcode = as.character(postcode)) %>% 
  # --------------join adjacent POS------------------
  left_join(SYD_POA_adjacency, by="postcode") %>% 
  select(notification_date, postcode, adjacent_postcode, 
         lga_name19, lhd_2010_name) %>% 
  # --------------------------------------------
  arrange(notification_date, postcode) %>% 
  mutate(case = 1:nrow(.)) %>% 
  # ----add indicator of cases avoided by adjacent POA cases within 14 days----
  mutate(avoided_by_adjacent_lockdown = map2_lgl(
    notification_date, adjacent_postcode, function(x, y) {
      confirmed_cases %>% 
        filter(notification_date < x, 
               notification_date >= x - 14) %>% 
        filter(postcode %in% y) %>% 
        nrow(.) -> n_adjacent_case_within14days
      n_adjacent_case_within14days > 0
    }
  )) %>% 
  # ---add indicator of cases avoided by LGA cases within 14 days------
mutate(avoided_by_lga_lockdown = map2_lgl(
  notification_date, lga_name19, function(x, y) {
    confirmed_cases %>% 
      filter(notification_date < x, 
             notification_date >= x - 14) %>% 
      filter(lga_name19 == y) %>% 
      nrow(.) -> n_lga_case_within14days
    n_lga_case_within14days > 0
  }
)) %>% 
# ---add indicator of cases avoided by LHD cases within 14 days------
mutate(avoided_by_lhd_lockdown = map2_lgl(
  notification_date, lhd_2010_name, function(x, y) {
    confirmed_cases %>% 
      filter(notification_date < x, 
             notification_date >= x - 14) %>% 
      filter(lhd_2010_name == y) %>% 
      nrow(.) -> n_lhd_case_within14days
    n_lhd_case_within14days > 0
  }
))

# ----function to produce avoid_indc by left_join------------------------
# save this function here to allow context to understand what it does-------
check_avoided_cases <- function(case_df = baseline_cases, 
                                cluster_df = census_clusters, 
                                cluster_var = "kcluster") {
  
  predicted_cases <- case_df %>% 
    left_join(cluster_df, by=c("postcode"="POA_NAME16")) %>% 
    filter(!is.na(.data[[cluster_var]])) %>% 
    left_join(.,., by=cluster_var) %>% 
    filter(notification_date.y - notification_date.x <= 14,
           notification_date.y - notification_date.x > 0) %>%
    distinct(case.y) %>% pull(case.y)
  
  return(case_df$case %in% predicted_cases)
    
  # browser()
  # confirmed_cases %>% 
  #   mutate(POA_NAME16 = as.character(postcode)) %>% 
  #   left_join(cluster_df, by="POA_NAME16") %>% 
  #   filter(notification_date < DATE.x, 
  #          notification_date >= DATE.x - 14) %>% 
  #   filter({{ cluster_var }} == CLUSTER.y) %>% 
  #   nrow(.) -> n_CLUSTER_case_within14days
  # n_CLUSTER_case_within14days > 0
}

# check_avoided_cases() %>% mean
# check_avoided_cases(case_df = baseline_cases %>% 
#                       select(postcode, notification_date, case), 
#                     cluster_df = confirmed_cases %>% 
#                       mutate(POA_NAME16 = as.character(postcode)) %>% 
#                       distinct(POA_NAME16, lga_name19), 
#                     cluster_var = "lga_name19") %>% 
#   mean()

# covid_cluster <- baseline_cases %>%
#   mutate(POA_NAME16 = as.character(postcode)) %>%
#   left_join(census_clusters, by="POA_NAME16") %>% 
#   filter(!is.na(census_kcluster)) %>%
#   select(notification_date, POA_NAME16, case, census_kcluster)
#   
# covid_cluster %>% 
#   left_join(covid_cluster, by = "census_kcluster") %>% 
#   filter(notification_date.y - notification_date.x <= 14,
#          notification_date.y - notification_date.x > 0) %>%
#   distinct(case.y) %>% 
#   summarise(nrow(.)/nrow(confirmed_cases))
  
  
  
  
  
  













