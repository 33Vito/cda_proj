source("utility.R")
library(sp)

#-----------------------------Function to plot choropleth map-----------------------------
plot_map_TL <- function(df, sdf, key_var, fill_var, title_text,  
                        show_count=FALSE, label_size=3, fill_col="darkred", 
                        map_lp=c(.93,.15), return_obj="combined") {
  # browser()
  df['id'] <- unique(fortify(sdf)$id)[match(df[[key_var]], 
                                            sdf[[key_var]])] %>% as.character()
  sdf_tidy <- fortify(sdf) %>% 
    left_join(df, by="id")
  sdf_tidy[[fill_var]] <- replace_na(sdf_tidy[[fill_var]], 0)
  
  gg_map <- ggplot(sdf_tidy) +
    geom_map(inherit.aes = FALSE, alpha=.85,
             aes_string(map_id = "id", fill= fill_var),
             map = sdf_tidy, col="grey83", size=NA) +
    expand_limits(x = sdf_tidy$long, y = sdf_tidy$lat) +
    # xlim(150.7,151.48) + ylim(-34.1,-33.5) +
    scale_fill_gradient(low="grey88", high=fill_col, na.value = NA) +
    # scale_fill_viridis_c(option = "D", direction = -1, breaks = pretty_breaks(5)) +
    scale_alpha_continuous(range = c(0,.3)) +
    labs(title = title_text) + 
    theme_void(base_size=12) +
    # theme(legend.position = "right")
    theme(legend.position = map_lp, 
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.key.size = unit(1,"line"))
  
  if (show_count) {
    sdf_centroids <- getSpPPolygonsLabptSlots(sdf) %>% as.data.frame()
    sdf_centroids[[key_var]] <- sdf[[key_var]]
    label_df <- left_join(df, sdf_centroids, by=key_var)
    gg_map <- gg_map + 
      geom_text(
        data = label_df, 
        aes_string(x="V1", y="V2", label=fill_var), 
        size=label_size, col="white")
  }
  
  gg_bar <- df %>% 
    # top_n(20) %>% # There is a bug in top_n()
    slice_max(order_by = !!as.name(fill_var), n=20) %>% 
    ggplot(aes_string(x=key_var, y=fill_var, fill=fill_var)) + 
    geom_col(width=.8) + 
    geom_text(aes_string(label=fill_var), 
              hjust=0, vjust=.3, size=3.3, col="black") + 
    scale_fill_gradient(low="grey88", high=fill_col, na.value = NA) +
    # scale_fill_viridis_c(option = "D", direction = -1, breaks = pretty_breaks(5)) +
    coord_flip() + 
    xlab("") + 
    ylim(c(0, 1.1*max(df[[fill_var]], na.rm=T))) + 
    labs(subtitle = paste0("Top 20 ", key_var)) + 
    ggl(base_size=12) + 
    theme(legend.position = "none")
  
  switch(return_obj, 
         "combined" = gg_map + gg_bar + plot_layout(widths = c(3, 1)), 
         "map" = gg_map, 
         "bar" = gg_bar)
}

# confirmed_cases %>%
#   filter(as.character(postcode) %in% SYD_POA$POA_NAME16) %>%
#   count(postcode, name="Total_cases") %>%
#   rename(POA_NAME16 = postcode) %>%
#   mutate(POA_NAME16 = fct_reorder(as.factor(POA_NAME16), Total_cases)) %>%
#   plot_map_TL(SYD_POA, "POA_NAME16", "Total_cases",
#               "Total Covid-19 cases by POA (SYD Metro)",
#               show_count = T, label_size = 2)

plot_map_factor_TL <- function(df, sdf, key_var, fill_var, factor_var, title_text,  
                        show_count=FALSE, label_size=3, fill_col="darkred", 
                        map_lp="none", return_obj="combined") {
  # browser()
  df['id'] <- unique(fortify(sdf)$id)[match(df[[key_var]], 
                                            sdf[[key_var]])] %>% as.character()
  sdf_tidy <- fortify(sdf) %>% 
    left_join(df, by="id")
  sdf_tidy[[fill_var]] <- replace_na(sdf_tidy[[fill_var]], 0)
  sdf_tidy[[factor_var]] <- fct_explicit_na(sdf_tidy[[factor_var]], "") 
  
  gg_map <- ggplot(sdf_tidy) +
    geom_map(inherit.aes = FALSE, alpha=.85,
             aes_string(map_id = "id", fill= factor_var),
             map = sdf_tidy, col="grey83", size=NA) +
    expand_limits(x = sdf_tidy$long, y = sdf_tidy$lat) +
    # xlim(150.7,151.48) + ylim(-34.1,-33.5) +
    scale_fill_brewer(palette = "Set2") +
    # scale_fill_manual(values = DC) +
    labs(title = title_text) + 
    theme_void(base_size=12) +
    # theme(legend.position = "right")
    theme(legend.position = map_lp)
  
  if (show_count) {
    sdf_centroids <- getSpPPolygonsLabptSlots(sdf) %>% as.data.frame()
    sdf_centroids[[key_var]] <- sdf[[key_var]]
    label_df <- left_join(df, sdf_centroids, by=key_var)
    gg_map <- gg_map + 
      geom_text(
        data = label_df, 
        aes_string(x="V1", y="V2", label=fill_var), 
        size=label_size, col="white")
  }
  
  gg_bar <- df %>% 
    # top_n(20) %>% # There is a bug in top_n()
    slice_max(order_by = !!as.name(fill_var), n=20) %>% 
    ggplot(aes_string(x=key_var, y=fill_var, fill=factor_var)) + 
    geom_col(width=.8) + 
    geom_text(aes_string(label=fill_var), 
              hjust=0, vjust=.3, size=3.3, col="black") + 
    scale_fill_brewer(palette = "Set2") +
    # scale_fill_manual(values = DC) +
    coord_flip() + 
    xlab("") + 
    ylim(c(0, 1.1*max(df[[fill_var]], na.rm=T))) + 
    labs(subtitle = paste0("Top 20 ", key_var)) + 
    ggl(base_size=12) + 
    theme(legend.position = "none")
  
  switch(return_obj, 
         "combined" = gg_map + gg_bar + plot_layout(widths = c(3, 1)), 
         "map" = gg_map, 
         "bar" = gg_bar)
}
# confirmed_cases %>% 
#   filter(as.character(postcode) %in% SYD_POA$POA_NAME16) %>% 
#   count(postcode, name="Total_cases") %>% 
#   rename(POA_NAME16 = postcode) %>% 
#   mutate(POA_NAME16 = fct_reorder(as.factor(POA_NAME16), Total_cases)) %>% 
#   mutate(cluster_origin = ifelse(POA_NAME16 %in% c("2026", "2145", "2107"), 
#                                  "cluster_origin", "other") %>% as.factor) %>% 
#   plot_map_factor_TL(SYD_POA, "POA_NAME16", "Total_cases", "POA_NAME16", 
#               "Total Covid-19 cases by POA (SYD Metro), by cluster", 
#               show_count = T, label_size = 2)

theme_map_facet_TL <- theme(legend.position = c(.85, 1.15), 
                            legend.justification = "top",
                            legend.direction = "horizontal", 
                            legend.text = element_text(angle=30, size=9), 
                            plot.title = element_text(margin=unit(c(1,1,10,1), "mm")), 
                            plot.margin=unit(c(5,1,1,1),"mm"))

#---------------------Function to calculate gravity -----------------------------

calc_gravity_TL <- function(input_df = n_poi_by_POA, 
                            TARGET_KEY, MASS_VAR, 
                            JOIN_KEY = "POA_NAME16", 
                            power = 2, 
                            dist_matrix = SYD_POA_dist) {
  # browser()
  out_df <- dist_matrix %>% 
    filter(source != target) %>% 
    filter(target == TARGET_KEY) %>% 
    left_join(input_df, by = c("source"=JOIN_KEY)) %>% 
    select(source, target, dist, MASS_VAR)
  
  names(out_df)[4] <- "mass_source"
  out_df$mass_source <- ifelse(out_df$mass_source == 0 | is.na(out_df$mass_source), 
                               0.1, out_df$mass_source)
  out_df$mass_target <- input_df[[MASS_VAR]][
    match(out_df$target, input_df[[JOIN_KEY]])]
  
  out_df$mass_target <- ifelse(out_df$mass_target == 0 | is.na(out_df$mass_target), 
                               0.1, out_df$mass_target)
  
  out_df$gravity <- round(out_df$mass_source * out_df$mass_target / out_df$dist^power)
  # out_df$gravity <- scale(out_df$gravity)
  # out_df$gravity <-  (out_df$gravity - min(out_df$gravity, na.rm=T))/(
  #   max(out_df$gravity, na.rm=T) - min(out_df$gravity, na.rm=T))
  
  return(out_df %>% add_row(source = TARGET_KEY, 
                           target = TARGET_KEY, 
                           gravity = max(out_df$gravity, na.rm=T)) %>% 
           mutate(gravity_rank = min_rank(gravity))
         )
}
# calc_gravity_TL(n_poi_by_POA, "2145", "n_hospitals")

plot_gravity_map_TL <- function(input_df, TARGET_KEY, MASS_VAR, POWER=2, 
                                dist_matrix = SYD_POA_dist) {
  input_df %>% 
    calc_gravity_TL(TARGET_KEY, MASS_VAR, power = POWER, 
                    dist_matrix = dist_matrix) %>% 
    filter(as.character(source) %in% SYD_POA$POA_NAME16) %>% 
    rename(POA_NAME16 = source) %>% 
    mutate(POA_NAME16 = fct_reorder(as.factor(POA_NAME16), gravity)) %>% 
    plot_map_TL(SYD_POA, "POA_NAME16", "gravity", return_obj = "map", 
                paste0("Gravity to postcode ",TARGET_KEY, " by\n", MASS_VAR)) + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5),
          plot.subtitle=element_text(hjust = 0.5))
}
# plot_gravity_map_TL(n_poi_by_POA, "2145", "n_hospitals")


#------------------Function to convert SSC features to POA-------------------
convert_SSC_to_POA <- function(SSC_df, JOIN_KEY="SSC_NAME_2016")  {
  SSC_df %>% 
    # data.frame POA_to_SSC_mapping created from `R_data_prepocessing.R`
    left_join(POA_to_SSC_mapping %>% 
                rename("{JOIN_KEY}":="SSC_NAME_2016"), by=JOIN_KEY) %>% 
    group_by(POA_NAME_2016) %>% 
    mutate(wt = n_MB_CODE_2016/sum(n_MB_CODE_2016)) %>% 
    summarise(across(-SSC_NAME_2016, ~weighted.mean(.x, wt))) %>% 
    select(-n_MB_CODE_2016, -share_MB_CODE_2016, -wt) %>% 
    ungroup()
}
# data.frame(SSC_NAME_2016 = c("Darlinghurst", "Surry Hills"), 
#            avg_income = c(2000, 1000)) %>% 
#   convert_SSC_to_POA() %>% 
#   mutate(expected_avg_income = 2000*178/(178+198) + 1000*198/(178+198)) %>% 
#   mutate(check = expected_avg_income == avg_income)







