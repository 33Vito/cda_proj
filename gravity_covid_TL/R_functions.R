source("utility.R")
library(sp)

#-----------------------------Function to plot choropleth map-----------------------------
plot_map_TL <- function(df, sdf, key_var, fill_var, title_text, 
                        show_count=FALSE, label_size=3, 
                        return_obj="combined") {
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
    scale_fill_gradient(low="grey88", high="darkred") + 
    # scale_fill_viridis_c(option = "D", direction = -1, breaks = pretty_breaks(5)) +
    scale_alpha_continuous(range = c(0,.3)) +
    labs(title = title_text) + 
    theme_void() +
    # theme(legend.position = "right")
    theme(legend.position = c(.93,.15))
  
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
    scale_fill_gradient(low="grey88", high="darkred") + 
    # scale_fill_viridis_c(option = "D", direction = -1, breaks = pretty_breaks(5)) +
    coord_flip() + 
    xlab("") + 
    ylim(c(0, 1.1*max(df[[fill_var]], na.rm=T))) + 
    labs(subtitle = paste0("Top 20 ", key_var)) + 
    ggl() + 
    theme(legend.position = "none")
  
  switch(return_obj, 
         "combined" = gg_map + gg_bar + plot_layout(widths = c(3, 1)), 
         "map" = gg_map, 
         "bar" = gg_bar)
}