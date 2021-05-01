# Principle component transform
# Convert original datasets' raw features to PC features
helper_pc_convert = function(table, cutoff=0.9) {
  pc = principal(scale(table), nfactors = ncol(table), rotate='varimax')
  # min n required for var explanability to reach cutoff point
  n_keep = min(which(cumsum(pc$values/sum(pc$values))>cutoff))
  
  all_table_pc = data.frame(pc$scores)[, 1:n_keep]
  
  return(all_table_pc)
}

# Empirically determine n by setting an average of 8 suburbs per cluster
helper_clustering = function(table, k=8) {
  n = floor(nrow(table)/k)
  k = kmeans(table, n)
  table$cluster = k$cluster
  return(table)
}

# Empirically determine n by setting an average of 8 suburbs per cluster
# Hierarchical clustering
helper_h_clustering = function(table, k=8) {
  n = floor(nrow(table)/k)
  hierar = hclust(dist(table))
  fit = cutree(hierar, k = n)
  table$cluster = fit
  return(table)
}

# Calculating effectiveness
helper_effective = function(cluster_table, lockdown_n=14) {

# Link post code
covid_cluster = covid %>%
  left_join(cluster_table, by = c("postcode"="POA_CODE_2016")) %>%
  select(notification_date, postcode, lga_name19, cluster) %>% 
  filter(!is.na(cluster))

# Give a key to each covid case
covid_cluster =  covid_cluster %>%
  bind_cols(data.frame(case=1:nrow(covid_cluster)))

# Identify how many near future cases can be identified from previous clusters
covid_predicted = covid_cluster %>%
  left_join(covid_cluster, by = "cluster") %>%
  filter(notification_date.y - notification_date.x <= lockdown_n, notification_date.y - notification_date.x > 0) %>%
  select(c(6, 7, 8, 9, 4)) %>%
  dplyr::distinct() %>%
  arrange(case.y)

# Initial effectiveness
effectiveness = nrow(covid_predicted) / max(covid_cluster$case)

return(effectiveness)
}
