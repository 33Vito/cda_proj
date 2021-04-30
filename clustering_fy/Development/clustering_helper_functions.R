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
helper_clustering = function(table) {
  n = floor(nrow(table)/8)
  k = kmeans(table, n)
  table$cluster = k$cluster
  return(table)
}

# Empirically determine n by setting an average of 8 suburbs per cluster
# Hierarchical clustering
helper_h_clustering = function(table) {
  n = floor(nrow(table)/8)
  hierar = hclust(dist(table))
  fit = cutree(hierar, k = n)
  table$cluster = fit
  return(table)
}

