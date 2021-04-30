# Principle component transform
# Convert original datasets' raw features to PC features
helper_pc_convert = function(table, cutoff=0.9) {
  pc = principal(scale(table), nfactors = ncol(table), rotate='varimax')
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
