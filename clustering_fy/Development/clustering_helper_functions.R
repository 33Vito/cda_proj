# Principle component transform
# Convert original datasets' raw features to PC features
helper_pc_convert = function(table) {
  pc = principal(scale(table), nfactors = ncol(table), rotate='varimax')
  
  all_table_pc = data.frame(pc$scores)
  
  return(all_table_pc)
}

# Empirically determine n by setting an average of 8 suburbs per cluster
helper_clustering = function(table) {
  n = floor(nrow(table)/8)
  k = kmeans(table, n)
  table$cluster = k$cluster
  return(table)
}
