# Principle component transform
# Convert original datasets' raw features to PC features
helper_pc_convert = function(table, identi) {
  table1 = table %>%
    select(-c(identi))
  pc = principal(scale(table1), nfactors = ncol(table1), rotate='varimax')
  
  all_table_pc = data.frame(pc$scores)
  all_table_pc[identi] = all_table[identi]
  
  return(all_table_pc)
}

# Empirally determine n by setting an average of 8 suburbs per cluster
helper_clustering = function(table) {
  n = floor(nrow(table)/8)
  k = kmeans(all_table1, n)
  table$cluster = k$cluster
  return(table)
}

