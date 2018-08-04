type_splitter <- function(df) {
  list_dfs <- split(df, f = df$type)
  # The data frames are now split by type, just need to iterate over this and reassign ranks
  
  list_dfs <- lapply(list_dfs, rank_fixer)
  
  return(bind_rows(list_dfs))
  
}