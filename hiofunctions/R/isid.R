isid <- function(data, variables, not_id = FALSE) {
  assign('d', get(data))
  u <- unique(d[variables])
  id <- eval(nrow(d) == nrow(u))
  
  varlist <- ""  
  for (v in variables) {
    varlist <- paste0(varlist, " ", v)
  }   
  if (id == TRUE & not_id == FALSE) {
    print(paste0("Dataframe ", data, " is uniquely ID'd by:"))
    print(paste0("    ", varlist))
  } else if (id == TRUE & not_id == TRUE){
    stop(paste0("Dataframe ", data, " is uniquely ID'd! (And you don't want it to be)"))
  } else if (id == FALSE & not_id == FALSE) {
    stop(paste0("Dataframe ", data, " is NOT uniquely ID'd! (And you want it to be)"))
  } else if (id == FALSE & not_id == TRUE) {
    print(paste0("Dataframe ", data, " is not uniquely ID'd, and this is what you want"))
  }
}