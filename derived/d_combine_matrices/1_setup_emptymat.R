# Set up empty matrix #
#######################
setup.emptymat <- function() {
  assign('dim', length(country.list) * (26 + length(energy.names)))
  assign('empty.mat', matrix(0, nrow = dim, ncol = dim))
  
  assign('dimnames', c(energy.names, as.character(as.list(1:26))))
  
  assign('rnames', as.list(matrix(NA, nrow = dim, ncol = 1)))
  assign('cnames', as.list(matrix(NA, nrow = dim, ncol = 1)))
  
  k <- 1
  for (i in country.list) {
    for (j in dimnames) {
      rnames[k] <- paste0(i, '.', j)
      cnames[k] <- paste0(i, '.', j)
      k <- k + 1
    }
  }
  
  rownames(empty.mat) <- rnames
  colnames(empty.mat) <- cnames
  return(empty.mat)
}