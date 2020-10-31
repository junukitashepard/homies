# Function: multiply with FD to get total output
get_total <- function(matrix.in) {
  assign('mat', get(matrix.in))

  assign('x.total', mat%*%FD)

  assign('country.names', unique(gsub("\\..*$", "", rownames(x.total))))
  x.out <- matrix(nrow = nrow(x.total), ncol = 0)
  for (c in country.names) {
    assign('x.c', x.total[, grepl(c, colnames(x.total))])
    x.c <- rowSums(x.c)
    x.out <- cbind(x.out, x.c)
    colnames(x.out)[which(country.names == c, arr.ind = T)] <- c
  }

  return(x.out)
}
