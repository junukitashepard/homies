assign('mat', L)

clist <- list()
for (c in iso.list[!(iso.list %in% no_energy.list)]) {
  print(paste0('Add COUNTRY = ', c))
  clist <- c(clist, c)
  matin <- mat[substr(rownames(mat), 1, 3) %in% clist, substr(colnames(mat), 1, 3) %in% clist]
  matout <- solve(matin)
}

assign('mat', L)
mat <- mat[substr(rownames(mat), 1, 3) != 'FIN', substr(colnames(mat), 1, 3) != 'FIN']

check <- mat[substr(rownames(mat), 1, 3) == 'FIN', substr(colnames(mat), 1, 3) == 'FIN']