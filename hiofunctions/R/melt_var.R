# Function: melt matrices
melt_var <- function(matrix.in) {

  assign('mat', get(matrix.in))

  mat <- melt(mat)
  mat$Var1 <- as.character(mat$Var1)
  mat$Var2 <- as.character(mat$Var2)

  mat$from.country <- substr(mat$Var1, 1, 3)
  mat$to.country <- substr(mat$Var2, 1, 3)

  mat$from.sector <- stringr::str_remove(mat$Var1, "^.*\\.")
  mat$to.sector <- stringr::str_remove(mat$Var2, "^.*\\.")

  mat <- mat[c('from.country', 'to.country', 'from.sector', 'to.sector', 'value')]

  if (grepl(matrix.in, "A")) {mat$type <- "Direct"} else {mat$type <- "Total"}
  if (grepl(matrix.in, "cp")) {mat$cp <- 1} else {mat$cp <- 0}

  return(mat)
}
