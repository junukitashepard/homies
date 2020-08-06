# Domestic submatrices #
########################
setup.domestic <- function(country, year) {

  if (file.exists(file.path(output, 'PRIM_submat', paste0(year, '/mat_', country, '.rds'))) &
      file.exists(file.path(output, 'ELEC_submat', paste0(year, '/mat_', country, '.rds'))) &
      file.exists(file.path(output, 'NONE_submat', paste0(year, '/mat_', country, '.rds')))) {
  print(paste0('Assigning domestic matrix:', country))

  assign('dom.primary', readRDS(file.path(output, 'PRIM_submat', paste0(year, '/mat_', country, '.rds'))))
    dom.primary[grepl('PET', rownames(dom.primary)), grepl('PET', colnames(dom.primary))] <- 0

  assign('dom.elec', readRDS(file.path(output, 'ELEC_submat', paste0(year, '/mat_', country, '.rds'))))
  dom.elec <- rbind(dom.elec, matrix(0, ncol = 39, nrow = 1))
  rownames(dom.elec)[5] <- 'LOSSES'
  rownames(dom.elec)[grepl('ELHE', rownames(dom.elec)) == T] <- energy.names[grepl('ELEC', energy.names) == T]
  assign('dom.none', readRDS(file.path(output, 'NONE_submat', paste0(year, '/mat_', country, '.rds'))))
  assign('dom.mat', rbind(dom.primary, dom.elec, dom.none))

  # Change column and row names
  colnames(dom.mat)[colnames(dom.mat) %in% energy.names] <- rownames(dom.mat)[rownames(dom.mat) %in% energy.names] <-
    lapply(colnames(dom.mat)[colnames(dom.mat) %in% energy.names],
           function(x) paste0(country, '.', x))

  print("Assert conditions [2]")
  environment(assert) <- environment()
  assert('nrow(dom.mat) == ncol(dom.mat)')
  assert('rownames(dom.mat) == colnames(dom.mat)')

  # Insert matrix into empty matrix
  assign('mat', get(paste0('mat.', year)))
  mat[rownames(mat) == rownames(dom.mat), colnames(mat) == colnames(dom.mat)] <- dom.mat
  assign(paste0('mat.', year), mat, envir = parent.frame())
}
}
