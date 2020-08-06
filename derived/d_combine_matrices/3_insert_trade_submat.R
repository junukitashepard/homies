# Trade submatrices #
#####################
setup.trade <- function(country, year) {
  
  assign('fun.iso.list', country.list[country.list != country]) # ISO list without country of interest
  
  for (importer in fun.iso.list[1:length(fun.iso.list)]) {
    if (file.exists(file.path(output, paste0('TRADE_ENERGY_submat/', year, '/', importer, '/mat_', country, 'to', importer, '.rds'))) &
        file.exists(file.path(output, paste0('TRADE_NONE_submat/', year, '/', importer, '/mat_', country, 'to', importer, '.rds')))) {
    print(paste0('Reading trade matrices: ', importer))
    assign('trade.energy.mat', readRDS(file.path(output, paste0('TRADE_ENERGY_submat/', year, '/', importer, '/mat_', country, 'to', importer, '.rds'))))
    assign('trade.eora.mat', readRDS(file.path(output, paste0('TRADE_NONE_submat/', year, '/', importer, '/mat_', country, 'to', importer, '.rds'))))
    
    # Combine files
    trade.mat <- rbind(trade.energy.mat, trade.eora.mat)
    
    # Insert into empty matrix 
    assign('mat', get(paste0('mat.', year)))
    mat[rownames(mat) == rownames(trade.mat), colnames(mat) == colnames(trade.mat)] <- trade.mat
    assign(paste0('mat.', year), mat, envir = parent.frame())
  }
  }
}
