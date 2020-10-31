import_eora <- function(year,
                        indir = paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version),
                        adjust_ppp = FALSE,
                        include.Q = FALSE) {

  print(paste0('Importing Eora T matrix, YEAR = ', year))
  assign('eora.T', read.delim(file.path(indir, paste0(year, "/Eora26_", year, "_bp_T.txt")), header = F, stringsAsFactors = F))
  eora.T <- as.matrix(eora.T)

  print('Importing Eora T labels')
  assign('eora.T.labs', read.delim(file.path(indir, paste0(year, "/labels_T.txt")), header = F, stringsAsFactors = F))
  names(eora.T.labs) <- c('country', 'country.code', 'industries', 'sector')
  eora.T.labs <- eora.T.labs[c('country', 'country.code', 'industries', 'sector')]
  eora.T.labs <- dplyr::group_by(eora.T.labs, country, country.code) %>%
    dplyr::group_by(sector = row_number())
  environment(assert) = environment()
  assert('max(eora.T.labs$sector) == 26', eora.T.labs)

  # Assign row and column names
  eora.T.labs$names <- paste0(eora.T.labs$country.code, ".", eora.T.labs$sector)
  rownames(eora.T) <- eora.T.labs$names
  colnames(eora.T) <- eora.T.labs$names

  # Remove ROW
  eora.T <- eora.T[rownames(eora.T) != "ROW.1", colnames(eora.T) != "ROW.1"]

  # Where T > 0, set to 0
  eora.T[eora.T < 0] <- 0

  assign('mat.names', eora.T.labs$names[grepl("ROW.1", eora.T.labs$names) == F], envir = parent.frame()) # reassign matrix column/row names

  # Import FD
  print('Import matrix FD (final demand)')
  assign('FD', read.delim(file.path(indir, paste0(year, '/Eora26_', year, '_bp_FD.txt')), header = F, stringsAsFactors = F))
  FD <- as.matrix(FD)

  assign('FD.labs', read.delim(file.path(indir, paste0(year, '/labels_FD.txt')), header = F, stringsAsFactors = F))
  names(FD.labs) <- c('country', 'country.code', 'demand', 'sector')
  FD.labs <- FD.labs[c('country', 'country.code', 'demand', 'sector')]
  FD.labs$sector.name <- stringr::str_replace_all(substr(FD.labs$sector, nchar(FD.labs$sector) - 3, nchar(FD.labs$sector)), "\\.", "_")
  FD.labs$names <- paste0(FD.labs$country.code, '.', FD.labs$sector.name)

  environment(assert) <- environment()
  assert('nrow(FD) == nrow(eora.T.labs)')
  assert('ncol(FD) == nrow(FD.labs)')

  rownames(FD) <- eora.T.labs$names
  colnames(FD) <- FD.labs$names

  # Remove ROW from FD
  FD <- FD[rownames(FD) != "ROW.1", colnames(FD) != "ROW.1"]
  assert('nrow(FD) == nrow(eora.T)')

  # Convert to USD constant
  if (adjust_ppp == TRUE) {
    print("Adjusting for PPP")

    ppp <- read.csv(file.path(raw, 'WorldBank/PPP_Indicators_1960-2017.csv'), stringsAsFactors = F)
    ppp <- ppp[c('Country.Code', paste0('X', year))]
    names(ppp) <- c('country', 'ppp')

    for (c in unique(substr(rownames(T), 1, 3))) {

      ppp_index <- ppp$ppp[ppp$country == c]

      eora.T[, (substr(colnames(eora.T), 1, 3) == c)] <-
        eora.T[, (substr(colnames(eora.T), 1, 3) == c)] * ppp_index

      FD[, (substr(colnames(FD), 1, 3) == c)] <-
        FD[, (substr(colnames(FD), 1, 3) == c)] * ppp_index

    }
  }

  # Assign output to parent frame
  assign('eora.T', eora.T, envir = parent.frame())
  assign('eora.T.labs', eora.T.labs, envir = parent.frame())
  assign('FD', FD, envir = parent.frame())

  # If include.Q == TRUE, include satelite account
  if (include.Q == TRUE) {
    print('Import satelite account (Q)')
    assign('Q', read.delim(file.path(indir, paste0(year, '/Eora26_', year, '_bp_Q.txt')), header = F, stringsAsFactors = F))
    Q <- as.matrix(Q)

    assign('Q.labs', read.delim(file.path(indir, paste0(year, '/labels_Q.txt')), header = F, stringsAsFactors = F))
    Q.labs <- Q.labs[,1:2]
    names(Q.labs) <- c('type', 'detail')
    Q.labs$names <- stringr::str_replace_all(stringr::str_replace_all(Q.labs$detail, "[[:punct:]]", ""), " ", "")

    environment(assert) <- environment()
    assert('ncol(Q) == nrow(eora.T.labs)')
    assert('nrow(Q) == nrow(Q.labs)')

    rownames(Q) <- Q.labs$names
    colnames(Q) <- eora.T.labs$names

    # Remove ROW from Q
    Q <- Q[, colnames(Q) != "ROW.1"]
    assert('ncol(Q) == ncol(eora.T)')

    assign('Q', Q, envir = parent.frame())
  }

  # Reassign iso.list to only include countries found in Eora that year (e.g. SSD)
  assign('country.list', iso.list[iso.list %in% eora.T.labs$country.code], envir = parent.frame())
}
