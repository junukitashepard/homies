import_hybrid_eora <- function(input.dir = "/data/jus3/GlobalIO/output/derived",
                               wb.dir = "/data/jus3/GlobalIO/raw",
                               raw.dir = paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version),
                               year, balance_flows = TRUE, adjust_ppp = FALSE, exclude_countries = NA) {

  print(paste0('Setting up, YEAR = ', year))
  print('Import matrix T')
  assign('T', readRDS(file.path(input.dir, paste0('IO_MAT/io_mat_', year, '.rds'))))
  environment(assert) <- environment()
  quiet(assert('nrow(T) == ncol(T)'))
  T[T < 0 | is.na(T)] <- 0

  print('Import labels for Eora T')
  assign('eora.T.labs', read.delim(file.path(raw.dir, paste0(year, "/labels_T.txt")), header = F, stringsAsFactors = F))
  names(eora.T.labs) <- c('country', 'country.code', 'industries', 'sector')
  eora.T.labs <- eora.T.labs[c('country', 'country.code', 'industries', 'sector')]
  eora.T.labs <- dplyr::group_by(eora.T.labs, country, country.code) %>%
    dplyr::group_by(sector = row_number())
  quiet(assert('max(eora.T.labs$sector) == 26', eora.T.labs))

  print('Import matrix "FD" (final demand)')
  assign('FD', readRDS(file.path(input.dir, paste0('FD_MAT/fd_mat_', year, '.rds'))))
  FD[FD < 0 | is.na(FD)] <- 0

  assert('nrow(FD) == nrow(T)')

  # Balance HIO energy flows with DIO direct energy flows for PET, NG, COAL #
  ###########################################################################
  if (balance_flows == TRUE) {
    print('Balance DIO-Q and HIO-T energy flows')
    print('Import satelite account (Q)')
    assign('Q', read.delim(file.path(raw.dir, paste0(year, '/Eora26_', year, '_bp_Q.txt')), header = F, stringsAsFactors = F))
    Q <- as.matrix(Q)

    assign('Q.labs', read.delim(file.path(raw.dir, paste0(year, '/labels_Q.txt')), header = F, stringsAsFactors = F))
    Q.labs <- Q.labs[,1:2]
    names(Q.labs) <- c('type', 'detail')
    Q.labs$names <- stringr::str_replace_all(stringr::str_replace_all(Q.labs$detail, "[[:punct:]]", ""), " ", "")

    assert('nrow(Q) == nrow(Q.labs)')
    rownames(Q) <- Q.labs$names
    colnames(Q) <- paste0(eora.T.labs$country.code, '.', eora.T.labs$sector)

    # Remove ROW from Q
    Q <- Q[, colnames(Q) != "ROW.1"]

    # Extract energy flows to non-energy sectors
    assign('dio.pet_flows', as.data.frame(Q['Petroleum', ]))
    assign('dio.coal_flows', as.data.frame(Q['Coal',]))
    assign('dio.ng_flows', as.data.frame(Q['NaturalGas',]))
    dio.pet_flows$sink <- dio.coal_flows$sink <- dio.ng_flows$sink <- rownames(dio.pet_flows)
    names(dio.pet_flows) <- names(dio.coal_flows) <- names(dio.ng_flows) <- c('dio', 'sink')

    assign('hio.pet_flows', as.data.frame(colSums(T[grepl('PET', rownames(T)), ])))
    assign('hio.coal_flows', as.data.frame(colSums(T[grepl('COAL', rownames(T)), ])))
    assign('hio.ng_flows', as.data.frame(colSums(T[grepl('NG', rownames(T)), ])))
    hio.pet_flows$sink <- hio.coal_flows$sink <- hio.ng_flows$sink <- rownames(hio.pet_flows)
    names(hio.pet_flows) <- names(hio.coal_flows) <- names(hio.ng_flows) <- c('hio', 'sink')

    # Plot difference
    library('ggplot2')
    for (flow in c('pet_flows', 'coal_flows', 'ng_flows')) {

      # Add energy sinks to non-energy sinks to get the non-energy difference
      assign('hioflow', get(paste0('hio.', flow)))
      assign('hioflow_mined', subset(hioflow, (substr(sink, 5, 8) == 'COAL' |
                                               substr(sink, 5, 7) == 'CRU' |
                                               substr(sink, 5, 6) == 'NG')))
      assign('hioflow_elec', subset(hioflow, (substr(sink, 5, 8) == 'ELEC')))
      assign('hioflow_pet', subset(hioflow, (substr(sink, 5, 7) == 'PET')))

      hioflow_mined$asink <- 3
      hioflow_elec$asink <- 13
      hioflow_pet$asink <- 7

      for (esink in c('mined', 'elec', 'pet')) {
        assign('df', get(paste0('hioflow_', esink)))
        df$country <- substr(df$sink, 1, 3)
        df <- group_by(df, country, asink) %>% summarise(hio = sum(hio))
        df$sink <- paste0(df$country, '.', df$asink)
        df$country <- df$asink <- NULL
        names(df)[names(df) == 'hio'] <- paste0('hio_', esink)
        assign(paste0('hioflow_', esink), df)
      }

      hioflow <- left_join(hioflow, hioflow_mined, by = c('sink')) %>%
                 left_join(hioflow_elec, by = c('sink')) %>%
                 left_join(hioflow_pet, by = c('sink'))
      hioflow$hio[substr(hioflow$sink, 5, 5) == 3] <- hioflow$hio[substr(hioflow$sink, 5, 5) == 3] + hioflow$hio_mined[substr(hioflow$sink, 5, 5) == 3]
      hioflow$hio[substr(hioflow$sink, 5, 5) == 7] <- hioflow$hio[substr(hioflow$sink, 5, 5) == 7] + hioflow$hio_pet[substr(hioflow$sink, 5, 5) == 7]
      hioflow$hio[substr(hioflow$sink, 5, 6) == 13] <- hioflow$hio[substr(hioflow$sink, 5, 6) == 13] + hioflow$hio_elec[substr(hioflow$sink, 5, 6) == 13]
      hioflow$hio_mined <- hioflow$hio_pet <- hioflow$hio_elec <- NULL

      # Link HIO to DIO
      assign('comp', inner_join(hioflow, get(paste0('dio.', flow)), by = c('sink')))
      comp$hio[is.na(comp$hio)] <- 0
      comp$difference <- comp$dio - comp$hio

      assign('plot', ggplot(aes(x = difference), data = comp))
      plot <- plot +
              geom_histogram(fill = 'blue', colour = 'darkblue', alpha = 0.3) +
              labs(title = paste0("Difference between DIO and HIO direct energy inputs"),
                   subtitle = paste0("(", flow, ")"),
                   x = "Difference (TJ)", y = "Frequency")
      assign(paste0('plotdif.', flow), plot, envir = parent.frame())
      assign(paste0('comp.', flow), comp, envir = parent.frame())
    }

    # Balance by assigning difference by share of source
    for (flow in c('pet', 'coal', 'ng')) {
      print(paste0('Balancing: ', toupper(flow)))
      assign('hio.base', T[grepl(paste0(".",toupper(flow)), rownames(T)),])
      assign('diff', get(paste0('comp.', flow, '_flows')))

      # For sectors 3, 7, and 13, use shares from energy sectors
      for (country in unique(substr(colnames(hio.base), 1, 3))) {
        hio.base[, paste0(country, ".", "3")] <-   hio.base[, paste0(country, ".", "3")] +
                                             hio.base[, paste0(country, ".", "COAL")] +
                                             hio.base[, paste0(country, ".", "CRU")] +
                                             hio.base[, paste0(country, ".", "NG")]
        hio.base[, paste0(country, ".7")] <- hio.base[, paste0(country, ".7")] + hio.base[, paste0(country, ".PET")]
        hio.base[, paste0(country, ".13")] <-hio.base[, paste0(country, ".13")] + hio.base[, paste0(country, ".ELEC_RE")] +
                                              hio.base[, paste0(country, ".ELEC_NU")] +
                                              hio.base[, paste0(country, ".ELEC_HY")] +
                                              hio.base[, paste0(country, ".ELEC_COM")]
      }
      assign('hio.tot', as.data.frame(colSums(hio.base)))
      names(hio.tot) <- 'hio'
      hio.tot$sink <- rownames(hio.tot)
      hio.tot$hio[hio.tot$hio == 0] <- 1 # to not have NaN in dividing

      hio.base[is.na(hio.base)] <- 0

      for (country in unique(substr(colnames(hio.base), 1, 3))) {
        for (i in 1:26) {
          assign('total_industry', sum(hio.base[, paste0(country, '.', i)]))
          if (total_industry == 0) {
            hio.base[grepl(country, rownames(hio.base)), paste0(country, '.', i)] <- 1
          }
        }
      }

      hio.base <- sweep(hio.base, 2, hio.tot$hio, FUN = '/') # Share of source

      diff <- left_join(hio.tot['sink'], diff, by = c('sink'))[c('sink', 'difference')] # Difference only applies to non-energy sector
      diff$difference[is.na(diff$difference) | diff$difference < 0] <- 0

      hio.base <- sweep(hio.base, 2, diff$difference, FUN = '*')

      assert('dim(T[grepl(paste0(".", toupper(flow)), rownames(T)),]) == dim(hio.base)')
      T[grepl(paste0(".", toupper(flow)), rownames(T)), ] <- T[grepl(paste0(".",toupper(flow)), rownames(T))] + hio.base
      rm(list = c('hio.base', 'hio.tot', 'diff'))
    }
  }

  # Convert to USD constant
  if (adjust_ppp == TRUE) {
    print("Adjusting for PPP")
    ppp <- read.csv(file.path(wb.dir, 'WorldBank/PPP_Indicators_1960-2017.csv'), stringsAsFactors = F)
    ppp <- ppp[c('Country.Code', paste0('X', year))]
    names(ppp) <- c('country', 'ppp')

    for (c in unique(substr(rownames(T), 1, 3))) {

      ppp_index <- ppp$ppp[ppp$country == c]

      T[, (substr(colnames(T), 1, 3) == c & gsub('.*\\.', '', colnames(T)) %in% 1:26)] <-
        T[, (substr(colnames(T), 1, 3) == c & gsub('.*\\.', '', colnames(T)) %in% 1:26)] * ppp_index

      FD[, (substr(colnames(FD), 1, 3) == c & gsub('.*\\.', '', colnames(FD)) %in% 1:26)] <-
        FD[, (substr(colnames(FD), 1, 3) == c & gsub('.*\\.', '', colnames(FD)) %in% 1:26)] * ppp_index

    }
  }

  # Exclude specified countries
  if (!is.na(exclude_countries)) {
    T <- T[!(substr(rownames(T), 1, 3) %in% exclude_countries), !(substr(colnames(T), 1, 3) %in% exclude_countries)]
    FD <- FD[!(substr(rownames(FD), 1, 3) %in% exclude_countries), !(substr(colnames(FD), 1, 3) %in% exclude_countries)]
  }

  # Calculate total output
  assign('x_int', rowSums(T))
  assign('x_fd', rowSums(FD))
  assign('x', x_int + x_fd)
  names(x) <- rownames(T)
  assert('length(x) == nrow(T)')

  # Output T, x, FD, labels
  assign('hybrid.T', T, envir = parent.frame())
  assign('hybrid.T.labs', eora.T.labs, envir = parent.frame())
  assign('hybrid.FD', FD, envir = parent.frame())
  assign('hybrid.x', x, envir = parent.frame())
}

