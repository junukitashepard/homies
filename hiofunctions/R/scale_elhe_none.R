################################################################
# Function to scale electricity and heat to non-energy sectors #
################################################################
scale_elhe_none <- function(c, y, secondary = TRUE, name = 'secondary', domestic = TRUE) {

  # Subset to given country
  if (domestic == TRUE) {
    use.mat <- eora.T[rownames(eora.T)[grepl(c, rownames(eora.T))], colnames(eora.T)[grepl(c, colnames(eora.T))]]
  }

  # Set up scalars
  for (i in 1:26) {
    assign(paste0('elhe2sec', i), NA)
  }
  for (i in c(15, 18, 20, 21, 22, 23)) {
    assign(paste0('compub.', i), NA)
  }

  # Scale commercial and public services
  assign('all.compub', sum(use.mat[c(3, 7, 13),15]) + sum(use.mat[c(3, 7, 13), 18]) +
           sum(use.mat[c(3, 7, 13), 20]) + sum(use.mat[c(3, 7, 13), 21]) +
           sum(use.mat[c(3, 7, 13), 22]) + sum(use.mat[c(3, 7, 13), 22]) +sum(use.mat[c(3, 7, 13), 23]))
  compub.15 <- sum(use.mat[c(3, 7, 13), 15])/all.compub
  compub.18 <- sum(use.mat[c(3, 7, 13), 18])/all.compub
  compub.20 <- sum(use.mat[c(3, 7, 13), 20])/all.compub
  compub.21 <- sum(use.mat[c(3, 7, 13), 21])/all.compub
  compub.22 <- sum(use.mat[c(3, 7, 13), 22])/all.compub
  compub.23 <- sum(use.mat[c(3, 7, 13), 23])/all.compub

  # Electricity/heat to NON-E
  elhe2sec1 <- abs(df$value[df$flow == 'Agriculture/forestry' & df$year == y])
  elhe2sec2 <- abs(df$value[df$flow == 'Fishing' & df$year == y])
  elhe2sec3 <- abs(df$value[df$flow == 'Mining and quarrying' & df$year == y])
  elhe2sec4 <- abs(df$value[df$flow == 'Food and tobacco' & df$year == y])
  elhe2sec5 <- abs(df$value[df$flow == 'Textile and leather' & df$year == y])
  elhe2sec6 <- sum(abs(df$value[(df$flow == 'Wood and wood products' | df$flow == 'Paper, pulp and print' |
                                 df$flow == 'Memo: Non-energy use in wood and wood products' |
                                 df$flow == 'Memo: Non-energy use in paper/pulp and printing') & df$year == y]))
  elhe2sec7 <- sum(abs(df$value[(df$flow == 'Chemical and petrochemical' | df$flow == 'Non-metallic minerals') & df$year == y]))
  elhe2sec8 <- sum(abs(df$value[(df$flow == 'Iron and steel' | df$flow == 'Non-ferrous metals' | df$flow == 'Machinery' |
                                 df$flow == 'Memo: Non-energy use in non-ferrous metals' |
                                 df$flow == 'Memo: Non-energy use in iron and steel') & df$year == y]))
  elhe2sec9 <- sum(abs(df$value[(df$flow == 'Machinery' | df$flow == 'Energy industry own use' |
                                 df$flow == 'Non-specified (energy)' |
                                 df$flow == 'Own use in electricity, CHP and heat plants (energy)') & df$year == y]))
  elhe2sec10 <- sum(abs(df$value[(df$flow == 'Transport equipment') & df$year == y]))
  elhe2sec11 <- abs(df$value[df$flow == 'Non-specified (industry)' & df$year == y])
  elhe2sec12 <- sum(abs(df$value[(df$flow == 'Transfers') & df$year == y]))

  if (secondary == FALSE) {
    elhe2sec13 <- sum(abs(df$value[(df$flow == 'Main activity producer electricity plants (transf.)' |
                                      df$flow == 'Main activity producer heat plants (transf.)')
                                   & df$year == y])) - elhe2elec
  } else if (secondary == TRUE) {
    elhe2sec13 <- sum(abs(df$value[(df$flow == 'Own use in electricity, CHP and heat plants (energy)' |
                                      df$flow == 'Main activity producer heat plants (transf.)')
                                   & df$year == y])) - elhe2elec
  }

  elhe2sec14 <- abs(df$value[df$flow == 'Construction' & df$year == y])

  elhe2sec19 <- abs(df$value[df$flow == 'Transport' & df$year == y])

  assign('elhe2compub', abs(df$value[df$flow == 'Commercial and public services' & df$year == y]))
  elhe2sec15 <- elhe2compub * compub.15
  elhe2sec18 <- elhe2compub * compub.18
  elhe2sec20 <- elhe2compub * compub.20
  elhe2sec21 <- elhe2compub * compub.21
  elhe2sec22 <- elhe2compub * compub.22
  elhe2sec23 <- elhe2compub * compub.23

  #elhe2sec24 <- abs(df$value[df$flow == 'Residential' & df$year == y])

  # For wholesale/retail trade, import from EORA itself
  if (name %in% c('NG', 'COAL', 'PET')) {
    elhe2sec16 <- Q[name, paste0(c, '.16')] * unique(df$share[df$year == y])
    elhe2sec17 <- Q[name, paste0(c, '.17')] * unique(df$share[df$year == y])
    elhe2sec25 <- Q[name, paste0(c, '.25')] * unique(df$share[df$year == y])
    elhe2sec26 <- Q[name, paste0(c, '.26')] * unique(df$share[df$year == y])
    elhe2sec24 <- Q[name, paste0(c, '.24')] * unique(df$share[df$year == y])
  }


  for (i in 1:26) {
    assign('sec', get(paste0('elhe2sec', i)))
    if (length(sec) == 0) {sec <- 0}
    assign(paste0('elhe2sec', i), sec)
  }

  vec.none <- c(elhe2sec1, elhe2sec2, elhe2sec3, elhe2sec4, elhe2sec5, elhe2sec6, elhe2sec7, elhe2sec8, elhe2sec9,
                elhe2sec10, elhe2sec11, elhe2sec12, elhe2sec13, elhe2sec14, elhe2sec15, elhe2sec16, elhe2sec17, elhe2sec18,
                elhe2sec19, elhe2sec20, elhe2sec21, elhe2sec22, elhe2sec23, elhe2sec24, elhe2sec25, elhe2sec26)
  return(vec.none)
}
