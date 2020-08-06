T[, grepl('LOSSES', colnames(T))] <- -1 * T[, grepl('LOSSES', colnames(T))]
T[sub('.*\\.', '', rownames(T)) %in% energy.names, grepl('25', colnames(T))] <- 0

nuc.in <- T[grepl('NUC', rownames(T)), 'USA.ELEC_NU']
  #nuc.in <- nuc.in[nuc.in > 0]
nuc.out <- T['USA.ELEC_NU', ]
  #nuc.out <- nuc.out[nuc.out > 0]
  
hyd.in <- T[grepl('HYD', rownames(T)), 'USA.ELEC_HY']
  #hyd.in <- hyd.in[hyd.in > 0]
hyd.out <- T['USA.ELEC_HY',]
  #hyd.out <- hyd.out[hyd.out > 0]
  
coal.in <- T[grepl('COAL', rownames(T)), 'USA.ELEC_COM']
  #coal.in <- coal.in[coal.in > 0]
  
com.out <- T['USA.ELEC_COM',]
#com.out[grepl('ELEC', names(com.out))] <- -1 * com.out[grepl('ELEC', names(com.out))]
  com.out <- com.out[com.out != 0 ]

com.out[sub('.*\\.', '', names(com.out)) %in% energy.names] <- 0

com.in <- T[sub('.*\\.', '', rownames(T)) %in% c('BIO', 'COAL', 'CRU', 'NG', 'PET'), 'USA.ELEC_COM']
  com.in <- com.in[com.in != 0]
  
re.in <- T[grepl('RE', rownames(T)), 'USA.ELEC_RE']
re.out <- T['USA.ELEC_RE', ]

tomwh <- function(x) {
  x * (10^12) * (0.00000028) * (10^-3)
}