# Multiply by FD
multiply_FD <- function(in.df, subset.sector = FALSE, keep.to.sector = FALSE) {

  assign('df', get(in.df))

  df <- left_join(df, FD, by = c('to.country', 'to.sector'))
  df$value <- df$value * df$FD

  if (subset.sector == FALSE & keep.to.sector == FALSE) {
    print("Collapsing for all to sectors")
    df <- dplyr::group_by(df, from.country, to.country, from.sector, type, cp) %>%
          dplyr::summarise(value = sum(value, na.rm = T))
  } else if (subset.sector == TRUE & keep.to.sector == FALSE) {
    print(paste0("Collapsing to SECTOR = ", in.sector))
    df <- subset(df, to.sector == in.sector)
    df$to.sector <- df$FD <- NULL
  }

  assign(in.df, df, envir = parent.frame())
}
