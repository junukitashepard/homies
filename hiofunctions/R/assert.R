assert <- function(condition, na.rm = TRUE) {
  is_true <- all(eval(parse(text = condition)), na.rm = na.rm)
  if (is_true == TRUE) {
    print("Assertion TRUE")
  } else {
    stop(paste0(condition, ' is not TRUE!'))
  }
}
