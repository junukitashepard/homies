# quiet: run function quietly (by Hadley Wickham)
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}