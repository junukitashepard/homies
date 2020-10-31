\name{import_hybrid_eora}
\alias{import_hybrid_eora}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import hybridized Eora-26
}
\description{
Imports hybridized Eora-26 matrices.
}
\usage{
import_hybrid_eora(input.dir = "/data/jus3/GlobalIO/output/derived",
                   raw.dir = raw <-  paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version),
                   year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.dir, raw.dir, year}{
%%     ~~Describe \code{x} here~~
}
}
\details{
"input.dir" contains derived Eora-26 matrices (hybrid); "raw.dir" contains underlying raw Eora-26 matrices;
year is model year
}

\references{
Based on Eora-26 matrices (Lenzen, 2013)
}
\author{
Jun Shepard
}
