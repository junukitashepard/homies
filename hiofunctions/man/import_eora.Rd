\name{import_eora}
\alias{import_eora}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Import Eora-26
}
\description{
Imports underlying Eora-26 matrices for use in building the HG-MRIO model
}
\usage{
import_eora(year,
            indir = paste0("/data/jus3/GlobalIO/raw/EORA/", eora.version),
            include.Q = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year, indir, include.Q}{
%%     ~~Describe \code{x} here~~
}
}
\details{
"year" is the model year, "indir" is the Eora raw file directory, "include.Q" specifies whether to include satelite files
}

\references{
Imports Eora-26 matrices (Lenzen, 2013)
}
\author{
Jun Shepard
}
