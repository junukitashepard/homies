\name{scale_elhe_none}
\alias{scale_elhe_none}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale energy to non-energy flows
}
\description{
Scale specified energy flows into non-energy industries, specified in Eora-26
}
\usage{
scale_elhe_none(c, y, secondary = TRUE, name = 'secondary', domestic = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c, y, secondary = TRUE, name = 'secondary', domestic = TRUE}{
%%     ~~Describe \code{x} here~~
}
}
\details{
"c" is the country specified; "y" is the year specified, "secondary" is whether energy flow is secondary energy (i.e. electricity); name is either 'secondary' for secondary energy flows or the name of the primary energy flow (e.g. "CRU"), domestic is for whether scaling is for domestic (rather than bilateral) transactions.
}
\references{
For use with Eora-26 (Lenzen, 2013)
}
\author{
Jun Shepard
}
