\name{scale_elhe}
\alias{scale_elhe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scale energy to energy flows
}
\description{
Scale specified energy flows into other energy sinks.
}
\usage{
scale_elhe(c, y, secondary = TRUE, name = 'secondary', domestic = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c, y, secondary = TRUE, name = 'secondary', domestic = TRUE}{

}
}
\details{
"c" is the country specified; "y" is the year specified, "secondary" is whether energy flow is secondary energy (i.e. electricity); name is either 'secondary' for secondary energy flows or the name of the primary energy flow (e.g. "CRU"), domestic is for whether scaling is for domestic (rather than bilateral) transactions.
}

\author{
Jun Shepard
}
