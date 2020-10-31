\name{isid}
\alias{isid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
isid
}
\description{
Make sure that list of variables are/aren't unique identifiers of a dataframe
}
\usage{
data, variables, not_id = FALSE
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data, variables, not_id}{
%%     ~~Describe \code{x} here~~
}
}
\details{
"data" is a character object for data name; "variables" is a list of character objects for variables in data; "not_id" is TRUE when
you want to ensure that a list of variables do NOT identify a dataset, otherwise you are looking for unique identifiers.
}

\references{
Based on Stata, isid
}
\author{
Jun Shepard
}
