\name{get_total}
\alias{get_total}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multiply IO matrix by FD to get total output
}
\description{
Multiply technical coefficients (i.e. direct requirements or total requirements) by FD matrix, collapsing FD by country
}
\usage{
get_total(matrix.in)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix.in}{
}
}
\details{
"matrix.in" is a direct requirements or total requirements, to be multiplied by FD following the formula: (I-A)^(-1)FD = X
}
