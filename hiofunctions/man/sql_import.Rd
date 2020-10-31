\name{sql_import}
\alias{sql_import}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SQL import
}
\description{
Import dataframes from SQL database for this project.
}
\usage{
sql_import(statement, dbname = "GLOBALIO", outname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{statement, dbname = "GLOBALIO", outname}{
%%     ~~Describe \code{x} here~~
}
}
\details{
"statement" is the SQL statement to be put through (character); "dbname" is the SQL database from which to import data (default to GLOBALIO); "outname" is the name of the object to which the SQL dataframe is assigned.
}

\author{
Jun Shepard
}
