% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.matrix.list.R
\name{as.matrix.list}
\alias{as.matrix.list}
\title{as.matrix.list}
\usage{
\method{as.matrix}{list}(l, equal = TRUE)
}
\arguments{
\item{l}{a list of vectors}

\item{equal}{a boolean indicating if the vectors in the list should have equal lengths}
}
\value{
a matrix with the vectors in each column, if equal==FALSE the lengths will be adjusted with NA's
}
\description{
as.matrix.list
}
\examples{
l <- list(a=c(1,2,3), b=1:10)
as.matrix(l, equal=FALSE)
}
