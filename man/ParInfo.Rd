% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ParInfo.R
\name{ParInfo}
\alias{ParInfo}
\title{ParInfo}
\usage{
ParInfo(
  par,
  min = NULL,
  max = NULL,
  cond = NULL,
  fact = NULL,
  data = NULL,
  link = NULL,
  sep = "."
)
}
\arguments{
\item{par}{a named vector or list with parameter values}

\item{min}{a vector or list corresponding to parameter par with minimum values for the parameters}

\item{max}{a vector or list corresponding to parameter par with maximum values for the parameters}

\item{cond}{a vector or list corresponding to parameter par indicating with 1 estimate or 0 do not estimate parameter}

\item{fact}{a vecor or list with right sided formula indicating the dependency of the parameter}

\item{data}{a data structure containing the variables used in fact}

\item{link}{a vector or list with the link-function names for each corresponding parameter}

\item{sep}{a separator between parameter name and subgroup}
}
\value{
a list with matching values and with the model matrices in the element mat
}
\description{
ParInfo prepares the parameter info needed for the lnLik() function
}
\examples{
info <- ParInfo(c(k=4, lambda=60, c=0), cond=c(1,1,0), fact=list(~Sex, ~Ethnicity, NULL))
}
