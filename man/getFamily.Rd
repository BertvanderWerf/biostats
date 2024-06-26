% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getFamily.R
\name{getFamily}
\alias{getFamily}
\title{getFamily}
\usage{
getFamily(fit)
}
\arguments{
\item{fit}{a fitted model}
}
\value{
the family of the model
}
\description{
getFamily
}
\examples{
fit <- glm(count ~ gender, family=binomial, data=data)
getFamily(fit)
}
