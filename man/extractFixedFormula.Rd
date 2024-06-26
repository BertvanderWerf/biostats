% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractFixedFormula.R
\name{extractFixedFormula}
\alias{extractFixedFormula}
\title{extractFixedFormula}
\usage{
extractFixedFormula(fit, keep.response = FALSE)
}
\arguments{
\item{fit}{a fitted model}

\item{keep.response}{Keep the response in the resulting object?}
}
\value{
returns a formula with random terms dropped
}
\description{
extractFixedFormula
}
\examples{
fit <- lmer(count ~ gender*Age + (1|hospital), data=data)
extractFixedFormula(fit)
}
