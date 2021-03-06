% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scallop_est.R
\name{scallop_est}
\alias{scallop_est}
\title{scalllop_est}
\usage{
scallop_est(scal_catch, beds, Q, abundance = TRUE, boot = TRUE)
}
\arguments{
\item{boot}{}
}
\description{
An estimate of scallop abundance (numbers or weight) with bootstrap or lognormal CIs can be estimated.
}
\examples{
abund_est <- scallop_est(scal_catch, beds, 0.83, abundance = TRUE, boot = TRUE)
biom_est <- scallop_est(scal_catch, beds, 0.83, abundance = False, boot = TRUE)
}
