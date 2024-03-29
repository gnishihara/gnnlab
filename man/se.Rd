% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/se.R
\name{se}
\alias{se}
\title{Calculate the standard error of the values in x.
If na.rm is TRUE then the missing values are removed before
calculating the error (default).}
\usage{
se(x, na.rm = TRUE)
}
\arguments{
\item{x}{a numeric vector}

\item{na.rm}{logical. Should missing values be removed? Default na.rm = TRUE.}
}
\value{
a vector containing the width and height
}
\description{
Calculate the standard error of the values in x.
If na.rm is TRUE then the missing values are removed before
calculating the error (default).
}
\examples{
se(c(4,1,2,3,4))
}
