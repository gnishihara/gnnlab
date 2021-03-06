% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_encoding.R
\name{get_encoding}
\alias{get_encoding}
\title{Get the encoding of a file
This function will check the encoding of the file and is vectorized.}
\usage{
get_encoding(con)
}
\arguments{
\item{con}{the name of the file to process, which can be a vector.}
}
\value{
Return the encoding of the file.
}
\description{
This is especially important for the CEM and CKU files, since they are often stored in either Shift_JIIS or UTF-8.
This will depend on the computer used to offload the data from the instruments.
}
