% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_alec.R
\name{read_alec}
\alias{read_alec}
\title{Read ALEC instruments data}
\usage{
read_alec(filename, ...)
}
\arguments{
\item{filename}{the name of the file which the data are to be read from. It can be a CSV file or an XLSX file.}

\item{...}{further arguments passed to read_csv or read_xlsx}
}
\value{
A tibble containing the data.
The contents of the tibble depend on the data file.

\strong{Returned variables}
\describe{
   \item{datetime}{Date-time}
   \item{speed}{Current speed (cm / s)}
   \item{dir}{Current direction (radians)}
   \item{ew}{East-West velocity (cm / s)}
   \item{ns}{North-South velocity (cm / s)}
   \item{chla}{Chlorophyll-a (μg / L)}
   \item{turbidity}{Turbidity (FTU, Formazin Nephelometric Units)}
   \item{temperature}{Water temperature (\eqn{^\circ}{degrees}C)}
}
}
\description{
This function can read the CEM and CKU data files.
}
\details{
For CEM files, when the absolute x- and y- velocities are below 0 or above 200, all data are NA.

For CKU files, when the chlorophyll data is below 0 or above 400, the value is set to NA.
When turbidity is below 0 or above 1000, the value is set to NA.

For both CEM and CKU files, When the temperature is below 0 or above 40, all data are NA.
}
\examples{
\dontrun{
fnames = dir("~/Lab_Data/kawatea/", full.names=TRUE, recursive=TRUE)
df = tibble(fnames = fnames)#'
df \%>\% filter(str_detect(fnames, "CEM")) \%>\% slice(1) \%>\% pull(fnames) \%>\% read_alec()
df \%>\% filter(str_detect(fnames, "CKU")) \%>\% slice(1) \%>\% pull(fnames) \%>\% read_alec()
}

}
