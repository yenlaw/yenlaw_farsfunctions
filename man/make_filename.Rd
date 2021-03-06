% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Create a filename for a given year with format "accident_YEAR.csv.bz2"}
\usage{
make_filename(year)
}
\arguments{
\item{year}{of file creation as integer / string.}
}
\value{
This function returns a character vector  the format "accident_YEAR.csv.bz2"
}
\description{
This function print a character vector with the filename combining the default
file name and the \code{year} passed as argument to the function
}
\examples{
\dontrun{
make_filename(2017)
}

}
