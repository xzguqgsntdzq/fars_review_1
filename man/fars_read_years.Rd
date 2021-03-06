% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{fars_read_years read specific reads from a file}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{1 or several years to be selected, class: integer}
}
\value{
data from correct file as tibble dataframe
}
\description{
This function creates data using above defined functions make_filename (for selected year) and fars_read (reads the selected filename with correct year). Selects the defined year from data.
Usually used as a helper -function.
}
\note{
Rises an error if given year does not exists in files.
}
