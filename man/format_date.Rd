% Generated by roxygen2 (4.0.2): do not edit by hand
\name{format_date}
\alias{format_date}
\title{Format date variables in a data frame}
\usage{
format_date(df_2format, pattern = "date", orders = "dmy", year_max = NULL,
  as_date = FALSE, excel = FALSE)
}
\arguments{
\item{df_2format}{A data frame}

\item{pattern}{Vector of strings to match the name of date variables}

\item{orders}{a character vector of date-time formats.
  Each order string is series of formatting characters as
  listed \code{\link[base]{strptime}} but might not include
  the "%" prefix, for example "ymd" will match all the
  possible dates in year, month, day order.  Formatting
  orders might include arbitrary separators. These are
  discarded.  See details for implemented formats.}

\item{year_max}{Numeric. If a year exceeds this value, one century is subtracted to it.}

\item{as_date}{Logical. If \code{TRUE} dates are converted to class \code{"Date"}.}

\item{excel}{Logical. If the orifginal file is an excel file put to \code{TRUE} and dates will be converted from Excel numbering to class \code{"Date"}.}
}
\value{
A data frame with dates formatted.
}
\description{
This functions detects all date variables matching \code{pattern} and then use \code{\link[lubridate]{parse_date_time}} with specified \code{orders} to parse them as POSIXct date-time objects.
}
\note{
The \code{year_max} arguments is to handle the fact that date origin in \R is "1970-01-01" so dates like "08-05-45" will be parsed as "2045-05-08" instead of "1945-05-08"
}

