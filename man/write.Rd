\name{write}
\alias{write}
\title{Write data in format.tables format}
\arguments{
  \item{file}{either a character string naming a file or a
  \code{\link{connection}} open for writing. "" indicates
  output to the console}

  \item{dec}{the string to use for decimal points in
  numeric or complex columns: must be a single character.}

  \item{...}{parameters passed to
  \code{\link{write.table}}, \code{row.names} is not
  available as it is set to false by default.}
}
\description{
Write data in format.table format which is a special
formatted csv
}

