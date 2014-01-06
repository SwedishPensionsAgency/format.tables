\name{ft.write}
\alias{ft.write}
\title{Write data in format.tables format}
\usage{
ft.write(data, file = "", dec = ".", ...)
}
\arguments{
  \item{data}{data in ft.data format, containing tabular
  data in a data frame, column names, styles, notes, and
  header}

  \item{file}{either a character string naming a file or a
  \code{\link{connection}} open for writing. "" indicates
  output to the console}

  \item{dec}{the string to use for decimal points in
  numeric or complex columns: must be a single character.}

  \item{...}{parameters passed to
  \code{\link{write.table}}}
}
\description{
Write data in format.table format which is a special
formatted csv
}

