\name{ft.opts.set}
\alias{ft.opts.set}
\title{Set Option for format.tables}
\usage{
ft.opts.set(read = NULL, write = NULL, render = NULL)
}
\arguments{
  \item{read}{named list for reading format.tables data
  from file. The \code{names} attribute gives the argument
  names.}

  \item{write}{named list for writing format.tables data to
  file. The \code{names} attribute gives the argument
  names.}

  \item{render}{named list for rendering format.tables
  data. The \code{names} attribute gives the argument
  names.}
}
\description{
Set global option for format.tables
}
\examples{
\dontrun{
ft.opts.set(read = list(sep = "."))}
}

