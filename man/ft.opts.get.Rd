\name{ft.opts.get}
\alias{ft.opts.get}
\title{Get Option for format.tables}
\usage{
ft.opts.get(option, domain = c("read", "write", "render"), default = NULL)
}
\arguments{
  \item{option}{a character vector of length one with the
  name of the option. Can be \code{$} separated, e.g.
  "read$dec", used if \code{domain} is NULL.}

  \item{domain}{a character vector of length one which
  indicates where to look for \code{option}, default is
  "read"}

  \item{default}{if the specified option is not set in the
  options list, this value is returned}
}
\description{
Gets a global option for format.tables
}
\examples{
\dontrun{
ft.opts.get(option = "sep", domain = "read", default = ".")
ft.opts.get(option = "read$sep", domain = NULL, default = ".")}
}

