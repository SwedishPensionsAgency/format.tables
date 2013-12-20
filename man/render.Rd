\name{render}
\alias{render}
\title{Renders table with a template}
\arguments{
  \item{table.template}{path to the template for the table
  containing whisker tags, if not provided}

  \item{row.template}{path to the template for rows
  containing whisker tags}

  \item{type}{"latex" or "html" supported, in "latex" cells
  are collapsed with " & ", in "html" with ""}

  \item{collapse}{string for collapsing cells, overrides
  the standards used in the type parameter}

  \item{sep}{the field separator character, for more
  information see \code{\link{read.table}}}

  \item{...}{passed to \code{\link{format}}, e.g.
  decimal.mark, big.mark}
}
\description{
Renders table with a template
}
\details{
In your \link[whisker:whisker-package]{whisker} template
you can use the following tags, e. g. \{\{id\}\}:

Table template: \itemize{ \item{e. g. \code{caption}:
}{tags named in the header variable of your object}
\item{\code{id}: }{if there is a field 'id' in the header,
it is used for the id, otherwise a random character string
is generated with the prefix "tbl_", should be unique if
you like to have table notes} \item{\code{tableRows}:
}{list with all rows that is iterated over, use
\{\{#tableRows\}\} ... \{\{/tableRows\}\} in the template,
contains: \itemize{ \item{\code{tableRow}: }{contains the
whole row which is rendered with the data and the row
template} \item{\code{style}: }{contains the row style}
\item{e. g. \code{plain-0}: }{if you want to use the style
as a switch use the row style as a tag} \item{\code{id}:
}{for the table id is also provided in each row} } }

}

Row template: \itemize{ \item{\code{value}: }{raw value
without any formatting, NAs will show upp as the string
"NA" } \item{\code{formated}: }{the value formated with
\code{\link{format}} and the parameters provided in
\code{...}, NAs are replaced with empty strings}
\item{\code{ncol}: }{number of column of the table}
\item{\code{colNumber}: }{the present column number for
each cell} }

Note templae: \itemize{ \item{\code{tableNote}: }{contains
the note for the whole table} \item{\code{rowNotes}: }{list
with the notes for rows, use \{\{#rowNotes\}\} ...
\{\{/rowNotes\}\} in the template, contains: \itemize{
\item{\code{note}: }{the note} \item{\code{number}:
}{number of the corresponding note} \item{\code{id}: }{for
the table id is also provided for each note} } } }
}

