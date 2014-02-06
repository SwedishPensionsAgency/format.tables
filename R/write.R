#' Write data in format.tables format
#' 
#' @description Write data in format.table format which is a special formatted csv
#' 
#' @param file either a character string naming a file or a  \code{\link{connection}} open for writing. "" indicates output to the console
#' @param dec the string to use for decimal points in numeric or complex columns: must be a single character.
#' @param ... parameters passed to \code{\link{write.table}}, \code{row.names} is not available as it is set to false by default.  
#' 
#' @name write
NULL