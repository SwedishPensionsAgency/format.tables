#' read data from format.tables format
#' 
#' @description Read data from format.table format which is a special formatted csv
#' 
#' @param file the name of the file which the data are to be read from, for more information see \code{\link{read.table}}
#' @param has.column.names does the table contain a row with column names that are of a different type than the data?
#' @param convert.data should the data be processed with \code{\link{type.convert}}?
#' @param dec decimal separator, used for type conversion
#' @param sep the field separator character. Values on each line of the file are separated by this character, default is ","
#' @param na.strings passed to \code{\link{read.table}}, default "NA" and blanks ("") are always treated as NAs
#' @param ... parameters passed to read.table
#' 
#' @name read
NULL