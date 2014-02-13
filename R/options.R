#' Set Option for format.tables
#' 
#' @description Set global option for format.tables
#' @param read named list for reading format.tables data from file. The \code{names} attribute gives the argument names.
#' @param write named list for writing format.tables data to file. The \code{names} attribute gives the argument names.
#' @param render named list for rendering format.tables data. The \code{names} attribute gives the argument names.
#' 
#' @examples 
#' \dontrun{
#' ft.opts.set(read = list(sep = "."))}
#' 
#' @export
ft.opts.set <- function(read = NULL, write = NULL, render = NULL){
  opts <- list(read = arguments.merge(.Options$format.tables$read, read), 
               write = arguments.merge(.Options$format.tables$write, write), 
               render = arguments.merge(.Options$format.tables$render, render))
  .Options$format.tables <- opts
  options("format.tables" = opts)
}




#' Get Option for format.tables
#' 
#' @description Gets a global option for format.tables
#' @param option a character vector of length one with the name of the option. Can be \code{$} separated, e.g. "read$dec", used if \code{domain} is NULL.
#' @param domain a character vector of length one which indicates where to look for \code{option}, default is "read"
#' @param default if the specified option is not set in the options list, this value is returned
#' 
#' @examples
#' \dontrun{
#' ft.opts.get(option = "sep", domain = "read", default = ".")
#' ft.opts.get(option = "read$sep", domain = NULL, default = ".")}
#' 
#' @export
ft.opts.get <- function(option, domain = c("read", "write", "render"), default = NULL){
  domain <- match.arg(domain)
  if (!is.null(domain) & !is.na(domain)) {
    opt <- options("format.tables")[["format.tables"]][[domain]][[option]]
  } else {
    opt <- options("format.tables")[["format.tables"]][[option]]
  }
  
  if (is.null(opt)) {
    path <- unlist(strsplit(option, "\\$"))
    opt <- options("format.tables")[["format.tables"]]
    for (i in 1:length(path)) {
      opt <- opt[[ path[[i]] ]]
    }
  }
  if (missing(default) | !is.null(opt)) 
    return(opt)
  else default
}

