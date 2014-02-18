#' Merge two lists of function arguments 
#' @description Merge two lists of function arguments 
#' 
#' @param x base list
#' @param y list to be merged with \code{x}
#' @param FUN function that should take the arguments. If the \code{\link{formals}} of \code{FUN} does not contain \code{...} the default of append is changed.  
#' @param append should elements of \code{y} not contained in \code{x} be appended?
#' 
#' @export
# merge elements of y into x with the same names
# check if there is an "..." in default.arguments
# if true, merge args with options
# if false, merge args with options except elements that are not in args
arguments.merge <- function(
  x, 
  y, 
  FUN = NULL, 
  append = ifelse(is.null(FUN), 
                  TRUE, 
                  "..." %in% names(formals(FUN)))) {
  if (!append)
    y[!names(y) %in% names(x)] <- NULL

  x[names(y)] = y
  return(x)
}