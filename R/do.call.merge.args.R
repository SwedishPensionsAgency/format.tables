#' Execute a Function Call with Merged Arguments 
#' @description Constructs and executes a function call from a name or a function and a one or more lists/vectors of arguments that are merged prior to be passed to the funtion. 
#' 
#' @param FUN function to call
#' @param ... named vectors or lists with arguments that should be merged together and used in \code{\link{do.call}}, the last item of \code{...} has the highest priority. The \code{names} attribute of \code{...} gives the argument names.
#' @examples 
#' \dontrun{
#' data <- data.frame(a = c(1:10)/2, b = LETTERS[1:10])
#' do.call.merge.args(FUN = write.table, list(x = data, sep = ";", dec = ".", row.names = FALSE), c(dec = ","))
#' 
#' do.call.merge.args(FUN = sqrt, c(x=2), list(x=3)) }
#' 
#' @export
do.call.merge.args <- function(FUN, ...){
  # order of argument use
  # 1. default argument of function
  default.args <- formals(FUN)
  args <- default.args[!names(default.args) %in% c("", "...")]
  
  if(is.null(args))
    args <- list()
  
  # 2. arguments passed to ... in order of appereance
  argument.list <- list(...)
  
  for (i in 1:length(argument.list)){
    if(!"list" %in% class(argument.list[[i]])){
      argument.list[[i]] <- as.list(argument.list[[i]])
    }
    args <- arguments.merge(args, argument.list[[i]], FUN = FUN, append = ifelse(is.primitive(FUN), 
                                                                                 TRUE, 
                                                                                 "..." %in% names(formals(FUN))))
  }
  
  # call the function with the merged arguments and return the result
  # args can contain names of arguments in args as values that should be evaluated; hence envir = list2env(args) is needed
  return(do.call(FUN, args, envir = list2env(args)))
}
