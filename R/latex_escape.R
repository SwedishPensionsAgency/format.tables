#' Escaping for LaTeX
#' 
#' Escaping \code{{}[]\%} in character vectors, lists, data frames, and data tables for use in LaTeX
#' 
#' 
#' @param x character vector, list, data frame, or data table
#' 
#' @import data.table
#' @export
latex_escape <- function(x){
  out <- lapply(x, function(x){
    if (!is.numeric(x)) {
      out <- gsub("\\{", "\\\\{", x)
      out <- gsub("\\}", "\\\\}", out)
      out <- gsub("\\[", "\\\\[", out)
      out <- gsub("\\]", "\\\\]", out)
      out <- gsub("\\%", "\\\\%", out)
    } else {
      out <- x
    }
    
    return(out)
  })
  if ("data.table" %in% class(x)) {
    out <- data.table::as.data.table(out)
  } else if (is.data.frame(x)) {
    out <- as.data.frame(out)
  } else if (class(x) == "list") {
    out <- out
  } else {
    out <- unlist(out)
  }
  return(out)
}