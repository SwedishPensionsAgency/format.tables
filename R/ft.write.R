#' Write data in format.tables format
#' 
#' @description Write data in format.table format which is a special formatted csv
#' 
#' @param data data in ft.data format, containing tabular data in a data frame, column names, styles, notes, and header
#' @param file either a character string naming a file or a  \code{\link{connection}} open for writing. "" indicates output to the console
#' @param dec the string to use for decimal points in numeric or complex columns: must be a single character.
#' @param ... parameters passed to \code{\link{write.table}}
#' 



ft.write <- function(data, file = "", dec= ".", ...){
  
  if(!getPackageName() %in% class(data)){
    stop("data is not of class format.tables, the function supports only format.tables ")
  }
  
  
  export.data <- data$data
  
  for (i in 1:ncol(export.data)){
    if(class(export.data[, i]) != "character" & typeof(export.data[, i]) != "character"){
      export.data[, i] <- format(export.data[, i], 
                                 decimal.mark = dec, 
                                 scientific = FALSE)
    }
  }
  
  #putting together styles, data, and notes
  styles <- data$styles
  notes <- data$notes
  if(!is.null(data$names.style)){
    styles <- c(data$names.style, styles)
    if(!is.null(notes)) notes <- c(ifelse(is.null(data$names.note), NA, data$names.note), notes)
    export.data <- rbind(data$column.names, export.data)
  } 
  styles <- c("styles", styles)
  
  export.data <- rbind(rep(NA, ncol(export.data)), export.data)
  
  export.data <- cbind(styles, export.data, stringsAsFactors = FALSE)
  
  if(!is.null(notes)) export.data <- cbind(export.data, c("notes", notes), stringsAsFactors = FALSE)
  
  
  
  
  
  
  header <- data.frame(keys = names(data$header), values = unlist(data$header))
  

  
  # adding columns to fit the width of the data
  export.header <- cbind(header, 
                         matrix(rep(NA, (ncol(export.data)-ncol(header))*nrow(header)), 
                                ncol =  ncol(export.data)-ncol(header) 
                         ) 
  )
  names(export.data) <- names(export.header)
  
  export.data <- rbind(export.header, rep(NA, ncol(export.header)), export.data)
  
  names(export.data) <- paste0("V", 1:ncol(export.data))
  row.names(export.data) <- 1:nrow(export.data)
  
  if (file == ""){
    print(export.data)
  }else{
    write.table(x = export.data, file = file, row.names = FALSE, ...)
  }
  
}
