format.tables
=============

Formatting tables with R for LaTeX and HTML

## Installation

Use `devtools` for easy installation:

    devtools::install_github('format.tables', 'SwedishPensionsAgency')
    
## Usage 
    
    x <- format.tables(data=head(cars, 3), 
                        styles=rep("plain", 3), 
                        column.names = names(cars), 
                        names.style = "header", 
                        header=list(caption = "Caption", subcaption="subcaption")
                        )
    cat(x$render(type="tex")
    


