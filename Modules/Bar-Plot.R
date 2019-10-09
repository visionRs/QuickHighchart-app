#' @title Bar Plot Code
#' @description Helper function to generate bar plot based on different conditions.
#' @param data data.frame - Passed from shiny enviornment
#' @param x character- This is x variable for aesthitic, supplied from shiny enviornment.
#' @param y character- This is y variable for aesthitic, supplied from shiny enviornment.
#' @param colorby character - Parameter used to colorby plot. (defaults to None).
#' @param Theme character- parameter to change plot theme. (defaults to None).
#' @param fontsize numeric- This parameter controls fontsize for x-y titles, x-y ticks labels. (defaults to 10).
#' @param legendPos character- Adjust Legend position (defaults to right)
#' @param title_x character- Set x axis title.
#' @param title_y character- Set y axis title.
#' @param plotTitle character- Set plot title.
#' @return returns H2OFrame
#' @export



#1 PLOTS CODE: --------------------------------

#1.1 PLOTS CODE: Bar Plot---------------------

bar_plot <- function(data=NULL,
                     df_name=NULL,
                     x=NULL,
                     y=NULL,
                     theme=NULL,
                     coordflip=F
                     ) {
  
  
  if (is.null(data))
    return(expr(hchart()))
  
  
  mapping <- list(x=x,y=y)
  
  
  if (rlang::is_call(mapping)) 
    mapping <- eval(mapping)
  
  mapping <- mapping[!vapply(mapping, is.null, FUN.VALUE = logical(1))]

  syms2 <- function(x) {
    lapply(
      X = x,
      FUN = function(y) {
        if (inherits(y, "AsIs")) {
          as.character(y)
        } else {
          sym(as_name(y))
        }
      }
    )
  }
  
  hcaes_mappings <- expr(hcaes(!!!syms2(mapping)))
  
  df_name <- expr(!!sym(df_name) )
  
  # Selecting column vs bar based on user input
  if(coordflip){ 
    hccall <- expr(hchart(. , type='bar' , !!hcaes_mappings)) } 
  else {
    hccall <- expr(hchart(. , type='column' , !!hcaes_mappings))
  }
  
  
  hccall <- expr(!!df_name %>% !!hccall)
  
  
  if (!is.null(theme)) {
    theme <- tolower(theme)
    theme <- paste0("hc_theme_",theme)
    theme <- expr(hc_add_theme((!!sym(theme))()))
    hccall <- expr(!!hccall %>% !!theme)
 
    
  }
 
  return(hccall)
  
} #function ends here
