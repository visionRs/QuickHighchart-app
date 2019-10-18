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

.bar_plot <- function(data=NULL,
                     df_name=NULL,
                     x='None',
                     y='None',
                     group='None',
                     theme=NULL,
                     coordflip=NULL,
                     legendPos='center',
                     legendVerticalAlign='bottom',
                     legendLayout='horizontal',
                     legendx='0',
                     legendy='0',
                     title_text=NULL,
                     title_margin = 15,
                     title_align='center',
                     title_color='black',
                     title_font_weight="bold",
                     title_useHTML=T,
                     title_text_x =NULL, 
                     title_x_opposite=NULL,
                     title_x_plotline_text=NULL,
                     title_x_plotline_color=NULL,
                     title_x_plotline_width=1,
                     title_x_plotline_value=NULL
                     ) {
  
  #1. Validation: Checking if data is null------------
  if (is.null(data))
    return(expr(hchart()))
 
  #2. Validation: Mapping conditions------------
  
  if(x!='None' & y!='None' & group!='None'){
    mapping <- list(x=x,y=y,group=group)
    
  } else if(x=='None' & y!='None' & group!='None') {
    
    mapping <- list(y=y,group=group)
    
  } else if(x=='None' & y=='None' & group!='None') {
    
    return(expr(hchart()))
    
    
  } else if(x!='None' & y!='None' & group=='None') {
    
    mapping <- list(x=x,y=y)
    
  
  } else if(x!='None' & y=='None' & group!='None') {
    
    mapping <- list(x=x,group=group)
    
  } else if(x=='None' & y!='None' & group=='None') {
  
    mapping <- list(y=y)
    
  } else if(x!='None' & y=='None' & group=='None') {
    mapping <- list(x=x)
    
  
  } else if(x=='None' & y=='None' & group=='None') {
    return(expr(hchart()))
    
  }
  
  if (rlang::is_call(mapping)) 
    mapping <- eval(mapping)
  
  mapping <- mapping[!vapply(mapping, is.null, FUN.VALUE = logical(1))]

  #4. Define Sym2 function---------------
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
 
  #5. Selecting column vs bar based on user input-------------
  if(coordflip=='Bar'){ 
    hccall <- expr(hchart(. , type='bar' , !!hcaes_mappings)) } 
  else {
    hccall <- expr(hchart(. , type='column' , !!hcaes_mappings))
  }
  hccall <- expr(!!df_name %>% !!hccall)
  
  
  #6.Legend Options---------------
  legend <-expr(hc_legend(align = !!paste0(legendPos), verticalAlign = !!paste0(legendVerticalAlign), layout= !!paste0(legendLayout) , x = !!as.numeric(legendx), y = !!as.numeric(legendy)))
  hccall <- expr(!!hccall %>% !!legend)
  
  
  #7.Title Options--------------
  if(!is.null(title_text)){
    
  title <- expr(hc_title(text=!!paste0(title_text),
                         margin=!!as.numeric(title_margin),
                         align=!!paste0(title_align),
                         style=list(color=!!paste0(title_color),
                                    fontWeight=!!paste0(title_font_weight),
                                    useHTML=!!as.logical(title_useHTML))))
  hccall <- expr(!!hccall %>% !!title)
  
  }
  
  
  
  #8. X & Y Axis Options---------
  plotline_vars <- c(title_text_x, title_x_opposite,
                     title_x_plotline_color,title_x_plotline_width,
                     title_x_plotline_value,title_x_plotline_text)

  if(!any(is.null(plotline_vars))){

    hc_x_axis <- expr(hc_xAxis(title=list(text=!!paste0(title_text_x)),
                               opposite = !!as.logical(title_x_opposite),
                               plotLines = list(
                                 list(label = list(text = !!paste0(title_x_plotline_text)),
                                      color = !!paste0(title_x_plotline_color),
                                      width = !!paste0(title_x_plotline_width),
                                      value = !!paste0(title_x_plotline_value)
                                      )
                                 )
                               )
                      )

    hccall <- expr(!!hccall %>% !!hc_x_axis)

  }

  
  #8.Theme Options-----------
  if (!is.null(theme)) {
    theme <- tolower(theme)
    theme <- paste0("hc_theme_",theme)
    theme <- expr(hc_add_theme((!!sym(theme))()))
    hccall <- expr(!!hccall %>% !!theme)
 
    
  }
 
  #9. Evaluating Final Code------------
  plot <- rlang::eval_tidy(hccall)
  
  #10. Parsing Final Code-----------
  code <-  expr_deparse(hccall, width = 1e4)
  code <- stri_replace_all(str = code, replacement = "%>%\n", fixed = "%>%")
  ls <- list()
  ls[['plot']] <- plot
  ls[['code']] <- code
  
  return(ls)

  
  #return(hccall)
  
  
} #function ends here
