UI <- function( id, 
                choose_data = TRUE, 
                container=visionRContainer(),
                insert_code = FALSE,
                disable_filters=F
                ) {
  
  ns <- NS(id)
  
  box_title <- tags$div(
    class="gadget-title visionR-title-box",
    tags$h1(shiny::icon("drafting-compass"), "Highchart Builder", class = "visionR-title"),
    tags$div(
      class = "pull-right",
      miniTitleBarButton(inputId = ns("close"), label = "Close")
    ),
      tags$div(
        class = "pull-left",
        chooseDataUI(id = ns("choose-data"), class = "btn-sm")
      )
    
  )
  
  output <- miniPage(
    includeCSS("/home/rupatel/working_dir/projects/QuickHighChart/Modules/style.css"),
    # singleton(x = tagList(
    #   tags$link(rel="stylesheet", type="text/css", href="/home/rupatel/working_dir/projects/QuickHighChart/Modules/style.css")
    # )),
    box_title,
    # page
    layout(
     top =   actionGroupButtons(
        inputIds = c("Bar", "Histogram", "Scatter", "Line","Box"),
        labels = list("Bar", "Histogram", "Scatter","Line","Box"),
        status = "danger",
        fullwidth = T
      ),
     main = htmltools::tags$div(
       style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
       tags$div(
         style = "position: absolute; right: 0; top: 10px; font-weight: bold; z-index: 1000;"
       ),
       shiny::plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
     )
    ),
    
    optionUI(
      id = ns("controls"), 
      insert_code = insert_code,
      disable_filters = disable_filters
    )
  )
  
  if (is.function(container)) {
    output <- container(output)
  }
  return(output)
}