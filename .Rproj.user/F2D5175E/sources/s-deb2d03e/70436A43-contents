library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

ui <- miniPage(
  
    
    # style sheet
    singleton(x = tagList(
      tags$link(rel="stylesheet", type="text/css", href="esquisse/styles.css"),
      tags$script(src = "esquisse/clipboard/clipboard.min.js")
    )),
    
    if (isTRUE(header)) box_title,
    # page
    layoutAddin(
      top_left = htmltools::tagList(
        htmltools::tags$div(
          style = if (isTRUE(choose_data) & !isTRUE(header)) "padding: 10px;" else "padding: 8px; height: 108%;",
          dropInput(
            inputId = ns("geom"),
            choicesNames = geomIcons()$names, 
            choicesValues = geomIcons()$values,
            dropWidth = "290px",
            width = "100%"
          ),
          if (isTRUE(choose_data) & !isTRUE(header)) chooseDataUI(id = ns("choose-data"))
        )
      ),
      top_right = dragulaInput(
        inputId = ns("dragvars"), 
        sourceLabel = "Variables", 
        targetsLabels = c("X", "Y", "Fill", "Color", "Size", "Group", "Facet"), 
        targetsIds = c("xvar", "yvar", "fill", "color", "size", "group", "facet"),
        choices = "",
        badge = FALSE, 
        width = "100%", 
        height = "100%",
        replace = TRUE
      ),
      main = htmltools::tags$div(
        style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
        tags$div(
          style = "position: absolute; right: 0; top: 10px; font-weight: bold; z-index: 1000;",
          prettyToggle(
            inputId = ns("play_plot"), 
            value = TRUE,
            label_on = "Play",
            label_off = "Pause",
            outline = TRUE,
            plain = TRUE,
            bigger = TRUE, 
            inline = TRUE,
            icon_on = icon("play-circle-o", class = "fa-2x"),
            icon_off = icon("pause-circle-o", class = "fa-2x")
          )
        ),
        shiny::plotOutput(outputId = ns("plooooooot"), width = "100%", height = "100%")
      )
    ),
    
    chartControlsUI(
      id = ns("controls"), 
      insert_code = insert_code,
      disable_filters = disable_filters
    )
  )

server <- function(input, output, session) {

  

}

runGadget(shinyApp(ui, server), viewer = paneViewer())