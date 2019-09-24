optionUI <- function(id, insert_code = FALSE, disable_filters = FALSE) {
  
  # Namespace
  ns <- NS(id)
  
  # ui
  tags$div(
    class = "btn-group-charter btn-group-justified-charter",
    tags$style(sprintf(
      "#%s .sw-dropdown-in {margin: 8px 0 8px 10px !important; padding: 0 !important;}",
      "sw-content-filterdrop"
    )),
    dropdown(
      #controls_labs(ns),
      inputId = "Aesthetics",
      style = "default",
      label = "Aesthetics", 
      up = TRUE, 
      icon = icon("font"), 
      status = "default btn-controls"
    ),
    dropdown(
      #controls_params(ns), controls_appearance(ns),
      style = "default",
      label = "Plot options",
      up = TRUE, 
      inputId = "paramsdrop",
      icon = icon("gears"), 
      status = "default btn-controls"
    ),
    dropdown(
      #controls_code(ns, insert_code = insert_code), 
      style = "default", 
      label = "Code Options", 
      up = TRUE,
      right = TRUE, 
      inputId = "codedrop",
      icon = icon("code"), 
      status = "default btn-controls"
    ),
    tags$script("$('.sw-dropdown').addClass('btn-group-charter');"),
    tags$script(HTML("$('.sw-dropdown > .btn').addClass('btn-charter');")),
    tags$script("$('#sw-content-codedrop').css('min-width', '350px');"),
    tags$script("$('#sw-content-paramsdrop').css('min-width', '330px');")
  )
}
