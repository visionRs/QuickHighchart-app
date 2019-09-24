chooseDataUI <- function(id, label = "Data", icon = "database", ...) {
  
  ns <- NS(id)
  
  if (is.character(icon))
    icon <- icon(icon)
  
  tagList(
    actionButton(
      inputId = ns("changeData"), label = label,
      icon = icon, width = "100%", ...
    )
  )
}