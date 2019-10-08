Server <- function(input, output, session, data = NULL, dataModule = c("GlobalEnv", "ImportFile"), sizeDataModule = "m") {
  
  ggplotCall <- reactiveValues(code = "")
  
  observeEvent(data$data, {
    dataChart$data <- data$data
    dataChart$name <- data$name
  }, ignoreInit = FALSE)
  
  dataChart <- callModule(
    module = chooseDataServer, 
    id = "choose-data",
    data = isolate(data$data),
    name = isolate(data$name),
    launchOnStart = F,
    dataModule = dataModule, size = sizeDataModule
  )
  
  
  
  # Close addin
  observeEvent(input$close, shiny::stopApp())
  
  
  
  
  # Update X -Y Axes -----
  observeEvent(dataChart$data, {
  var_choices <- setdiff(names(dataChart$data), attr(dataChart$data, "sf_column"))
  
  updatePickerInput(session = session,
                    label = "",
                    inputId = "x_label",
                    choices =var_choices
                   )
  updatePickerInput(session = session,
                    label = "",
                    inputId = "y_label",
                    choices =var_choices
  )
  
  output$code <- renderUI({
    code <- ggplot_rv$code
    code <- stri_replace_all(str = code, replacement = "+\n", fixed = "+")
    if (!is.null(output_filter$code$expr)) {
      code_dplyr <- deparse(output_filter$code$dplyr, width.cutoff = 80L)
      code_dplyr <- paste(code_dplyr, collapse = "\n")
      nm_dat <- data_name()
      code_dplyr <- paste(nm_dat, code_dplyr, sep = " <- ")
      code_dplyr <- stri_replace_all(str = code_dplyr, replacement = "%>%\n", fixed = "%>%")
      code <- paste(code_dplyr, code, sep = "\n\n")
    }
    htmltools::tagList(
      rCodeContainer(id = ns("codeggplot"), code)
    )
  })
  
  
  output$plot <- renderPlot({
    
    ggplot(data = dataChart$data , aes(x=Sepal.Length, y=Petal.Length,fill = Species)) + 
      geom_bar(stat="identity" , position = 'stack') +  labs(title = "skjd") + 
      xlab("ksadj") +  ylab("ksajd") + 
      theme(axis.text = element_text(size = 7),     
            axis.title.x = element_text(size = 7),
            axis.title.y = element_text(size = 7),
            plot.title = element_text(size = 7),
            legend.position = "right",
            axis.text.x = element_text(angle = 0, hjust = 1)) +
      coord_flip()
    
  })
  
  })
  
}