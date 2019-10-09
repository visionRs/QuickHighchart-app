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
  
  
  observeEvent(c(input$x_label,input$y_label), {
    dt <- dataChart$data
    
    if((!is.numeric(dt[[input$x_label]]) & !is.numeric(dt[[input$y_label]])) | (is.null(dt[[input$x_label]]) & is.null(dt[[input$y_label]])) | is.null(dt[[input$x_label]])){
      shinyjs::disable("Bar")
      shinyjs::disable("Histogram")
      shinyjs::disable("Scatter")
      shinyjs::disable("Line")
      shinyjs::disable("Box")
      
      
    } else if((is.null(dt[[input$y_label]]) & is.numeric(dt[[input$x_label]]))) {
      
      shinyjs::disable("Bar")
      shinyjs::disable("Scatter")
      shinyjs::disable("Line")
      shinyjs::disable("Box")
      shinyjs::enable("Histogram")
      
      
    } else if(!is.numeric(dt[[input$x_label]]) | !is.numeric(dt[[input$y_label]])) {
      
      shinyjs::enable("Bar")
      shinyjs::disable("Scatter")
      shinyjs::enable("Line")
      shinyjs::disable("Histogram")
      shinyjs::enable("Box")
      
      
    } else {
      
      shinyjs::enable("Bar")
      shinyjs::enable("Scatter")
      shinyjs::enable("Line")
      shinyjs::enable("Box")
      shinyjs::disable("Histogram")
      
    }
    
    # update textInputs for renaming axes
    # updateTextInput(session = session,inputId = "titleX", value = input$x_label)
    # updateTextInput(session = session,inputId = "titleY", value = input$y_label)
  })
  
  
  })
  #4 PLOTS CODE: -------------
  
  list_both <- reactiveValues(code = NULL)
  
  
  #___4.0 PLOTS CODE: Bar Plot Code-----------------
  observeEvent(input$Bar,{
    
   
    #______4.0.1 GGPLOT Code--------------------
    
    list_both$code <- bar_plot(data = dataChart$data,
                               df_name = dataChart$name,
                               x=input$x_label,
                               y=input$y_label,
                               theme = input$theme#if(input$theme=='None'){"NULL"} else{input$theme}
                              
    )
    print(list_both$code)
    
    
  })
  
  
  
  
  output$code <- renderUI({
    code <-  expr_deparse(list_both$code, width = 1e4)
    code <- stri_replace_all(str = code, replacement = "%>%\n", fixed = "%>%")
    htmltools::tagList(
      rCodeContainer(id ="codeggplot", code)
    )
  })
  
  
  output$plot <- renderHighchart({
    
    rlang::eval_tidy(list_both$code)
  
  })
  
}