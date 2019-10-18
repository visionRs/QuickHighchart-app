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
                    label = "X Axes Input:",
                    inputId = "x_label",
                    selected = 'None',
                    choices =c(var_choices,'None')
                   )
  updatePickerInput(session = session,
                    label = "Y Axes Input:",
                    inputId = "y_label",
                    selected = 'None',
                    choices =c(var_choices,'None')
  )
  
  updatePickerInput(session = session,
                    label = "Group Column:",
                    inputId = "group",
                    selected = 'None',
                    choices =c(var_choices,'None')
  )
  print(input$col_bar_check)
  
  })
  observeEvent(c(input$x_label,input$y_label), {
    dt <- dataChart$data
    
    if((!is.numeric(dt[[input$x_label]]) & !is.numeric(dt[[input$y_label]])) | (is.null(dt[[input$x_label]]) & is.null(dt[[input$y_label]])) ){
      #shinyjs::runjs('$("#radio button:eq(1)").attr("disabled", true);')

      
     # shinyjs::disable(selector = "$('#radio_grp button:eq(3)")
     # shinyjs::disable("Histogram")
     # shinyjs::disable("Scatter")
     # shinyjs::disable("Line")
     # shinyjs::disable("Box")
      
      
      shinyjs::runjs("$('input[value=Bar]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Histogram]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Scatter]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Line]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Box]').parent().attr('disabled', true);")
      
    } else if((is.null(dt[[input$y_label]]) & is.numeric(dt[[input$x_label]]))) {
      
      
      shinyjs::runjs("$('input[value=Bar]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Histogram]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Scatter]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Line]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Box]').parent().attr('disabled', false);")
      
      
      # shinyjs::disable("Bar")
      # shinyjs::disable("Scatter")
      # shinyjs::disable("Line")
      # shinyjs::disable("Box")
      # shinyjs::enable("Histogram")
      
    } else if((is.null(dt[[input$x_label]]) & is.numeric(dt[[input$y_label]]))) {
      
      
      shinyjs::runjs("$('input[value=Bar]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Histogram]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Scatter]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Line]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Box]').parent().attr('disabled', true);")
      
      
    } else if(!is.numeric(dt[[input$x_label]]) | !is.numeric(dt[[input$y_label]])) {
      
      # shinyjs::enable("Bar")
      # shinyjs::disable("Scatter")
      # shinyjs::enable("Line")
      # shinyjs::disable("Histogram")
      # shinyjs::enable("Box")
      # 
      
      shinyjs::runjs("$('input[value=Bar]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Histogram]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Scatter]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Line]').parent().attr('disabled', true);")
      shinyjs::runjs("$('input[value=Box]').parent().attr('disabled', false);")
      
    } else {
      
      # shinyjs::enable("Bar")
      # shinyjs::enable("Scatter")
      # shinyjs::enable("Line")
      # shinyjs::enable("Box")
      # shinyjs::disable("Histogram")
      
      
      shinyjs::runjs("$('input[value=Bar]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Histogram]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Scatter]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Line]').parent().attr('disabled', false);")
      shinyjs::runjs("$('input[value=Box]').parent().attr('disabled', true);")
    
    # update textInputs for renaming axes
    # updateTextInput(session = session,inputId = "titleX", value = input$x_label)
    # updateTextInput(session = session,inputId = "titleY", value = input$y_label)
      
      
    }
  })
  
  
  
  
  output$plot <- renderHighchart({
    dt <- dataChart$data
    if(is.null(dt)){return()}
    switch(input$radio_grp,
           "Bar" =    .bar_plot(data = dt,
                               df_name = dataChart$name,
                               x=input$x_label, 
                               y=input$y_label,
                               group=input$group,
                               coordflip = input$col_bar_check,
                               legendPos=input$legendPos,
                               legendVerticalAlign=input$legendVerticalAlign,
                               legendLayout=input$legendLayout,
                               legendx=input$legendx,
                               legendy=input$legendy,
                               title_text = input$title_text,
                               title_align = input$title_align,
                               title_margin = input$title_margin,
                               title_color = input$title_color,
                               title_useHTML = input$title_useHTML,
                               title_text_x = input$title_text_x,
                               title_x_opposite = input$title_x_opposite,
                               title_x_plotline_color = input$title_x_plotline_color,
                               title_x_plotline_width = input$title_x_plotline_width,
                               title_x_plotline_value = input$title_x_plotline_value,
                               title_x_plotline_text = input$title_x_plotline_text,
                               theme = input$theme)$plot,
           
           "Line"=.line_plot(data = dt,
                            df_name = dataChart$name,
                            x=input$x_label, 
                            y=input$y_label,
                            group=input$group,
                            legendPos=input$legendPos,
                            legendVerticalAlign=input$legendVerticalAlign,
                            legendLayout=input$legendLayout,
                            legendx=input$legendx,
                            legendy=input$legendy,
                            title_text = input$title_text,
                            title_align = input$title_align,
                            title_margin = input$title_margin,
                            title_color = input$title_color,
                            title_useHTML = input$title_useHTML,
                            title_text_x = input$title_text_x,
                            title_x_opposite = input$title_x_opposite,
                            title_x_plotline_color = input$title_x_plotline_color,
                            title_x_plotline_width = input$title_x_plotline_width,
                            title_x_plotline_value = input$title_x_plotline_value,
                            title_x_plotline_text = input$title_x_plotline_text,
                            theme = input$theme)$plot,
           
           "Scatter"=.scatter_plot(data = dt,
                             df_name = dataChart$name,
                             x=input$x_label, 
                             y=input$y_label,
                             group=input$group,
                             legendPos=input$legendPos,
                             legendVerticalAlign=input$legendVerticalAlign,
                             legendLayout=input$legendLayout,
                             legendx=input$legendx,
                             legendy=input$legendy,
                             title_text = input$title_text,
                             title_align = input$title_align,
                             title_margin = input$title_margin,
                             title_color = input$title_color,
                             title_useHTML = input$title_useHTML,
                             title_text_x = input$title_text_x,
                             title_x_opposite = input$title_x_opposite,
                             title_x_plotline_color = input$title_x_plotline_color,
                             title_x_plotline_width = input$title_x_plotline_width,
                             title_x_plotline_value = input$title_x_plotline_value,
                             title_x_plotline_text = input$title_x_plotline_text,
                             theme = input$theme)$plot
           
    )
    
  })
  
  output$code <- renderUI({
    
    dt <- dataChart$data
    if(is.null(dt)){return()}
    
    switch(input$radio_grp,
           "Bar" =  htmltools::tagList(
             rCodeContainer(id ="codeggplot", .bar_plot(data = dt,
                                                       df_name = dataChart$name,
                                                       x=input$x_label, 
                                                       y=input$y_label, 
                                                       group=input$group,
                                                       coordflip = input$col_bar_check,
                                                       legendPos=input$legendPos,
                                                       legendVerticalAlign=input$legendVerticalAlign,
                                                       legendLayout=input$legendLayout,
                                                       legendx=input$legendx,
                                                       legendy=input$legendy,
                                                       title_text = input$title_text,
                                                       title_align = input$title_align,
                                                       title_margin = input$title_margin,
                                                       title_color = input$title_color,
                                                       title_useHTML = input$title_useHTML,
                                                       title_text_x = input$title_text_x,
                                                       title_x_opposite = input$title_x_opposite,
                                                       title_x_plotline_color = input$title_x_plotline_color,
                                                       title_x_plotline_width = input$title_x_plotline_width,
                                                       title_x_plotline_value = input$title_x_plotline_value,
                                                       title_x_plotline_text = input$title_x_plotline_text,
                                                       
                                                       theme = input$theme)$code)
           ) ,
           "Line"=htmltools::tagList(
             rCodeContainer(id ="codeggplot", .line_plot(data = dt,
                                                        df_name = dataChart$name,
                                                        x=input$x_label, 
                                                        y=input$y_label, 
                                                        group=input$group,
                                                        legendPos=input$legendPos,
                                                        legendVerticalAlign=input$legendVerticalAlign,
                                                        legendLayout=input$legendLayout,
                                                        legendx=input$legendx,
                                                        legendy=input$legendy,
                                                        title_text = input$title_text,
                                                        title_align = input$title_align,
                                                        title_margin = input$title_margin,
                                                        title_color = input$title_color,
                                                        title_useHTML = input$title_useHTML,
                                                        title_text_x = input$title_text_x,
                                                        title_x_opposite = input$title_x_opposite,
                                                        title_x_plotline_color = input$title_x_plotline_color,
                                                        title_x_plotline_width = input$title_x_plotline_width,
                                                        title_x_plotline_value = input$title_x_plotline_value,
                                                        title_x_plotline_text = input$title_x_plotline_text,
                                                        theme = input$theme)$code)
           ) ,
           
           "Scatter"=htmltools::tagList(
             rCodeContainer(id ="codeggplot", .scatter_plot(data = dt,
                                                         df_name = dataChart$name,
                                                         x=input$x_label, 
                                                         y=input$y_label, 
                                                         group=input$group,
                                                         legendPos=input$legendPos,
                                                         legendVerticalAlign=input$legendVerticalAlign,
                                                         legendLayout=input$legendLayout,
                                                         legendx=input$legendx,
                                                         legendy=input$legendy,
                                                         title_text = input$title_text,
                                                         title_align = input$title_align,
                                                         title_margin = input$title_margin,
                                                         title_color = input$title_color,
                                                         title_useHTML = input$title_useHTML,
                                                         title_text_x = input$title_text_x,
                                                         title_x_opposite = input$title_x_opposite,
                                                         title_x_plotline_color = input$title_x_plotline_color,
                                                         title_x_plotline_width = input$title_x_plotline_width,
                                                         title_x_plotline_value = input$title_x_plotline_value,
                                                         title_x_plotline_text = input$title_x_plotline_text,
                                                         theme = input$theme)$code)
           ) 
           
    )
    
    
    
  })

  
  
  #4 PLOTS CODE: -------------
  
  # list_both <- reactiveValues(code = NULL)
  
  # 
  # bar_input_val <- reactiveValues(x=NULL,
  #                                 y=NULL,
  #                                 theme = NULL)
  # 

  
  #___4.0 PLOTS CODE: Bar Plot Code-----------------
  # observeEvent(input$Bar,{
  #   
  #  
  #   #______4.0.1 GGPLOT Code--------------------
  #   bar_input_val$x=input$x_label
  #   bar_input_val$y=input$y_label
  #   bar_input_val$theme=input$theme
  #   
  #   
  #   list_both$code <- bar_plot(data = dataChart$data,
  #                              df_name = dataChart$name,
  #                              x=bar_input_val$x,
  #                              y=bar_input_val$y,
  #                              theme=bar_input_val$theme
  #                              # x=input$x_label,
  #                              # y=input$y_label,
  #                              # theme = input$theme
  #                             
  #   )
  #   print(list_both$code)
  #   
  #   
  # })
  
  
  
  
  # output$code <- renderUI({
  #   code <-  expr_deparse(list_both$code, width = 1e4)
  #   code <- stri_replace_all(str = code, replacement = "%>%\n", fixed = "%>%")
  #   htmltools::tagList(
  #     rCodeContainer(id ="codeggplot", code)
  #   )
  # })
  # 
  
  # output$plot <- renderHighchart({
  #   
  #   rlang::eval_tidy(list_both$code)
  # 
  # })
  # 
}