xy_options <- function(ns) {
  
  tagList(
   
    pickerInput(
      inputId = ns("x_label"),
      label = "X Axes Input:",
      choices = '',
      selected = "minimal",
      options = list(size = 10),
      width = "100%"
    ),
    tags$script(
      paste0("$('#", ns("x_label"), "').addClass('dropup');")
    ),
    
    pickerInput(
      inputId = ns("y_label"),
      label = "Y Axes Input:",
      choices = '',
      selected = "minimal",
      options = list(size = 10),
      width = "100%"
    ),
    tags$script(
      paste0("$('#", ns("y_label"), "').addClass('dropup');")
    )
    
  )
  
}






aesthetics_options <- function(ns) {
  
 
}



plot_options <- function(ns) {
  themes <- list(
    'Fivethirtyeight', 'Economist' ,'Financial Times',
    'Dotabuff' ,'Flat','Simple',
    'Elementary','Google','Firefox','Monokai',
    'Tufte','Sparkline',
    'Grid Light', 'Sand Signika' ,'Dark Unica' , 
    'Chalk' , 'Hand Drawn' 
  )
  
  
  tagList(
    tags$div(
      id = ns("controls-spectrum"), style = "display: block;",
      spectrumInput(
        inputId = ns("fill_color"),
        label = "Choose a color:",
        choices = c(list,(c("#0C4C8A", "#EF562D"))), 
        width = "100%"
      )
    ),
    
    pickerInput(
      inputId = ns("theme"),
      label = "X Axes Input:",
      choices = themes,
      selected = "minimal",
      options = list(size = 10),
      width = "100%"
    ),
    tags$script(
      paste0("$('#", ns("theme"), "').addClass('dropup');")
    ),
    radioGroupButtons(
      inputId = ns("legend_position"), 
      label = "Legend position:",
      choiceNames = list(
        icon("arrow-left"), icon("arrow-up"),
        icon("arrow-down"), icon("arrow-right"), icon("close")
      ),
      choiceValues = c("left", "top", "bottom", "right", "none"),
      selected = "right",
      justified = TRUE, 
      size = "sm"
    )
  )
 
}



code_options <- function(ns, insert_code = FALSE) {
  
  tagList(
    tags$button(
      class = "btn btn-default btn-xs pull-right btn-copy-code",
      "Copy to clipboard", `data-clipboard-target` = paste0("#", ns("codeggplot"))
    ), tags$script("new Clipboard('.btn-copy-code');"),
    tags$br(),
    tags$b("Code:"),
    uiOutput(outputId = ns("code")),
    tags$textarea(id = ns("holderCode"), style = "display: none;"),
    if (insert_code) {
      actionLink(
        inputId = ns("insert_code"),
        label = "Insert code in script",
        icon = icon("arrow-circle-left")
      )
    },
    tags$br(),
    tags$b("Export:"),
    tags$br(),
    tags$div(
      class = "btn-group btn-group-justified",
      downloadButton(
        outputId = ns("export_png"), 
        label = ".png",
        class = "btn-primary btn-xs"
      ),
      downloadButton(
        outputId = ns("export_ppt"), 
        label = ".pptx",
        class = "btn-primary btn-xs"
      )
    )
  )
}
