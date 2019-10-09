UI <- function( id, 
                choose_data = TRUE, 
                container=visionRContainer(fixed = T),
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
        esquisse::chooseDataUI(id = ns("choose-data"), class = "btn-sm")
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
     top =  radioGroupButtons(inputId = ns('radio_grp'),choices = c('Bar','Scatter','Line','Box'),status='danger',justified = T ),
       
      #  actionGroupButtons(
      #   inputIds = c(ns("Bar"), ns("Histogram"), ns("Scatter"), ns("Line"),ns("Box")),
      #   labels = list("Bar", "Histogram", "Scatter","Line","Box"),
      #   status = "danger",
      #   fullwidth = T
      # ),
     main = #htmltools::tags$div(
     #   style = "margin-top: 10px; padding-bottom: 25px; height: 100%;",
     #   tags$div(
     #     style = "position: absolute; right: 0; top: 10px; font-weight: bold; z-index: 1000;"
     #   ),
       highcharter::highchartOutput(outputId = ns("plot"), width = "100%", height = "100%")
     # )
    ),
    
    # ui
    tags$div(
      class = "btn-group-charter btn-group-justified-charter",
      tags$style(sprintf(
        "#%s .sw-dropdown-in {margin: 8px 0 8px 10px !important; padding: 0 !important;}",
        "sw-content-filterdrop"
      )),
      
      # X-Y DropUp options ------
      dropdown(
        tagList(
          pickerInput(
            inputId = ns("x_label"),
            label = "X Axes Input:",
            choices = '',
            selected = ''
            
          ),
          tags$script(
            paste0("$('#", ns("x_label"), "').addClass('dropup');")
          ),
          
          
          pickerInput(
            inputId = ns("y_label"),
            label = "Y Axes Input:",
            choices = '',
            selected = ""
             
          ),
          tags$script(
            paste0("$('#", ns("y_label"), "').addClass('dropup');")
          ),
          pickerInput(
            inputId = ns("group"),
            label = "Group Column:",
            choices = '',
            selected = ""
            
          ),
          tags$script(
            paste0("$('#", ns("group"), "').addClass('dropup');")
          )
          
        ),
        
        
        
       # xy_options(ns),
        inputId = "xy_labels",
        style = "default",
        label = "Select X-Y Axes", 
        up = TRUE, 
        icon = icon("font"), 
        status = "default btn-controls"
      ),
      
      
      # Aesthetics DropUp options ------
      
      dropdown(
        
        
        #aesthetics_options(ns),
        inputId = "Aesthetics",
        style = "default",
        label = "Aesthetics", 
        up = TRUE, 
        icon = icon("font"), 
        status = "default btn-controls"
      ),
      dropdown(
        #plot_options(ns),
       
        
        
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
            choices = list('538','flat'),
              # list(
              # 'Fivethirtyeight', 'Economist' ,'Financial Times',
              # 'Dotabuff' ,'Flat','Simple',
              # 'Elementary','Google','Firefox','Monokai',
              # 'Tufte','Sparkline',
              # 'Grid Light', 'Sand Signika' ,'Dark Unica' , 
              # 'Chalk' , 'Hand Drawn' ,'None'),
            selected = 'None',
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
        ),

        style = "default",
        label = "Plot options",
        up = TRUE, 
        inputId = "paramsdrop",
        icon = icon("gears"), 
        status = "default btn-controls"
      ),
      dropdown(
        #code_options(ns, insert_code = insert_code), 
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
        ),
        
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
    
   
  )
  
  if (is.function(container)) {
    output <- container(output)
  }
  return(output)
}