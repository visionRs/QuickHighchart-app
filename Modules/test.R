source('~/working_dir/projects/QuickHighChart/Modules/UI.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/chooseDataUI.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/layoutAddin.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/visionRContainer.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/optionUI.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/Server.R', echo=TRUE)
library(shiny)
library(shinyWidgets)
library(miniUI)
library(ggplot2)
library(shinyjs)
ui <- fluidPage(

  # Force scroll bar to appear (otherwise hidden by esquisse)
  shinyjs::useShinyjs(),
  
    UI(
      id = "test", 
      choose_data = T # dont display button to change data
    )
  
)

server <- function(input, output, session) {
  
  callModule(
    module = Server, 
    id = "test", 
    data = iris
  )
  
  
  
}

runGadget(app=ui,server = server,viewer = dialogViewer('test',width=1000,height=750))
