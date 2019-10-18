source('~/working_dir/projects/QuickHighChart/Modules/UI.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/chooseDataUI.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/layoutAddin.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/visionRContainer.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/optionUI.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/Server.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/Bar-Plot.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/rCodeContainer.R', echo=F)
source('~/working_dir/projects/QuickHighChart/Modules/Line-Plot.R', echo=TRUE)
library(shiny)
library(shinyWidgets)
library(miniUI)
library(ggplot2)
library(shinyjs)
library(stringr)
library(stringi)
library(esquisse)
library(highcharter)
library(rlang)
library(RColorBrewer)

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
