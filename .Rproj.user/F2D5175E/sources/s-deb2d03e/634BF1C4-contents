source('~/working_dir/projects/QuickHighChart/Modules/UI.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/chooseDataUI.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/layoutAddin.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/visionRContainer.R', echo=TRUE)
source('~/working_dir/projects/QuickHighChart/Modules/optionUI.R', echo=TRUE)
library(shiny)
library(shinyWidgets)
library(miniUI)

ui <- fluidPage(

  # Force scroll bar to appear (otherwise hidden by esquisse)


 
    UI(
      id = "test", 
      choose_data = FALSE # dont display button to change data
    )
  
)

server <- function(input, output, session) {
  

  
}

runGadget(app=ui,server = server,viewer = dialogViewer('test',width=1000,height=750))
