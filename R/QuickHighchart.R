QuickHighchart <- function(data = NULL,
                      viewer = "dialog") {
  
  
  if (viewer == "browser") {
    inviewer <- shiny::browserViewer(browser = getOption("browser"))
  } else if (viewer == "pane") {
    inviewer <- shiny::paneViewer(minHeight = "maximize")
  } else {
    inviewer <- shiny::dialogViewer(
      "ggQuickPlot",width=1350,height=1000
      
    )
  }
  
  runGadget(
    app = UI(
      id = "test", 
      choose_data = T # dont display button to change data
    ), 
    server = function(input, output, session) {
      callModule(
        module = Server, 
        id = "test", 
        data = iris
      )
    }, 
    viewer = inviewer
  )
}
