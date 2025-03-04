library(shiny)
setwd("~/Library/CloudStorage/OneDrive-UWA/Research/Projects/AIM3_TRANSCRIPTOMICS/app/rnaweb")
# Source your modules and external functions
source("global.R")
source("modules/SampleAnno_module.R")
source("modules/VolcPlot_module.R")
source("modules/GeneCentric_module.R")
source("modules/OUTRIDER_module.R")
source("modules/FRASER_module.R")
source("modules/Overlap_module.R")
source("modules/SampleSpecific_module.R")
source("R/plotVolcano.R")
source("R/preprocess.R")

# UI definition
ui <- navbarPage(
  title = "RNA-Web",
  tabPanel("Sample Summary", page1UI("page1")),
  tabPanel("Volcano Plot", page2UI("page2")),
  tabPanel("Gene Plot", page3UI("page3")),
  tabPanel("OUTRIDER Results", outriderUI("page4")),
  tabPanel("FRASER Results", fraserUI("page5")),
  tabPanel("Overlapping Results", OverlapUI("page6")),
  tabPanel("Sample-Specific Results", sampleSpecificUI("page7"))
)

# Server logic
server <- function(input, output, session) {
  # Call the server logic for each page with namespace IDs
  page1Server("page1")
  page2Server("page2")
  page3Server("page3")
  outriderServer("page4")
  fraserServer("page5")
  OverlapServer("page6")
  sampleSpecificServer("page7")
  
  # Observe events for the clickable links to switch tabs
  observeEvent(input$show_frares, {
    updateNavbarPage(session, "RNA-Web", selected = "FRASER Results")
  })
  
  observeEvent(input$show_outres, {
    updateNavbarPage(session, "RNA-Web", selected = "OUTRIDER Results")
  })
}

# Run the app
shinyApp(ui = ui, server = server)