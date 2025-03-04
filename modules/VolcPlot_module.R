
# Module UI
page2UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Input controls (on the left)
        selectInput(ns("sampleID"), "Select Sample ID:", choices = colnames(ods_hgnc), selected = colnames(ods_hgnc)[1]),
        textInput(ns("main"), "Plot Title:", "Volcano Plot"),
        textInput(ns("label"), "Gene Labels (comma-separated):", "aberrant"),
        numericInput(ns("padjCutoff"), "Adjusted P-Value Cutoff:", value = 0.05),
        selectInput(ns("xaxis"), "Select X-Axis Type:", choices = c("zscore", "log2fc", "fc"), selected = "zscore"),
        selectInput(ns("type"), "Select FRASER Type:", choices = c("jaccard", "psi5", "psi3", "theta"), selected = "jaccard"),
        actionButton(ns("updatePlot"), "Update Plot")
      ),
      mainPanel(
        fluidRow(
          column(6, plotOutput(ns("outriderVolcPlot"), width = "550px", height = "600px")),
          column(6, plotOutput(ns("fraserVolcPlot"), width = "550px", height = "600px"))
        )
      )
    )
  )
}

# Module Server
page2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to generate the volcano plot
    reactive_plot_outrider <- reactive({
      plotVolcano.OUTRIDER(
        object = ods_hgnc,
        sampleID = input$sampleID,
        main = input$main,
        padjCutoff = input$padjCutoff,
        xaxis = input$xaxis,
        basePlot = TRUE,
        label = if (input$label != "") strsplit(input$label, ",")[[1]] else NULL
      )
    })
    
    # Reactive expression to generate the volcano plot
    reactive_plot_fraser <- reactive({
      FRASER::plotVolcano(
        object = fds,
        type = input$type,
        sampleID = input$sampleID,
        main = input$main,
        padjCutoff = input$padjCutoff,
        basePlot = TRUE,
        label = if (input$label != "") strsplit(input$label, ",")[[1]]
      )
    })
    
    # Render the volcano plot
    output$outriderVolcPlot <- renderPlot({
      # Only update the plot when the button is clicked
      input$updatePlot
      isolate({
        reactive_plot_outrider()
      })
    })
    
    output$fraserVolcPlot <- renderPlot({
      # Only update the plot when the button is clicked
      input$updatePlot
      isolate({
        reactive_plot_fraser()
      })
    })
  })
}