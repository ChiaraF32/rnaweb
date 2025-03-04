# Sample Specific Results UI
sampleSpecificUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sample-Specific Results"),
    
    # Dropdown for selecting a sample
    selectInput(ns("sampleID"), "Select Sample ID:", choices = unique(sam_data$RNA_ID), selected = NULL),
    
    # Render tables and plot based on selected sample
    fluidRow(
      column(12, h4("OUTRIDER Results")),
      column(12, DT::dataTableOutput(ns("OUTRIDER_table")))
    ),
    
    fluidRow(
      column(12, h4("FRASER Results")),
      column(12, DT::dataTableOutput(ns("FRASER_table")))
    ),
    
    fluidRow(
      column(12, h4("RNA Variants")),
      column(12, DT::dataTableOutput(ns("RNAvc_table")))
    ),
    
    fluidRow(
      # OUTRIDER Volcano Plot
      column(6,
             h4("OUTRIDER Volcano Plot"),
             plotOutput(ns("volcano_plot_out"))
      ),
      
      # FRASER Volcano Plot with dropdown above
      column(6,
             h4("FRASER Volcano Plot"),
             selectInput(ns("type"), "Select Metric:", choices = c("psi5", "psi3", "theta", "jaccard")),
             plotOutput(ns("volcano_plot_fra"))
      )
    )
  )
}

# Sample Specific Results Server
sampleSpecificServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression to filter tables based on selected sampleID
    filtered_OUTRIDER <- reactive({
      req(input$sampleID) # Ensure a sampleID is selected
      outres[outres$sampleID == input$sampleID, ]
    })
    
    filtered_FRASER <- reactive({
      req(input$sampleID) # Ensure a sampleID is selected
      frares[frares$sampleID == input$sampleID, ]
    })
    
    filtered_VC <- reactive({
      req(input$sampleID)
      vds[FILTER == "PASS_rare" & vds[[input$sampleID]] %in% c("0/1", "1/1")]
    })
    
    reactive_volcano_out <- reactive({
      plotVolcano.OUTRIDER(
        object = ods_hgnc,
        sampleID = input$sampleID,
        main = paste("Volcano Plot for", input$sampleID),
        basePlot = TRUE,
        label = "aberrant"
      )
    })
    
    reactive_volcano_fra <- reactive({
      FRASER::plotVolcano(
        object = fds,
        sampleID = input$sampleID,
        type = input$type,
        main = paste("Volcano Plot for", input$sampleID),
        basePlot = TRUE,
        label = "aberrant"
      )
    })
    
    # Render the OUTRIDER results table
    output$OUTRIDER_table <- DT::renderDataTable({
      datatable(filtered_OUTRIDER())
    })
    
    # Render the FRASER results table
    output$FRASER_table <- DT::renderDataTable({
      datatable(filtered_FRASER())
    })
    
    # Render the RNAvc results table
    output$RNAvc_table <- DT::renderDataTable({
      datatable(filtered_VC())
    })
    
    #Render the OUTRIDER Volcano Plot
    output$volcano_plot_out <- renderPlot({
      reactive_volcano_out()
    })
    
    # Render the FRASER Volcano Plots
    output$volcano_plot_fra <- renderPlot({
      reactive_volcano_fra()
    })
  })
}