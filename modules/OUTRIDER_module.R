# OUTRIDER Module with namespaces
outriderUI <- function(id) {
  ns <- NS(id)  # Namespace for this module
  tagList(
    h3("OUTRIDER results"),
    DT::dataTableOutput(ns("OUTRIDER"))  # Use namespace for output ID
  )
}

outriderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use the namespace
    
    # Render OUTRIDER table
    output$OUTRIDER <- DT::renderDataTable({
      outres  # Render the full table
    })
    
    # Filter OUTRIDER results based on selected geneID and sampleID
    observeEvent(input[[ns("show_outres")]], {
      selected_outres <- outres[outres$geneID == input[[ns("show_outres")]]$geneID & outres$sampleID == input[[ns("show_outres")]]$sampleID, ]
      output$OUTRIDER <- DT::renderDataTable({
        datatable(selected_outres)
      })
    })
  })
}