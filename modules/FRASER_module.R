fraserUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("FRASER results"),
    DT::dataTableOutput(ns("FRASER"))
  )
}

fraserServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Use the namespace
    
    # Render FRASER table
    output$FRASER <- DT::renderDataTable({
      frares  # Render the full table
    })
    
    # Filter FRASER results based on selected geneID and sampleID
    observeEvent(input[[ns("show_frares")]], {
      selected_frares <- frares[frares$geneID == input[[ns("show_frares")]]$geneID & frares$sampleID == input[[ns("show_frares")]]$sampleID, ]
      output$FRASER <- DT::renderDataTable({
        datatable(selected_frares)
      })
    })
    
  })
}

