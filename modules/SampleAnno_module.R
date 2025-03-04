
page1UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Sample Sheet Summary"),
    DT::dataTableOutput(ns("sampleSummary"))
  )
}

page1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
      output$sampleSummary <- DT::renderDataTable({
        sam_data  # Render the full table with search, sort, etc.
      })
  })
}