# Overlapping results
OverlapUI <- function(id) {
  ns <- NS(id)  # Namespace for this module
  tagList(
    h3("OUTRIDER FRASER Overlap"),
    DT::dataTableOutput(ns("Overlap"))  # Use namespace for output ID
  )
}

OverlapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Ensure correct namespace is used
    
    # Create clickable links in the "merged" table
    merged_with_links <- merged
    merged_with_links$frares_link <- paste0(
      '<a href="#" onclick="Shiny.setInputValue(\'', ns("show_frares"), '\', {geneID:\'', 
      merged$geneID, '\', sampleID:\'', merged$sampleID, '\'})">View in FRASER</a>'
    )
    merged_with_links$outres_link <- paste0(
      '<a href="#" onclick="Shiny.setInputValue(\'', ns("show_outres"), '\', {geneID:\'', 
      merged$geneID, '\', sampleID:\'', merged$sampleID, '\'})">View in OUTRIDER</a>'
    )
    
    # Render the merged table with clickable links
    output$Overlap <- DT::renderDataTable({
      datatable(merged_with_links, escape = FALSE)  # escape = FALSE allows HTML links
    })
    
  })
}