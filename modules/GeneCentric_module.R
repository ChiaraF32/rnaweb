## using normalised count data from OUTRIDER data set
counts_norm <- as.data.frame(counts(ods_hgnc, normalized = TRUE))
counts_norm$geneID <- row.names(counts_norm)

# Reorder and reshape counts for plotting (removes any potential issues with rownames)
reord_counts <- counts_norm %>%
  gather("sample_ID", "counts", -geneID)

# Get unique genes dynamically from the dataset
all_genes <- unique(reord_counts$geneID)

# Shiny UI for gene-level expression plots
page3UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        # Dropdown for selecting a gene (dynamically populate choices from all genes)
        selectInput(ns("gene"), "Select Gene:", choices = all_genes, selected = all_genes[1]),
        
        # Dropdown for selecting a patient (from available sample IDs)
        selectInput(ns("patientID"), "Select Patient ID:", choices = unique(reord_counts$sample_ID), selected = "D20-0017")
      ),
      mainPanel(
        # Plot output for the gene-level expression plot
        plotOutput(ns("genePlot"), width = "800px", height = "600px")
      )
    )
  )
}

# Shiny Server for gene-level expression plots
page3Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to filter data based on selected gene and patient
    reactive_counts <- reactive({
      selected_gene <- input$gene
      selected_patient <- input$patientID
      
      # Filter the counts for the selected gene
      filtered_counts <- reord_counts %>%
        filter(geneID == selected_gene)
      
      # Create separate data for the highlighted patient and other samples
      highlight <- filtered_counts %>%
        filter(sample_ID == selected_patient)
      
      other <- filtered_counts %>%
        filter(sample_ID != selected_patient)
      
      list(highlight = highlight, other = other)
    })
    
    # Render the plot
    output$genePlot <- renderPlot({
      counts_data <- reactive_counts()  # Get the filtered data
      
      ggplot(counts_data$other, aes(factor(0), counts)) + 
        facet_wrap(~geneID, scales = "free_y") +
        geom_violin() + 
        geom_boxplot(width = 0.3) +
        geom_jitter(color = "dark grey", size = 1, alpha = 0.9) +
        geom_point(data = counts_data$highlight, color = "red", size = 2, alpha = 0.9) +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank()) + 
        labs(x = "gene", 
             y = "Expression (normalized counts)", 
             title = paste("Expression of", input$gene))
    })
  })
}