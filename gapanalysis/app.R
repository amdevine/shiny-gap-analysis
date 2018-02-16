library(shiny)
library(eulerr)
library(dplyr)
library(ggplot2)
library(tidyr)
eulerr_options(pointsize = 12)
options(digits = 4)

# Load the Catalogue of Life data frame
col <- read.csv('col.csv', stringsAsFactors = FALSE)

# User interface
ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    sidebarLayout(
        sidebarPanel(
            numericInput('inp.test', 'Test Numeric Input', 20, min = 10, max = 30),
            h4('Upload Names List'),
            fileInput('upl.names', 'Names to analyze'),
            textAreaInput(
                'inp.names', 
                'or Paste list of names here',
                placeholder = 'Paste one name per row',
                width = '200px',
                height = '200px'
                ),
            h4('Optional Settings'),
            selectInput(
                'inp.taxlevel', 
                'Select taxonomic level', 
                c(
                    # 'All' = 'All', 
                    unique(col$taxRank)
                )),
            selectInput(
                'inp.kingdom', 
                'Select kingdom', 
                c(
                    # 'All' = 'All', 
                    unique(col$kingdom)
                ))
            ),
        mainPanel(
            h2('Venn Diagram Results'),
            actionButton('dl.fig', 'Download Venn Diagram'),
            plotOutput('venn.diagram'),
            h2('List Results'),
            actionButton('dl.table', 'Download Results Table'),
            tableOutput('results.table')
            )
    )
)


server <- function(input, output) {
    
    output$venn.diagram <- renderPlot({
        coldata <- col %>%
            filter(taxRank == input$inp.taxlevel,
                   kingdom == input$inp.kingdom) %>%
            select(kingdom, phylum, classis, ordo, family, genus) %>%
            gather('trank', 'tname')
        print(coldata)
            # distinct() %>%
            # filter(!is.na(tname))
            # group_by(trank) %>%
            # count(tname)
        plot(coldata)  
            
        # plot(euler(c(
        #     "GGBN" = input$inp.test, "GenBank" = 5, "GBIF" = 5,
        #     "GGBN&GenBank" = 5, "GGBN&GBIF" = 5, "GenBank&GBIF" = 3,
        #     "GGBN&GenBank&GBIF" = 3
        #     )))
    })
    
}

shinyApp(ui = ui, server = server)