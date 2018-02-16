library(shiny)

# Load the Catalogue of Life data frame
col <- read.csv('col.csv', stringsAsFactors = FALSE)

ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    br(),
    sidebarLayout(
        sidebarPanel(
            h4('Upload Names List'),
            fileInput('file.names', 'Names to analyze'),
            h4('Optional Settings'),
            selectInput(
                'select.tax', 
                'Select taxonomic level', 
                c('Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species')
                ),
            selectInput(
                'select.king', 
                'Select kingdom', 
                c('Animalia', 'Plantae', 'Fungi', 'Chromista', 'Protozoa', 'Bacteria', 'Viruses')
                )
            ),
        mainPanel(
            h2('Venn Diagram Results'),
            p('Insert pretty figure here', style = 'color: gray;'),
            h2('List Results'),
            p('Download button', style = 'color: gray;'),
            p('Insert table here', style = 'color: gray;')
            )
    )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)