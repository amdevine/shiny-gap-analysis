library(shiny)

# Load the Catalogue of Life data frame
col <- read.csv('col.csv', stringsAsFactors = FALSE)

# User interface
ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    sidebarLayout(
        sidebarPanel(
            h4('Upload Names List'),
            fileInput('inp.names', 'Names to analyze'),
            h4('Optional Settings'),
            selectInput(
                'inp.taxlevel', 
                'Select taxonomic level', 
                c('Kingdom' = 'kin', 
                  'Phylum/Division' = 'phy', 
                  'Class' = 'cla', 
                  'Order' = 'ord', 
                  'Family' = 'fam', 
                  'Genus' = 'gen')
                ),
            selectInput(
                'inp.kingdom', 
                'Select kingdom', 
                c('Animalia' = 'ani', 
                  'Plantae' = 'pla', 
                  'Fungi' = 'fun', 
                  'Chromista' = 'chr', 
                  'Protozoa' = 'pro', 
                  'Bacteria' = 'bac', 
                  'Viruses' = 'vir')
                )
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

server <- function(input, output) {}

shinyApp(ui = ui, server = server)