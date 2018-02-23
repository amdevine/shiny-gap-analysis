# library(eulerr)
library(dplyr)
# library(ggplot2)
library(rdrop2)
library(shiny)
library(tidyr)

# Load data from Dropbox
token <- readRDS('dropbox_token.rds')
col <- drop_read_csv('shiny/col.csv', dtoken = token, stringsAsFactors = FALSE)
ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token, stringsAsFactors = FALSE)
genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token, stringsAsFactors = FALSE)

get.tax.names <- function(df) {
    
}

# User interface
ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    sidebarLayout(
        sidebarPanel(
            # numericInput('inp.test', 'Test Numeric Input', 20, min = 10, max = 30),
            h4('Upload Names List'),
            fileInput('upl.names', 'Names to analyze'),
            textAreaInput(
                'inp.names', 
                'or Paste list of names here',
                placeholder = 'Paste one name per row',
                width = '200px',
                height = '100px'
            ),
            h4('Optional Settings'),
            selectInput(
                'inp.taxlevel', 
                'Select taxonomic level', 
                c(
                    'All',
                    unique(col$taxRank)
                ),
                width = '200px'
            ),
            selectInput(
                'inp.kingdom', 
                'Select kingdom', 
                c(
                    'All',
                    unique(col$kingdom)
                ),
                width = '200px'
            )
        ),
        mainPanel(
            h2('Results'),
            actionButton('dl.fig', 'Download Diagram'),
            actionButton('toggle.graph', 'Toggle Graph Type'),
            plotOutput('venn.diagram'),
            h2('List Results'),
            actionButton('dl.table', 'Download Results Table'),
            tableOutput('results.table')
        )
    )
)


server <- function(input, output) {
    
    filtered.data <- reactive({
        if (input$inp.taxlevel == 'All' && input$inp.kingdom == 'All') {
            c <- col
        } else if (input$inp.taxlevel == 'All' && input$inp.kingdom != 'All') {
            c <- filter(col, kingdom == input$inp.kingdom)
        } else if (input$inp.taxlevel != 'All' && input$inp.kingdom == 'All') {
            c <- filter(col, taxRank == input$inp.taxlevel)
        } else {
            c <- filter(col, taxRank == input$inp.taxlevel, kingdom == input$inp.kingdom)
        }
        c %>% select(kingdom, phylum, classis, ordo, family, genus) %>% print()
    })
    
    output$venn.diagram <- renderPlot({

        coldata <- filtered.data() %>% 
            gather(key = rnk, value = txnm, kingdom, phylum, classis, ordo, family, genus) %>%
            filter(txnm != '') %>%
            group_by(rnk) %>%
            summarise(n.names = n_distinct(txnm))

        # if (input$toggle.graph %% 2 == 0) {
        # barplot(coldata$txnm)
        # } else {
        pie(coldata$n.names, labels = coldata$rnk)
        # }

    })
    
    output$results.table <- renderTable({
        coltable <- filtered.data()
        colnames(coltable) <- c('Kingdom', 'Phylum/Division', 'Class', 'Order', 'Family', 'Genus')
        head(coltable, 10)
    })

}

shinyApp(ui = ui, server = server)