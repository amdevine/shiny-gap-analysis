# library(eulerr)
library(dplyr)
# library(ggplot2)
library(rdrop2)
library(shiny)
library(tidyr)

# Load data from Dropbox
token <- readRDS('dropbox_token.rds')

gbif <- drop_read_csv('shiny/gbif.csv', dtoken = token, stringsAsFactors = FALSE)

ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token, stringsAsFactors = FALSE)
ggbn <- ggbn[, -1]
ggbn$rank <- tolower(ggbn$rank)

genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token, stringsAsFactors = FALSE)
genbank <- genbank[, -1]
genbank$rank <- tolower(genbank$rank)

# User interface
ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    sidebarLayout(
        sidebarPanel(
            # numericInput('inp.test', 'Test Numeric Input', 20, min = 10, max = 30),
            h4('Upload Names List'),
            fileInput('inp.name.file', 'Names to analyze'),
            textAreaInput(
                'inp.name.list', 
                'or paste list of names here (one per row)',
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
                    'Kingdom' = 'kingdom',
                    "Phylum/Division" = 'phylum',
                    'Class' = 'class',
                    'Order' = 'order',
                    'Family' = 'family',
                    'Genus' = 'genus'
                ),
                width = '200px'
            ),
            selectInput(
                'inp.kingdom', 
                'Select kingdom', 
                c(
                    'All',
                    sort(unique(gbif$kingdom))
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
    
    filtered <- reactive({
        print(c('Input tax level:', input$inp.taxlevel))
        print(c('Input kingdom:', input$inp.kingdom))
        if (input$inp.taxlevel == 'All' && input$inp.kingdom == 'All') {
            f <- gbif
            n <- ggbn
            k <- genbank
        } else if (input$inp.taxlevel == 'All' && input$inp.kingdom != 'All') {
            f <- filter(gbif, kingdom == input$inp.kingdom)
            n <- filter(ggbn, kingdom == input$inp.kingdom)
            k <- filter(genbank, kingdom == input$inp.kingdom)
        } else if (input$inp.taxlevel != 'All' && input$inp.kingdom == 'All') {
            f <- filter(gbif, rank == input$inp.taxlevel) 
            n <- filter(ggbn, rank == input$inp.taxlevel) 
            k <- filter(genbank, rank == input$inp.taxlevel)
        } else {
            f <- filter(gbif, rank == input$inp.taxlevel, kingdom == input$inp.kingdom)
            n <- filter(ggbn, rank == input$inp.taxlevel, kingdom == input$inp.kingdom)
            k <- filter(genbank, rank == input$inp.taxlevel, kingdom == input$inp.kingdom)
        }
        list(gbif = f, ggbn = n, genbank = k)
    })
    
    query.names <- reactive({
        if ( is.null(input$inp.name.file) && is.null(input$inp.name.list) ) {
            return(NULL)
        } else if ( input$inp.name.list == 'plant families' ) {
            return(c('Asteraceae', 'Betulaceae', 'Caprifoliaceae', 'Droseraceae', 
                     'Ericaceae', 'Fabaceae', 'Gesneriaceae','Heliconiaceae',
                     'Iridaceae', 'Jungermanniaceae', 'Kirkiaceae', 'Lauraceae',
                     'Melastomataceae', 'Nyctaginaceae', 'Orchidaceae', 'Poaceae',
                     'Quillajaceae', 'Rosaceae', 'Sapindaceae', 'Taxaceae',
                     'Urticaceae', 'Violaceae', 'Woodsiaceae', 'Xanthorrhoeaceae',
                     'Zingiberaceae'))
        } else if ( input$inp.name.list == 'test data' ) {
            return(c(
                'Animalia', 'Arthropoda', 'Aves', 'Artiodactyla', 'Agamidae', 'Apis',
                'Plantae', 'Pteridophyta', 'Polypodiopsida', 'Poales', 'Poaceae', 'Poa',
                'Fungi', 'Basidiomycota', 'Agaricomycetes', 'Agaricales', 'Agaricaceae', 'Calvatia'))
        } else if ( is.null(input$inp.name.file) && !is.null(input$inp.name.list) ) {
            return(strsplit(input$inp.name.list, "\n"))
        }
    })
    
    output$venn.diagram <- renderPlot({

        # coldata <- filtered.col() %>% 
        #     gather(key = rnk, value = txnm, kingdom, phylum, classis, ordo, family, genus) %>%
        #     filter(txnm != '') %>%
        #     group_by(rnk) %>%
        #     summarise(n.names = n_distinct(txnm))
        # 
        # # if (input$toggle.graph %% 2 == 0) {
        # # barplot(coldata$txnm)
        # # } else {
        # pie(coldata$n.names, labels = coldata$rnk)
        # # }
        return()

    })
    
    output$results.table <- renderTable({
        if (is.null(query.names())) {
            return()
        }
        query.df <- data.frame(query.names())
        colnames(query.df) <- c('qn')
        query.df$ggbn <- query.df$qn %in% filtered()$ggbn$name
        query.df$genbank <- query.df$qn %in% filtered()$genbank$name
        colnames(query.df) <- c('Query Name', 'In GGBN', 'In GenBank')
        query.df
    })

}

shinyApp(ui = ui, server = server)