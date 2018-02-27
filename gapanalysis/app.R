# library(eulerr)
library(dplyr)
# library(ggplot2)
library(rdrop2)
library(shiny)
library(tidyr)
# library(VennDiagram)

# Load data from Dropbox
token <- readRDS('dropbox_token.rds')

# CHANGE FOR DEPLOYMENT
# gbif <- drop_read_csv('shiny/gbif.csv', dtoken = token, stringsAsFactors = FALSE)
gbif <- read.csv('gbif.csv', stringsAsFactors = FALSE)

# CHANGE FOR DEPLOYMENT
# ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token, stringsAsFactors = FALSE)
ggbn <- read.csv('ggbn.csv', stringsAsFactors = FALSE)
ggbn <- ggbn[, -1]
ggbn$rank <- tolower(ggbn$rank)

# CHANGE FOR DEPLOYMENT
# genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token, stringsAsFactors = FALSE)
genbank <- read.csv('genbank.csv', stringsAsFactors = FALSE)
genbank <- genbank[, -1]
genbank$rank <- tolower(genbank$rank)

# User interface
ui <- fluidPage(
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    sidebarLayout(
        sidebarPanel(
            # numericInput('inp.test', 'Test Numeric Input', 20, min = 10, max = 30),
            h4('Input Names for Analysis'),
            textInput('list.name', 'Dataset label', value = "User Data"),
            fileInput('inp.name.file', 'Upload list of names'),
            textAreaInput(
                'inp.name.list', 
                'OR enter list of names',
                placeholder = 'One name per row',
                width = '200px',
                height = '100px'
            ),
            selectInput(
                'inp.taxlevel', 
                'Taxonomic level of names', 
                c(
                    'Not Specified' = 'All',
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
                'Kingdom of names', 
                c(
                    'Not Specified' = 'All',
                    sort(unique(gbif$kingdom))
                ),
                width = '200px'
            ),
            actionButton('analyze', 'Run Analysis')
        ),
        mainPanel(
            # h2('Results'),
            # actionButton('dl.fig', 'Download Diagram'),
            # actionButton('toggle.graph', 'Toggle Graph Type'),
            # plotOutput('venn.diagram'),
            h2('List Results'),
            downloadButton('dl.table', 'Download Results Table'),
            div(tableOutput('results.table'), style = "font-size:80%")
        )
    )
)


server <- function(input, output) {
    
    filtered <- eventReactive(input$analyze, {
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
    
    query.names <- eventReactive(input$analyze, {
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
        } else if (!is.null(input$inp.name.file)) {
            
        } else if ( is.null(input$inp.name.file) && !is.null(input$inp.name.list) ) {
            return(strsplit(input$inp.name.list, "\n"))
        }
    })
    
    results.df <- reactive({
        query.df <- data.frame(query.names())
        colnames(query.df) <- c('submitted_name')
        gbif.results <- filtered()$gbif %>% filter(name %in% query.df$submitted_name)
        query.df <- query.df %>% 
                    full_join(gbif.results, by = c('submitted_name' = 'name')) %>%
                    mutate(qn = ifelse(accepted_name == '', 
                                       submitted_name, 
                                       accepted_name)) %>%
                    left_join(filtered()$ggbn, 
                              by = c('qn' = 'name', 'rank', 'kingdom'),
                              suffix = c("_gbif", "_ggbn")) %>%
                    left_join(filtered()$genbank,
                              by = c('qn' = 'name', 'rank', 'kingdom'),
                              suffix = c("", "_genbank")) %>%
                    rename(name_status = status, phylum_genbank = phylum, 
                           class_genbank = class, order_genbank = order, 
                           family_genbank = family, genus_genbank = genus) %>%
                    select(submitted_name, rank, name_status, accepted_name, qn, 
                           kingdom, phylum_gbif, class_gbif, order_gbif, family_gbif, 
                           genus_gbif, phylum_ggbn, class_ggbn, order_ggbn, family_ggbn, 
                           genus_ggbn, phylum_genbank, class_genbank, order_genbank, 
                           family_genbank, genus_genbank)
    })
    
    # output$venn.diagram <- renderPlot({

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
    #     return()
    # 
    # })
    
    output$results.table <- renderTable({
        table.data <- results.df() %>%
                      mutate(in_ggbn = ifelse(is.na(phylum_ggbn), "No", "Yes")) %>%
                      mutate(in_genbank = ifelse(is.na(phylum_genbank), "No", "Yes")) %>%
                      select(submitted_name, rank, name_status, accepted_name, 
                             in_ggbn, in_genbank,
                             kingdom, phylum_gbif, class_gbif, order_gbif, family_gbif,
                             genus_gbif) %>%  
                      rename('Submitted Name' = submitted_name,
                             Rank = rank,
                             'Name Status' = name_status,
                             'Accepted Name' = accepted_name,
                             Kingdom = kingdom,
                             Phylum = phylum_gbif,
                             Class = class_gbif,
                             Order = order_gbif,
                             Family = family_gbif,
                             Genus = genus_gbif,
                             'In GGBN' = in_ggbn,
                             'In GenBank' = in_genbank)
        if (nrow(table.data) > 100) {
            return(table.data[100,])
        } else {
            return(table.data)
        }
    })
    
    output$dl.table <- downloadHandler(
        filename = function() {
            paste(input$list.name, "Gap Analysis.csv", sep = ' ')
        },
        content = function(file) {
            write.csv(results.df(), file, row.names = FALSE)
        },
        contentType = 'text/plain'
    )

}

shinyApp(ui = ui, server = server)