# library(eulerr)
library(dplyr)
# library(ggplot2)
library(rdrop2)
library(shiny)
library(tidyr)
# library(VennDiagram)

# Load data from Dropbox
token <- readRDS('dropbox_token.rds')

# FOR USE DURING DEVELOPMENT
gbif <- read.csv('../gbif.csv', stringsAsFactors = FALSE)
ggbn <- read.csv('../ggbn.csv', stringsAsFactors = FALSE)
genbank <- read.csv('../genbank.csv', stringsAsFactors = FALSE)

# FOR USE ON LIVE SITE
# gbif <- drop_read_csv('shiny/gbif.csv', dtoken = token, stringsAsFactors = FALSE)
# ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token, stringsAsFactors = FALSE)
# genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token, stringsAsFactors = FALSE)

# Select appropriate GGBN and GenBank columns
ggbn <- ggbn[, -1]
ggbn$rank <- tolower(ggbn$rank)

genbank <- genbank[, -1]
genbank$rank <- tolower(genbank$rank)

# User interface
ui <- fluidPage(
    
    # Title
    titlePanel('GGI Gap Analysis Tool'),
    p('This app is used to conduct gap analyses for GGI funded projects.'),
    
    sidebarLayout(
        
        # Options menu on sidebar
        sidebarPanel(
            width = 3,
            h3('Input Names'),
            textInput('list.name', 'Dataset label', value = "User Data"),
            fileInput('inp.name.file', 'Upload list of names'),
            textAreaInput(
                'inp.name.list', 
                'OR enter list of names',
                placeholder = 'One name per row',
                # width = '150px',
                height = '100px'
            ),
            selectInput(
                # width = '200px,',
                'inp.taxlevel', 
                'Taxonomic rank of names', 
                c(
                    'Not Specified' = 'All',
                    'Kingdom' = 'kingdom',
                    "Phylum/Division" = 'phylum',
                    'Class' = 'class',
                    'Order' = 'order',
                    'Family' = 'family',
                    'Genus' = 'genus'
                )
            ),
            selectInput(
                # width = '200px',
                'inp.kingdom', 
                'Kingdom of names', 
                c(
                    'Not Specified' = 'All',
                    sort(unique(gbif$kingdom))
                )
            ),
            actionButton('analyze', 'Run Analysis')
        ),
        
        # Results tabs
        mainPanel(
            h3('Results'),
            tabsetPanel(type = 'tabs',
                        
                        tabPanel("Instructions",
                                 h4("Input Names"),
                                 p(strong("Dataset label:"), 
                                   "Enter the name of analysis. e.g. \'NMNH Amphibians\'"),
                                 p(strong("Upload list of names:"), 
                                   "Allows text (.txt) file containing list of names to be upload for analysis. 
                                   Please format text file with one name per line."),
                                 p(strong("Enter list of names:"), 
                                   "Alternatively, enter a list of names to be analyzed.
                                   Please enter one name per line."),
                                 p(strong("Taxonomic rank of names:"), 
                                   "If all names are the same taxonomic rank (e.g. Family, Genus),
                                   select the appropriate rank. This may reduce the number of
                                   extraneous results returned from incorrect ranks."),
                                 p(strong("Kingdom of names:"), 
                                   "If all names are the found in the same kingdom (e.g. Animalia, Plantae), 
                                   select the appropriate kingdom. This may reduce the number of 
                                   extraneous results from homonymous names."),
                                 h4("Results"),
                                 p("Submitted names are queried in static inventory lists from GBIF, GGBN,
                                   and GenBank. Names are first matched to GBIF, which returns the taxonomic
                                   rank, name status, and if the name is not accepted, the accepted name.
                                   Then the accepted name is queried in GGBN and GenBank, which returns whether 
                                   that name is found in those databases. The taxonomic hierarchy is provided 
                                   for each name to allow the user to verify that the correct name has been queried."),
                                 p(strong("Summary:"),
                                   "A summary of the names entered, and how many were found to be in GBIF,
                                   GGBN, and GenBank."),
                                 p(strong("Results Table:"),
                                   "Returns a table containing results of the first 100 names submitted. 
                                   The \'Download Results Table\' button allows the user to download the results 
                                   for all submitted names as a comma-delimited .csv file."),
                                 p(strong("Figures:"),
                                   "Venn diagrams generated at each taxonomic level, showing the breakdown of 
                                   names submitted and their presence in GGBN and GenBank.")
                                 ),
                        
                        tabPanel("Summary",
                                 tableOutput('summary.table')),
                        
                        tabPanel("Results Table", 
                                 downloadButton('dl.table', 
                                                'Download Results Table', 
                                                style = "margin-bottom:1em;margin-top:1em"),
                                 div(tableOutput('results.table'), style = "font-size:80%")),
                        
                        tabPanel("Figures",
                                 # downloadButton('dl.figures',
                                 #                'Download Figures',
                                 #                style = "margin-bottom:1em;margin-top:1em"),
                                 plotOutput('venn.phyla'),
                                 plotOutput('venn.classes'),
                                 plotOutput('venn.orders'),
                                 plotOutput('venn.families'),
                                 plotOutput('venn.genera')))
    )),
    
    # Footer
    p('For assistance with this application, please contact ggi@globalgeno.me.',
      style = "margin-bottom:3em")
)


server <- function(input, output) {
    
    # Returns string with first letter of each word capitalized
    .simpleCap <- function(x) {
        x <- tolower(x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    
    # Filters GBIF, GGBN, and GenBank dataframes based on user input of Kingdom and Tax Rank
    filtered <- eventReactive(input$analyze, {
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
    
    # Processes list of user-inputed names
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
            infile <- read.table(input$inp.name.file$datapath, sep = "\n", stringsAsFactors = FALSE)
            ns <- as.character(infile[,1])
            ns <- sapply(ns, .simpleCap)
            print(ns)
            return(ns)
        } else if ( is.null(input$inp.name.file) && !is.null(input$inp.name.list) ) {
            ns <- unlist(strsplit(input$inp.name.list, "\n"))
            ns <- sapply(ns, .simpleCap)
            return(ns)
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
    
    graph.label <- eventReactive(input$analyze, { return(input$list.name) })
    
    pfig <- reactive({
        hist(rnorm(100, mean = 10, sd = 1), 
             main = paste(graph.label(), '- Phyla', sep = " "),
             xlab = 'Phyla')
    })
    
    cfig <- reactive({
        hist(rnorm(100, mean = 20, sd = 1), 
             main = paste(graph.label(), '- Classes', sep = " "),
             xlab = 'Classes')        
    })
    
    ofig <- reactive({
        hist(rnorm(100, mean = 30, sd = 1), 
             main = paste(graph.label(), '- Orders', sep = " "),
             xlab = 'Orders')
    })
    
    ffig <- reactive({
        hist(rnorm(100, mean = 40, sd = 1), 
             main = paste(graph.label(), '- Families', sep = " "),
             xlab = 'Families')
    })
    
    gfig <- reactive({
        hist(rnorm(100, mean = 50, sd = 1), 
             main = paste(graph.label(), '- Genera', sep = " "),
             xlab = 'Genera')
    })
    
    output$venn.phyla <- renderPlot({ pfig() })
    output$venn.classes <- renderPlot({ cfig() })
    output$venn.orders <- renderPlot({ ofig() })
    output$venn.families <- renderPlot({ ffig() })
    output$venn.genera <- renderPlot({ gfig() })

    output$summary.table <- renderTable({})
    
    output$results.table <- renderTable({
        table.data <- results.df() %>%
                      mutate(in_ggbn = ifelse(is.na(phylum_ggbn), "No", "Yes"),
                             in_genbank = ifelse(is.na(phylum_genbank), "No", "Yes")) %>%
                      # select(submitted_name, rank, name_status, accepted_name, 
                      #        in_ggbn, in_genbank,
                      #        kingdom, phylum_gbif, class_gbif, order_gbif, family_gbif,
                      #        genus_gbif) %>%  
                      select('Submitted Name' = submitted_name,
                             Rank = rank,
                             'Name Status' = name_status,
                             'Accepted Name' = accepted_name,
                             'Queried Name' = qn,
                             'In GGBN' = in_ggbn,
                             'In GenBank' = in_genbank,
                             Kingdom = kingdom,
                             Phylum = phylum_gbif,
                             Class = class_gbif,
                             Order = order_gbif,
                             Family = family_gbif,
                             Genus = genus_gbif)
        if (nrow(table.data) > 100) {
            return(table.data[1:100,])
        } else {
            return(table.data)
        }
    })
    
    output$dl.table <- downloadHandler(
        contentType = 'text/plain',
        filename = function() {
            paste(input$list.name, "Gap Analysis.csv", sep = ' ')
        },
        content = function(file) {
            filtered.columns <- results.df() %>%
                                mutate(in_ggbn = case_when(is.na(phylum_ggbn) ~ "no", TRUE ~ "yes"),
                                       in_genbank = case_when(is.na(phylum_genbank) ~ "no", TRUE ~ "yes")) %>%
                                select(submitted_name, rank, name_status, accepted_name, queried_name = qn,
                                       in_ggbn, in_genbank,
                                       kingdom, phylum = phylum_gbif, class = class_gbif, order = order_gbif,
                                       family = family_gbif, genus = genus_gbif)
            write.table(filtered.columns, file, row.names = FALSE, sep = '\t')
        }
    )
    
    # output$dl.figures <- downloadHandler(
    #     contentType = 'application/zip',
    #     filename = function() {
    #         paste(input$list.name, "Figures.zip", sep = ' ')
    #     },
    #     content = function(file) {
    #         paths <- c()
    #         figs <- list(c("phyla", pfig()), 
    #                      c("classes", cfig()), 
    #                      c("orders", ofig()), 
    #                      c("families", ffig()), 
    #                      c("genera", gfig()))
    #         tmp <- tempdir()
    #         setwd(tempdir())
    #         for (f in figs) { 
    #             fname <- paste0(f[1], ".png")
    #             paths <- c(paths, fname)
    #             ggsave(fname, plot = f[2], device = 'png')
    #         }
    #         zip(zipfile = file, files = paths)
    #     }
    # )
    # output$dl.figures <- downloadHandler(
    #     filename = "phyla.txt",
    #     content = function
    # )

}

shinyApp(ui = ui, server = server)