function(input, output, session) {

#-----------------------------------------------------------------------------#
# FUNCTIONS
    
    # Returns string with first letter of each word capitalized
    .simpleCap <- function(x) {
        x <- tolower(x)
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
    }
    
    # Returns a list of taxonomic names from GBIF at a specified rank
    .listchoices <- function(nameoptions, filtrank) {
        if(filtrank == 'All') {
            return(c('None Selected' = 'All'))
        } else {
            return(c(
                'None Selected' = 'All',
                sort(unique(nameoptions[,filtrank]))
            ))
        }
    }
    
#-----------------------------------------------------------------------------#
# TAXONOMIC FILTERING

    gbifnames <- gbif %>%
                 select(kingdom, phylum, class, order, family) %>%
                 distinct()
    
    # Whenever the filter rank changes, updates the list of name possibilities
    observeEvent(input$filter.rank, {
        updateSelectInput(
            session,
            inputId = 'filter.name',
            choices = .listchoices(gbifnames, input$filter.rank)
        )
    })
        
#-----------------------------------------------------------------------------#
# NAME INPUT FIELDS 
    
    # Filters GBIF, GGBN, and GenBank dataframes based on user input of Kingdom and Tax Rank
    filtered <- eventReactive(input$analyze, {
        
        .filterdataset <- function(x) {
            # Filters based on input tax rank specified
            if (input$inp.taxlevel == 'All') {
                txfilter <- !is.na(x$name)
            } else {
                txfilter <- x$rank == input$inp.taxlevel
            }
            
            # Filters output based on desired taxonomic rank and name
            if (input$filter.name == 'All') {
                nfilter <- !is.na(x$name)
            } else {
                nfilter <- x[,input$filter.rank] == input$filter.name
            }
            
            # Filters based on name status, if present in dataset
            if ("status" %in% colnames(x) && input$filter.nstatus != 'All') {
                sfilter <- x$status == input$filter.nstatus
            } else {
                sfilter <- !is.na(x$name)
            }
            
            filter(x, txfilter, nfilter, sfilter)
        }
        
        list(
            gbif = .filterdataset(gbif), 
            ggbn = .filterdataset(ggbn), 
            genbank = .filterdataset(genbank)
        )
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
            return(ns)
        } else if ( is.null(input$inp.name.file) && !is.null(input$inp.name.list) ) {
            ns <- unlist(strsplit(input$inp.name.list, "\n"))
            ns <- sapply(ns, .simpleCap)
            return(ns)
        }
    })

#-----------------------------------------------------------------------------#
# DATA FRAMES AND SUMMARY TABLES
    
    # Data frame containing the results of joining all the data sets
    results.df <- reactive({
        query.df <- data.frame(query.names(), stringsAsFactors = FALSE)
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
    
    # Data frame containing numerical counts of taxa new to GGBN and/or GenBank
    summary.df <- reactive({
        n.querynames <- length(query.names())
        n.results <- nrow(results.df())
        ggbn.names <- ggbn %>%
            select(kingdom, phylum, class, order, family, genus) %>%
            gather(key = 'rank', value = 'name', na.rm = TRUE) %>%
            filter(name != '') %>%
            select(name) %>%
            distinct()
        ggbn.names <- ggbn.names$name
        genbank.names <- genbank %>%
            select(kingdom, phylum, class, order, family, genus) %>%
            gather(key = 'rank', value = 'name', na.rm = TRUE) %>%
            filter(name != '') %>%
            select(name) %>%
            distinct()
        genbank.names <- genbank.names$name
        sub.data <- results.df() %>%
            select(kingdom, phylum = phylum_gbif, class = class_gbif, order = order_gbif,
                   family = family_gbif, genus = genus_gbif) %>%
            gather(key = 'rank', value = 'name', na.rm = TRUE) %>%
            distinct() %>%
            filter(name != '')
        sub.data <- sub.data %>%
            mutate(in_ggbn = ifelse(name %in% ggbn.names, TRUE, FALSE),
                   in_genbank = ifelse(name %in% genbank.names, TRUE, FALSE))
        sub.data <- sub.data %>%
            mutate(new_ggbn = !in_ggbn,
                   new_genbank = !in_genbank)
        sub.data <- sub.data %>%
            mutate(in_ggbn_only = ifelse(in_ggbn & !in_genbank, TRUE, FALSE),
                   in_genbank_only = ifelse(!in_ggbn & in_genbank, TRUE, FALSE),
                   in_both = ifelse(in_ggbn & in_genbank, TRUE, FALSE),
                   in_neither = ifelse(!in_ggbn & !in_genbank, TRUE, FALSE),
                   new_ggbn_only = ifelse(new_ggbn & !new_genbank, TRUE, FALSE),
                   new_genbank_only = ifelse(!new_ggbn & new_genbank, TRUE, FALSE),
                   new_both = ifelse(new_ggbn & new_genbank, TRUE, FALSE),
                   new_neither = ifelse(!new_ggbn & !new_genbank, TRUE, FALSE)) %>%
            group_by(rank) %>%
            summarize('total' = n(),
                      'total_in_ggbn' = sum(in_ggbn),
                      'total_in_genbank' = sum(in_genbank),
                      'in_ggbn_only' = sum(in_ggbn_only),
                      'in_genbank_only' = sum(in_genbank_only),
                      'in_both' = sum(in_both),
                      'in_neither' = sum(in_neither),
                      'total_new_to_ggbn' = sum(new_ggbn),
                      'total_new_to_genbank' = sum(new_genbank),
                      'new_to_ggbn_only' = sum(new_ggbn_only),
                      'new_to_genbank_only' = sum(new_genbank_only),
                      'new_to_both' = sum(new_both),
                      'new_to_neither' = sum(new_neither))
    })

    # Creates summary table for display and download
    summary.df.table <- reactive({
        rank.order <- c('kingdom', 'phylum', 'class', 'order', 'family', 'genus')
        select(summary.df(),
               'Rank' = rank,
               'Total' = total,
               'New to GGBN' = total_new_to_ggbn,
               'New to GenBank' = total_new_to_genbank,
               'New to Both GGBN and GenBank' = new_to_both,
               'Already in Both GGBN and GenBank' = new_to_neither)
    })
    
    # Adds summary display table to output
    output$summary.table <- renderTable({ summary.df.table() })
    
    # Creates results table for display and download
    results.df.table <- reactive({
        results.df() %>%
        mutate(in_ggbn = ifelse(is.na(phylum_ggbn), "No", "Yes"),
               in_genbank = ifelse(is.na(phylum_genbank), "No", "Yes"),
               name_status = replace(name_status, is.na(name_status), "NOT FOUND")) %>%
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
    })
    
    # Adds results table to output
    output$results.table <- renderTable({
        if (nrow(results.df.table()) > 100) {
            return(results.df.table()[1:100,])
        } else {
            return(results.df.table())
        }
    })
    
#-----------------------------------------------------------------------------#
# DOWNLOAD BUTTONS
    
    # Download summary table and results table as Excel file
    output$dl.all.xlsx <- downloadHandler(
        filename = function() {
            paste(input$list.name, "Gap Analysis.xlsx", sep = ' ')
        },
        content = function(file) {
            wb <- createWorkbook(creator = "GGI Gap Analysis Tool")
            addWorksheet(wb, "summary")
            writeData(wb, sheet = 'summary', summary.df.table(), rowNames = FALSE)
            addWorksheet(wb, "results")
            outputtable <- arrange(results.df.table(),
                                   Kingdom, Phylum, Class, Order, 
                                   Family, Genus)
            writeData(wb, sheet = 'results', outputtable, rowNames = FALSE)
            saveWorkbook(wb, file = file)
        }
    )
    
    # Download results table as .tsv file
    output$dl.table.tsv <- downloadHandler(
        contentType = 'text/plain',
        filename = function() {
            paste(input$list.name, "Gap Analysis.tsv", sep = ' ')
        },
        content = function(file) {
            outputtable <- arrange(results.df.table(),
                                   Kingdom, Phylum, Class, Order, 
                                   Family, Genus)
            write.table(outputtable, file, row.names = FALSE, 
                        quote = FALSE, na = '', sep = '\t')
        }
    )
    
    # Download summary table as .tsv file
    output$dl.summary.tsv <- downloadHandler(
        contentType = 'text/plain',
        filename = function() {
            paste(input$list.name, "Summary.tsv", sep = ' ')
        },
        content = function(file) {
            write.table(summary.df.table(), file, row.names = FALSE, 
                        quote = FALSE, na = '', sep = '\t')
        }
    )
    
#-----------------------------------------------------------------------------#
# VENN DIAGRAMS - FOR FUTURE DEVELOPMENT
    
    # Label 
    # graph.label <- eventReactive(input$analyze, { return(input$list.name) })
    
    # # Generates Venn diagrams at each taxonomic level depending on results.df
    # pfig <- reactive({
    #     hist(rnorm(100, mean = 10, sd = 1), 
    #          main = paste(graph.label(), '- Phyla', sep = " "),
    #          xlab = 'Phyla')})
    # cfig <- reactive({
    #     hist(rnorm(100, mean = 20, sd = 1), 
    #          main = paste(graph.label(), '- Classes', sep = " "),
    #          xlab = 'Classes')})
    # ofig <- reactive({
    #     hist(rnorm(100, mean = 30, sd = 1), 
    #          main = paste(graph.label(), '- Orders', sep = " "),
    #          xlab = 'Orders')})
    # ffig <- reactive({
    #     hist(rnorm(100, mean = 40, sd = 1), 
    #          main = paste(graph.label(), '- Families', sep = " "),
    #          xlab = 'Families')})
    # gfig <- reactive({
    #     hist(rnorm(100, mean = 50, sd = 1), 
    #          main = paste(graph.label(), '- Genera', sep = " "),
    #          xlab = 'Genera')})
    # 
    # # Adds Venn diagrams to the output
    # output$venn.phyla <- renderPlot({ pfig() })
    # output$venn.classes <- renderPlot({ cfig() })
    # output$venn.orders <- renderPlot({ ofig() })
    # output$venn.families <- renderPlot({ ffig() })
    # output$venn.genera <- renderPlot({ gfig() })
    
    # # Download Venn diagrams
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