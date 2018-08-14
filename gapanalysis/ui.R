fluidPage(theme = "style.css", title = 'GGI Gap Analysis Tool',
          
    # Title
    titlePanel(
        fluidRow(
            column(1, img(src = "ggilogo.png", height = "60px")),
            column(11, h1("GGI Gap Analysis Tool"))
        )
    ),
    
    sidebarLayout(
        
        # Options menu on sidebar
        sidebarPanel(
            width = 2,
            h2('Input Names'),
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
            selectInput(
                # width = '200px',
                'inp.nstatus', 
                'Status of names', 
                c(
                    'Not Specified' = 'All',
                    sort(unique(gbif$status))
                )
            ),
            actionButton('analyze', 'Run Analysis')
        ),
        
        # Results tabs
        mainPanel(
            h2('Results'),
            tabsetPanel(id = 'resultstabs', type = 'tabs',
                       
                tabPanel("Instructions",
                         h3("Input Names"),
                         p("Note: This gap analysis calculator only works on taxonomic names at
                           the kingdom, phylum/division, class, order, family, or genus level. 
                           Species are not supported."),
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
                         h3("Results"),
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
                         )
                
                # tabPanel("Summary",
                #          downloadButton('dl.summary',
                #                         'Download Summary Table',
                #                         style = "margin-bottom:1em;margin-top:1em"),
                #          tableOutput('summary.table')),
                # 
                # tabPanel("Results Table",
                #          downloadButton('dl.table',
                #                         'Download Results Table',
                #                         style = "margin-bottom:1em;margin-top:1em"),
                #          div(tableOutput('results.table'), style = "font-size:80%"))
                
                # tabPanel("Figures",
                #          # downloadButton('dl.figures',
                #          #                'Download Figures',
                #          #                style = "margin-bottom:1em;margin-top:1em"),
                #          plotOutput('venn.phyla'),
                #          plotOutput('venn.classes'),
                #          plotOutput('venn.orders'),
                #          plotOutput('venn.families'),
                #          plotOutput('venn.genera')))
                #          )
        ))),
    
    # Footer
    p('For assistance with this application, please contact ggi@globalgeno.me.',
      style = "margin-bottom:3em")
)