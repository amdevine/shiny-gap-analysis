navbarPage("GGI Gap Analysis Tool", theme = 'style.css', id = 'tabs',

    # tabPanel(id = 'instructions', title = "Instructions",
    #     h1("Instructions"),
    #     p(strong("Note: This gap analysis calculator only works on taxonomic names at
    #         the kingdom, phylum/division, class, order, family, or genus level. 
    #              Species are not supported.")),
    #     h2("Input"),
    #     p('This tab contains fields to input taxonomic names and specify name options and filtering options.'),
    #     p(strong("Dataset label:"), 
    #         "Enter the name of analysis. e.g. \'NMNH Amphibians\'"),
    #     p(strong("Upload list of names:"), 
    #         "Allows text (.txt) file containing a list of names to be upload for analysis. 
    #         Please format text file with one name per line."),
    #     p(strong("Enter list of names:"), 
    #         "Alternatively, enter a list of names to be analyzed.
    #         Please enter one name per line."),
    #     p(strong("Taxonomic rank of names:"), 
    #         "If all names are the same taxonomic rank (e.g. Family, Genus),
    #         select the appropriate rank. This may reduce the number of
    #         extraneous results returned from incorrect ranks."),
    #     p(strong("Kingdom of names:"), 
    #         "If all names are the found in the same kingdom (e.g. Animalia, Plantae), 
    #         select the appropriate kingdom. This may reduce the number of 
    #         extraneous results from homonymous names."),
    #     p(strong("Name status:"), 
    #         "Filter results based on the status of the names returned. e.g. Selecting 
    #         \'accepted\' will include results only for accepted names that match the 
    #         input names, and will not return any matching synonyms or unaccepted name 
    #         alternatives."),
    #     h2("Results"),
    #     p("Submitted names are queried in static inventory lists from GBIF, GGBN,
    #         and GenBank. Names are first matched to GBIF, which returns the taxonomic
    #         rank, name status, and if the name is not accepted, the accepted name.
    #         Then the accepted name is queried in GGBN and GenBank, which returns whether 
    #         that name is found in those databases. The taxonomic hierarchy is provided 
    #         for each name to allow the user to verify that the correct name has been queried."),
    #     p(strong("Summary:"),
    #         "A summary of the names entered, and how many were found to be in GBIF,
    #         GGBN, and GenBank. The \'Download Summary Table\' button allows the user to 
    #         download the counts for each taxonomic rank as a tab-delimited .tsv file."),
    #     p(strong("Results Table:"),
    #         "Returns a table containing results of the first 100 names submitted. 
    #         The \'Download Results Table\' button allows the user to download the results 
    #         for all submitted names as a tab-delimited .tsv file.")
    # ),
    
    tabPanel(id = "inp", title = "Input",
        fluidRow(
            column(4,
                h2("Input Names"),
                p(tags$small('Names can be uploaded in a file or pasted directly into the text box.')),
                p(tags$small('Only names at the kingdom, phylum/division, class, order, family, or genus rank 
                         can be analyzed.', strong('This tool does not work on species names.'))),
                hr(),
                h3('Upload name file'),
                p(tags$small('Upload a .txt file containing one name per line.')),
                fileInput('inp.name.file', 'Names file (.txt)'),
                hr(),
                h3('Paste list of names'),
                p(tags$small('Paste a list of names directly into this text box. Please enter one name per row.')),
                textAreaInput(
                   'inp.name.list', 
                   'List of names',
                   placeholder = 'One name per row',
                   # width = '150px',
                   height = '100px'
                ),
                tags$div(class = 'analyzebutton',
                    h2("Run Analysis"),
                    actionButton('analyze', 'Run Analysis'),
                    p(tags$small('After clicking Run Analysis, manually click on 
                                 Results Summary of Results Table to view results.'))
                )
            ),
            column(4,
                h2("Name Options"),
                p(tags$small("These options can be specified for the list of input names.")),
                hr(),
                h3("Taxonomic rank of submitted names"),
                p(tags$small('Select the taxonomic rank of the names submitted. E.g. If submitting a 
                  list of all families, select', strong('Family'), 'as the', 
                  strong('Taxonomic rank of submitted names'))),
                selectInput(
                    # width = '200px,',
                    'inp.taxlevel', 
                    'Taxonomic rank of submitted names', 
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
                hr(),
                h3("Dataset label"),
                p(tags$small("This label is used to name the download files.")),
                textInput('list.name', 'Dataset label', value = "User Data")
            ),
            column(4,
                h2("Output Filtering"),
                p(tags$small("These options filter the output of the gap analysis.")),
                hr(),
                h3('Filter by names status'),
                p(tags$small("Filter results based on the GBIF name status. E.g. To receive only the results 
                  matching accepted GBIF names, select", strong("accepted"), "from", 
                  strong("Name status"))),
                selectInput(
                    # width = '200px',
                    'filter.nstatus', 
                    'Name status', 
                    c(
                        'Not Specified' = 'All',
                        sort(unique(gbif$status))
                    )
                ),
                hr(),
                h3("Filter by taxon"),
                p(tags$small("These options allow for filtering by a specified taxonomic groups. e.g. If submitting a 
                  list of mammal families, select", strong("Class"), "from", strong("Select filter rank"),
                  ", then select", strong("Mammalia"), "from", strong("Select filter taxonomic name"))),
                selectInput(
                    'filter.rank',
                    'Select filter rank',
                    c(
                        'Not Specified' = 'All',
                        'Kingdom' = 'kingdom',
                        "Phylum/Division" = 'phylum',
                        'Class' = 'class',
                        'Order' = 'order',
                        'Family' = 'family'
                    )
                ),
                selectInput(
                    'filter.name',
                    'Select filter taxonomic name',
                    c('Not Specified' = 'All')
                )
            )
        )
    ),
    
    tabPanel(id = "summary", title = "Results Summary",
             h2('Results Summary'),
             tableOutput('summary.table')),
    
    tabPanel(id = "table", title = "Results Table",
        h2("Results Table"),
                 div(tableOutput('results.table'), style = "font-size:80%")),
    
    tabPanel(id = "download", title = "Download Results",
        h2("Download Results"),
        downloadButton('dl.all.xlsx',
                       'Download All Results (.xlsx)',
                       style = "margin-bottom:1em;margin-top:1em"),
        downloadButton('dl.summary.tsv', 
                       'Download Summary Table (.tsv)', 
                       style = "margin-bottom:1em;margin-top:1em"),
        downloadButton('dl.table.tsv', 
                       'Download Results Table (.tsv)', 
                       style = "margin-bottom:1em;margin-top:1em"))

)