navbarPage("GGI Gap Analysis Tool", theme = 'style.css', id = 'tabs',

    # Input tab
    tabPanel(id = "inp", title = "Input", value = "inp",
        fluidRow(
            
            # Inputs column
            column(4,
                
                # Input names information
                h2("Input Names"),
                p(tags$small('Names can be uploaded in a file or pasted directly into the text box.')),
                p(tags$small('Only names at the kingdom, phylum/division, class, order, family, or genus rank 
                         can be analyzed.', strong('This tool does not work on species names.'))),
                hr(),
                
                # Upload name file
                h3('Upload name file'),
                p(tags$small('Upload a .txt file containing one name per line.')),
                fileInput('inp.name.file', 'Names file (.txt)'),
                hr(),
                
                # Paste names in text box
                h3('Paste list of names'),
                p(tags$small('Paste a list of names directly into this text box. Please enter one name per row.')),
                textAreaInput(
                   'inp.name.list', 
                   'List of names',
                   placeholder = 'One name per row',
                   # width = '150px',
                   height = '100px'
                ),
                hr(),
                
                # Analyze names button
                h2("Run Analysis"),
                actionButton('analyze', 'Run Analysis'),
                p(tags$small('After clicking Run Analysis, you will automatically
                             be redirected to the Results Table tab.'))
            ),
            
            # Name options column
            column(4,
                   
                # Name options info
                h2("Name Options"),
                p(tags$small("These options can be specified for the list of input names.")),
                hr(),
                
                # Taxonomic rank dropdown 
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
                
                # Name analysis
                h3("Analysis name"),
                p(tags$small("This name is used to name the download files.")),
                textInput('list.name', 'Analysis name', value = "Gap Analysis")
            ),
            
            # Output filtering column
            column(4,
                   
                # Output filtering info
                h2("Output Filtering"),
                p(tags$small("These options filter the output of the gap analysis.")),
                hr(),
                
                # Name status dropdown
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
                
                # Taxon filter autocomplete
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
    
    # Summary tab
    tabPanel(id = "summary", title = "Results Summary", value = "summary",
             h2('Results Summary'),
             tableOutput('summary.table')),
    
    # Results table tab
    tabPanel(id = "table", title = "Results Table", value = "table",
        h2("Results Table"),
        div(tableOutput('results.table'), style = "font-size:80%")),
    
    # Download tab
    tabPanel(id = "download", title = "Download Results", value = "download",
        h2("Download Results"),
        downloadButton('dl.all.xlsx',
                       'Download All Results (.xlsx)',
                       style = "margin-bottom:1em;margin-top:1em"),
        downloadButton('dl.summary.tsv', 
                       'Download Summary Table (.tsv)', 
                       style = "margin-bottom:1em;margin-top:1em"),
        downloadButton('dl.table.tsv', 
                       'Download Results Table (.tsv)', 
                       style = "margin-bottom:1em;margin-top:1em")),
    
    # About tab
    tabPanel(id = "about", title = "About", value = "about",
        h2("About The Gap Analysis Tool"),
        includeMarkdown("docs/about.md"))

)