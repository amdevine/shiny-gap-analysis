navbarPage("GGI Gap Analysis Tool", theme = 'style.css', id = 'tabs',

    tabPanel(id = 'instructions', title = "Instructions",
        h1("Instructions"),
        h2("Input"),
        p(strong("Note: This gap analysis calculator only works on taxonomic names at
            the kingdom, phylum/division, class, order, family, or genus level. 
            Species are not supported.")),
        p(strong("Dataset label:"), 
            "Enter the name of analysis. e.g. \'NMNH Amphibians\'"),
        p(strong("Upload list of names:"), 
            "Allows text (.txt) file containing a list of names to be upload for analysis. 
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
        p(strong("Name status:"), 
            "Filter results based on the status of the names returned. e.g. Selecting 
            \'accepted\' will include results only for accepted names that match the 
            input names, and will not return any matching synonyms or unaccepted name 
            alternatives."),
        h2("Results"),
        p("Submitted names are queried in static inventory lists from GBIF, GGBN,
            and GenBank. Names are first matched to GBIF, which returns the taxonomic
            rank, name status, and if the name is not accepted, the accepted name.
            Then the accepted name is queried in GGBN and GenBank, which returns whether 
            that name is found in those databases. The taxonomic hierarchy is provided 
            for each name to allow the user to verify that the correct name has been queried."),
        p(strong("Summary:"),
            "A summary of the names entered, and how many were found to be in GBIF,
            GGBN, and GenBank. The \'Download Summary Table\' button allows the user to 
            download the counts for each taxonomic rank as a tab-delimited .tsv file."),
        p(strong("Results Table:"),
            "Returns a table containing results of the first 100 names submitted. 
            The \'Download Results Table\' button allows the user to download the results 
            for all submitted names as a tab-delimited .tsv file.")
    ),
    
    tabPanel(id = "inp", title = "Input",
        fluidRow(
            column(4,
                h2("Input Names"),
                textInput('list.name', 'Dataset label', value = "User Data"),
                fileInput('inp.name.file', 'Upload list of names'),
                textAreaInput(
                   'inp.name.list', 
                   'OR enter list of names',
                   placeholder = 'One name per row',
                   # width = '150px',
                   height = '100px'
                ),
                actionButton('analyze', 'Run Analysis')
            ),
            column(4,
                h2("Name Options"),
                p("These options can be specified for the list of names being inputted."),
                h3("Taxonomic rank of submitted names"),
                p('Select the taxonomic rank of the names submitted. E.g. If submitting a 
                  list of all families, select', strong('Family'), 'as the', 
                  strong('Taxonomic rank of submitted names')),
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
                )
            ),
            column(4,
                h2("Output Filtering"),
                p("These options filter the output of the gap analysis."),
                h3('Name Status'),
                p("Filter results based on the GBIF name status. E.g. If you would like only results 
                  matching accepted GBIF names, select", strong("accepted"), "from", 
                  strong("Name status")),
                selectInput(
                    # width = '200px',
                    'filter.nstatus', 
                    'Name status', 
                    c(
                        'Not Specified' = 'All',
                        sort(unique(gbif$status))
                    )
                ),
                h3("Filter by taxonomic rank and name"),
                p("These options allow for filtering by certain taxonomic groups. e.g. If submitting a 
                  list of mammal families, select", strong("Class"), "from", strong("Select filter rank"),
                  ", then select", strong("Mammalia"), "from", strong("Select filter taxonomic name")),
                selectInput(
                    'filter.rank',
                    'Select filter rank',
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