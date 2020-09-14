## To deploy app through RStudio:
## library(rsconnect)
## deployApp('gapanalysis')

# Import packages 
suppressPackageStartupMessages(suppressWarnings(library(tidyverse)))
suppressPackageStartupMessages(suppressWarnings(library(lubridate)))
library(rdrop2)
library(shiny)
library(openxlsx)
library(markdown)

# Read token to connect to source data Dropbox account
token <- drop_auth(rdstoken = 'dropbox_token_2.rds')

# Function that returns date the files were last modified
update_date <- function(file_info, file_name){
    
    # Create datestamp template
    last_updated <- stamp("last updated 28 August 2020", 
                          orders = "dBY", quiet = TRUE)
    
    # Get datetime for file
    d <- file_info %>% 
         filter(name == file_name) %>% 
         select(client_modified) %>%
         .[[1]] %>%
         ymd_hms()
    
    # Return template applied to file datetime
    return(last_updated(d))
}

development <- FALSE

if (development) {
    
    # Read files locally - FOR USE DURING DEVELOPMENT
    gbif <- read.csv('../gbif.csv', stringsAsFactors = FALSE)
    ggbn <- read.csv('../ggbn.csv', stringsAsFactors = FALSE)
    genbank <- read.csv('../genbank.csv', stringsAsFactors = FALSE)
    
} else if (!development) {
    
    # Read files from Dropbox - FOR USE ON LIVE SITE
    gbif <- drop_read_csv('shiny/gbif.csv', dtoken = token,
                          stringsAsFactors = FALSE)
    ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token,
                          stringsAsFactors = FALSE)
    genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token,
                             stringsAsFactors = FALSE)
}

# Read update dates for files
file_info <- drop_dir('shiny')
gbif_date <- update_date(file_info, "gbif.csv")
ggbn_date <- update_date(file_info, "ggbn.csv")
genbank_date <- update_date(file_info, "genbank.csv")

# Select appropriate columns for each dataset
gbif <- distinct(gbif)

ggbn <- ggbn[, -1]
ggbn$rank <- tolower(ggbn$rank)

genbank <- genbank[, -1]
genbank$rank <- tolower(genbank$rank)