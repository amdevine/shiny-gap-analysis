library(dplyr, warn.conflicts = FALSE)
library(rdrop2)
library(shiny)
library(tidyr, warn.conflicts = FALSE)
library(openxlsx)
# library(VennDiagram)

# Read files locally - FOR USE DURING DEVELOPMENT
# gbif <- read.csv('../gbif.csv', stringsAsFactors = FALSE)
# ggbn <- read.csv('../ggbn.csv', stringsAsFactors = FALSE)
# genbank <- read.csv('../genbank.csv', stringsAsFactors = FALSE)

# Read files from Dropbox - FOR USE ON LIVE SITE
token <- readRDS('dropbox_token.rds')
gbif <- drop_read_csv('shiny/gbif.csv', dtoken = token, stringsAsFactors = FALSE)
ggbn <- drop_read_csv('shiny/ggbn.csv', dtoken = token, stringsAsFactors = FALSE)
genbank <- drop_read_csv('shiny/genbank.csv', dtoken = token, stringsAsFactors = FALSE)


# Select appropriate columns for each dataset
gbif <- distinct(gbif)

ggbn <- ggbn[, -1]
ggbn$rank <- tolower(ggbn$rank)

genbank <- genbank[, -1]
genbank$rank <- tolower(genbank$rank)