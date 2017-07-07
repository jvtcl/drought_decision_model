## Load Libraries
install.packages("RMySQL", type = "source")
library(plyr)
library(dplyr)
library(shiny)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(shinyjs)
library(shinyBS)
library(data.table)
library(ggplot2)
library(magrittr)
library(readr)
library(V8)
library(RMySQL)
library(scales)

## gird_base only needs to be loaded if we're going to be doing any raster stuff
# load("data/grid_base.RData") # Load base grid data
# load("data/insurance_base.RData") # Load base insurance data

# Load gridded precip
# Get the gridded precip file if not in 'data' folder
# ideally I can get this to work with git-lfs in the future...

# if(!"noaaIndex.RData" %in% list.files("data")){
#   cat("No NOAA Index data found. Fetching the files...\n")
#   download.file("https://o365coloradoedu-my.sharepoint.com/personal/jotu9073_colorado_edu/_layouts/15/guestaccess.aspx?guestaccesstoken=CLfTQ%2fVIj3v7cel5UkM65Noqx%2bHEgIDoS0lzqM5FHUw%3d&docid=08a40bf084add47629568d4d4161e283e&rev=1",destfile="data/noaaIndex.RData")
# }
load("data/noaaIndex_shiny.RData")


## Miscellaneous supporting var's/data
# State code - for pulling COOP sites & mlra forage weights
if(!exists("wrc.state")){
  wrc.state="co"
}

# # Shortcut for sourcing 'R/coop_scraper.R'
# if(!exists("coops")){
#   load("data/coops.RData") 
# }

# # load MLRA zone data
# if(!exists("mlra")){
#   mlra=readOGR("data","mlra_v42")
# }

