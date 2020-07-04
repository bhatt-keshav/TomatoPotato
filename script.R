##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##  This script points to all the sub-scripts and the idea is that if this
##  script is run from top to bottom all analysis can be performed        
##  automatically                                                         
##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


##------------------- Set Working Directory ------------------- ##
setwd('C:/TomatoPotato')

##-------- Load essentials = Libraries and Functions  --------- ##
source('dependencies.R')

##-------- Load country website scrapers (only one time) --------- ##
##-- All relevant data has been fetched and stored in .rds files --##

# source('scraperNL.R') # Scraper of smulweb.nl
# source('scraperIT.R') # Scraper of giallozafferano.it

##------------------------ Load data objects -------------------##
### Load Netherlands data ###

# URLs of recipes fetches per page of the website (optional)
# pages <- readRDS('recipeURLs.rds')

ingredients <- readRDS('data/ingredients.rds')
ingredientsClean <- readRDS("data/ingredientsClean.rds")
recipeCategory <- readRDS('data/recipeCategory.rds')
recipeNames <- readRDS("data/recipeNames.rds")

### Load Italy data ###
italianFood <- readRDS('italianFood.rds')
catsITDF <- readRDS('catsITDF.rds')
# load('.Rdata')

