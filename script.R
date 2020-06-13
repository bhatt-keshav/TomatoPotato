### This script is not really meant to be run, but the idea is that if this script is run from top to bottom all analysis can be performed automatically

### Directory
# setwd('/home/kiki/TomatoPotato')
setwd('C:/TomatoPotato')

### Load essentials = Libraries and Functions
source('dependencies.R')

### Load objects
recipeCategory <- readRDS('recipeCategory.rds')
italianFood <- readRDS('italianFood.rds')
### Load scraper of smulweb.nl (one time, not needed)
# source('scraperNL.R')

