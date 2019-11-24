# Libraries
library('rvest')
library('tidyverse')
library(RCurl) 

countries <- c('NL', 'DE')
websites <- c('https://www.24kitchen.nl/recepten/zoeken', 'https://www.lecker.de/rezepte')
# rm(list=ls(all=TRUE))

# TODO: This doesn't work very well as there is no way to get country results
# for now do manually
# get_first_google_link <- function(name, root = TRUE) {
#   url = URLencode(paste0("https://www.google.com/search?q=",name))
#   page <- xml2::read_html(url)
#   # extract all links
#   nodes <- rvest::html_nodes(page, "a")
#   links <- rvest::html_attr(nodes,"href")
#   # extract first link of the search results
#   link <- links[startsWith(links, "/url?q=")][1]
#   # clean it
#   link <- sub("^/url\\?q\\=(.*?)\\&sa.*$","\\1", link)
#   # get root if relevant
#   if(root) link <- sub("^(https?://.*?/).*$", "\\1", link)
#   link
# }
# 
# get_first_google_link('ricette')

getRecipes <- function(link) {
  webpage <- read_html('https://www.smulweb.nl/recepten?page=1')
  links <- webpage %>% html_nodes("a") %>% html_attr('href')
  recipes <- grep('https://www.smulweb.nl/recepten/', links, perl = T, value = T) %>% unique(.)
  # recipes <- recipes[which(recipes != "/recepten/zoeken")]
  return(recipes)
}

base <-  "https://www.smulweb.nl/recepten?page="
df <- data.frame(page = character(0), recipes = character(0))
bases <- rep(base, times = 9888)

# TODO this in a R way
for (i in 1:5) {
  link <- paste0(base, i)
  getRecipes(link)
}

