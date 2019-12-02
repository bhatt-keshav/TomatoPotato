## Libraries
library('rvest')
library('tidyverse')
library('tm')
# library('RCurl') 

# TODO: Maybe this can be made automatic
# countries <- c('NL', 'DE')
# websites <- c('https://www.smulweb.nl/recepten?page=', 'https://www.lecker.de/rezepte')
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

## Functions
getRecipes <- function(link) {
  webpage <- read_html(link)
  links <- webpage %>% html_nodes("a") %>% html_attr('href')
  recipes <- grep('https://www.smulweb.nl/recepten/[0-9]', links, perl = T, value = T) %>% unique(.)
  return(recipes)
}

pauseFetching <- function(secs){
  Sys.sleep(secs) #pause to let connection work
  closeAllConnections()
  gc()
}

getIngredients <- function(link) {
  webpage <- read_html(link)
  ingredients <- webpage %>% html_nodes('div.ingredienten>p') %>% html_text()
  ingredients <- paste(ingredients, collapse = ' ')
  ingredients <- str_replace_all(ingredients, "[\r\n]" , " ") %>% str_replace_all(., '[0-9]+', "")
  ingredients <- str_extract_all(ingredients, "[a-zA-Z]+")
  pauseFetching(1)
  return(ingredients)
}

################

base <-  "https://www.smulweb.nl/recepten?page="

# It is wiser to do with 2 pages for now, 9888 takes too long
# Each page has 40 odd results, so it is 80 in total now
# TODO: Change seq(1:2) to 1:9888 and then do it on a VM
bases <- paste0(base, seq(1:2))
page <- lapply(bases, getRecipes) 
page <- unlist(page)

df <- data.frame(page = page)
df$page <- as.character(df$page)

# get name of the recipe
df$recipe <- str_extract(df$page, '[^/]+$') %>% str_replace_all(., '-', ' ')

# created a test instance of the top 5 rows, because there is a sleep command
# till this is not migrated to a VM, do like this
dfTest <- head(df)

# these are the ingredients of the recipes 
dfTest$ingredients <- sapply(dfTest$page, getIngredients)

# TODO: Get most common words and remove these stoppy words
wordCounts <- table(unlist(dfTest$ingredients))
sort(wordCounts)

# TEST and PLAY!
a <- "https://www.smulweb.nl/recepten/126611/Lasagna"
aIngredients <- getIngredients(a)

link <- 'https://www.smulweb.nl/recepten/1319129/Pittige-kipfilet-a-la-pizzaiola'



