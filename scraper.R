### Libraries
library('rvest')
library('tidyverse')
library('tm')
# install.packages('qdap')
library('qdap')
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

### Functions
pauseFetching <- function(secs){
  Sys.sleep(secs) #pause to let connection work
  closeAllConnections()
  gc()
}

getRecipes <- function(link) {
  webpage <- read_html(link)
  links <- webpage %>% html_nodes("a") %>% html_attr('href')
  recipes <- grep('https://www.smulweb.nl/recepten/[0-9]', links, perl = T, value = T) %>% unique(.)
  return(recipes)
}

getRecipesAndErrors <- function(link) {
  recipes <- tryCatch(
    {
      webpage <- read_html(link)
      links <- webpage %>% html_nodes("a") %>% html_attr('href')
      recipes <- grep('https://www.smulweb.nl/recepten/[0-9]', links, perl = T, value = T) %>% unique(.)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      return(NULL)
    }
  )
  return(recipes)
}

getsIngredientsAndErrors <- function(url) {
  out <- tryCatch(
    {
      getIngredients(url)
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      return(NULL)
    }
  )    
  return(out)
}

###############################

base <-  "https://www.smulweb.nl/recepten?page="

# It is wiser to do with 2 pages for now, 9888 takes too long
# Each page has 40 odd results, so it is 80 in total now
# TODO: Change seq(1:2) to 1:9888 and then do it on a VM
bases <- paste0(base, seq(1:2))
page <- lapply(bases, getRecipesAndErrors) 
page <- unlist(page)

df <- data.frame(page = page)
df$page <- as.character(df$page)

# get name of the recipe
df$recipe <- str_extract(df$page, '[^/]+$') %>% str_replace_all(., '-', ' ')

# created a test instance of the top 5 rows, because there is a sleep command
# till this is not migrated to a VM, do like this
# dfTest <- head(df, 30)

# these are the ingredients of the recipes 
df$ingredients <- sapply(df$page, getsIngredientsAndErrors)
df$ingredients <- sapply(df$ingredients, tolower)

# These are the most common words and should remove these stoppy words
ingrdCounts <- freq_terms(unlist(df$ingredients), 30)

# HERE!
ic <- freq_terms(unlist(df$ingredients), 3000)

ic[ic$WORD == "aardappelen", ]
ic[ic$WORD == "tomaten", ]

ic[order(ic$WORD), ] %>% head(20)

###### TEST and PLAY! #####
a <- "https://www.smulweb.nl/recepten/126611/Lasagna"
getIngredients(a)

bad <- 'https://www.smulweb.nl/avfd'
getIngredients(bad)

urls <- c(a, bad)

y <- sapply(urls, readUrl)
y




