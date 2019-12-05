### Libraries
library('rvest')
library('tidyverse')
library('tm')
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
  pauseFetching(2)
}

getIngredients <- function(link) {
  webpage <- read_html(link)
  ingredients <- webpage %>% html_nodes('div.ingredienten>p') %>% html_text()
  ingredients <- paste(ingredients, collapse = ' ')
  ingredients <- str_replace_all(ingredients, "[\r\n]" , " ") %>% str_replace_all(., '[0-9]+', "")
  ingredients <- str_extract_all(ingredients, "[a-zA-Z]+")
  pauseFetching(2)
  return(ingredients)
}

getRecipesAndErrors <- function(url) {
  recipes <- tryCatch(
    {
      getRecipes(url)
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

getIngredientsAndErrors <- function(url) {
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

page <- character()
for (i in seq(1, 9888)) {
  print(i)
  atPage <- paste0(base, i)
  page[i] <- lapply(atPage, getRecipesAndErrors)
}
# contains results from pg 1 - 100

# These pages have been retreived
page0 <- readRDS('page.rds')
page <- readRDS('page.rds')
pages <- c(page, page0)
pages <- unlist(pages)
pages <- unique(pages) #346780

# Ingredients of the recipes from these pages
# TODO: Put this on a VM, for now I have ingredients from 5025 recipes

ingredients <- character()
c <- 0
for (p in pages) {
  ingredients[p] <- sapply(p, getIngredientsAndErrors)
  c <- c + 1
  print(c)
}

saveRDS(ingredients, 'ingredients.rds')
# TODO: Is a df even necessary?

df <- data.frame(page = page)
df$page <- as.character(df$page)

# TODO: Get tags e.g. engels, indo...

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
ingrdCounts <- freq_terms(unlist(ingredients,  use.names = F), 30)

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




