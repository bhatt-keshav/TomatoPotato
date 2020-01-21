### Setup
## Libraries
library('rvest')
library('tidyverse')
library('tm')
library('qdap')
# library('RCurl') 
## Directory
setwd('/home/kiki/TomatoPotato')

# TODO: Scrape from Italy also
# countries <- c('NL', 'IT')
# websites <- c('https://www.smulweb.nl/recepten?page=', 'www.giallozafferano.it')
# rm(list=ls(all=TRUE))

### Functions ###
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

### Script ###

### Scraping
base <-  "https://www.smulweb.nl/recepten?page="
## Retreive the URLs of all recipes listed per page
page <- character()
for (i in seq(1, 9888)) {
  print(i)
  atPage <- paste0(base, i)
  page[i] <- lapply(atPage, getRecipesAndErrors)
}
## These pages have been retreived
page0 <- readRDS('page.rds')
page <- readRDS('page.rds')
# page0 and page do not have their index correct, so it is better to merge and unlist them
# anyway index is irrelevant
pages <- c(page, page0)
pages <- unlist(pages)
# The total number of recipes now is: 346780, which is almost everything at the date of fetching
pages <- unique(pages) 
saveRDS(pages, 'recipeURLs')

## Ingredients of the recipes from these pages
# TODO: Put this on a VM, for now I have ingredients from 5025 recipes out of a total 346780 (1.44%)
ingredients <- character()
c <- 0
for (p in pages) {
  ingredients[p] <- sapply(p, getIngredientsAndErrors)
  c <- c + 1
  print(c)
}
# saveRDS(ingredients, 'ingredients.rds')
ingredients <- readRDS('ingredients.rds')

### Pre-processing
## Remove stopwords

## HERE!!!
# these are the ingredients of the recipes 
# df$ingredients <- sapply(df$ingredients, tolower)

# These are the most common words and should remove these stop words
ingrdCounts <- freq_terms(unlist(ingredients), 50, at.least = 3); ingrdCounts

nlStopwords <- c('aan' , 'af' , 'al' , 'als' , 'bij' , 'dan' , 'dat' , 'die' , 'dit' , 'een' , 'en' , 'er' , 'had' , 'heb' , 'hem' , 'het' , 'hij' , 'hoe' , 'hun' , 'ik' , 'is' , 'je' , 'kan' , 'me' , 'men' , 'met' , 'mij' , 'nog' , 'nu' , 'of' , 'ons' , 'ook' , 'te' , 'tot' , 'uit' , 'van' , 'voor', 'was' , 'we' , 'wel' , 'wij' , 'zal' , 'ze' , 'zei' , 'zij' , 'zo' , 'zou' , 'in' , 'wat', 
                'gr', 'gram', 'zout', 'eetlepels', 'eetl', 'el', 'theelepel', 'eetlepel', 'verse', 'rode', 'grote',                   'gesneden',  'geraspte', 'kleine', 'witte', 'met', 'teentjes', 'plakjes',  'naar',                        'smaak', 'blokjes', 'zakje', 'fijn', 'gedroogde', 'stukjes')

ingredientsClean <- rm_stopwords(ingredients, nlStopwords, strip = TRUE, ignore.case = TRUE, unlist = TRUE)

ingrdCounts1 <- freq_terms(ingredientsClean, 50, at.least = 3); ingrdCounts1

### Analysis

ingredients[1]

ic <- freq_terms(unlist(df$ingredients), 3000)

ic[ic$WORD == "aardappelen", ]
ic[ic$WORD == "tomaten", ]

ic[order(ic$WORD), ] %>% head(20)


# TODO: Is a df even necessary?

df <- data.frame(page = page)
df$page <- as.character(df$page)

# TODO: Get tags e.g. engels, indo...

# get name of the recipe
df$recipe <- str_extract(df$page, '[^/]+$') %>% str_replace_all(., '-', ' ')

# created a test instance of the top 5 rows, because there is a sleep command
# till this is not migrated to a VM, do like this
# dfTest <- head(df, 30)


###### TEST and PLAY! #####
a <- "https://www.smulweb.nl/recepten/126611/Lasagna"
getIngredients(a)

bad <- 'https://www.smulweb.nl/avfd'
getIngredients(bad)

urls <- c(a, bad)

y <- sapply(urls, readUrl)
y




