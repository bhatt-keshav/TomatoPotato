## Libraries neeeded 
library('rvest')
library('tidyverse')
library('SnowballC')
library('tm')
library('qdap')

## Functions needed for script to work
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

# This function is like the tm::removeWords 
removeStopWords <- function(from_vec, stopwords) {
  setdiff(from_vec, stopwords)
}
