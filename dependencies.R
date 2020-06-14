## Libraries neeeded 
library('rvest')
library('tidyverse')
library('SnowballC')
library('tm')
library('qdap')
library('rlist')
library("xlsx")
library('maps')
library('entity')
# library('textstem')
# library('koRpus')
# library('ggplot2')

## Functions needed for script to work
'%ni%' <- Negate('%in%')

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

getRecipesIT <- function(link) {
  webpage <- read_html(link)
  recipeLinks <- webpage %>% html_nodes(".gz-title a") %>% html_attr("href")
  recipeNames <- webpage %>% html_nodes(".gz-title a") %>% html_text()
  pageIT <- vector(mode="list", length=length(recipeNames))
  pageIT[] <- as.list(recipeNames)
  names(pageIT) <- recipeLinks  
  return(pageIT)
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

getEverythingIT <- function(link) {
  webpage <- read_html(link)
  category <- webpage %>% html_nodes('.gz-breadcrumb ul') %>% html_text
  category <- tolower(category) %>% str_replace_all(., "[\r\n\t]" , " ") %>%   str_replace_all(., '[0-9]+', "")
  category <- paste(category, collapse = "")
  category <- trimws(category)
  category <- strsplit(category, "  ")
  category <- unlist(category)
  category <- setdiff(category, "") 
  
  ingredients <- webpage %>% html_nodes('.gz-ingredient a') %>% html_text()
  ingredients <- tolower(ingredients)
  
  about <- webpage %>% html_nodes(".open-podcast+ p") %>% html_text()
  about <- tolower(about)
  
  output <- list()
  output[[1]] <- category
  output[[2]] <- ingredients
  output[[3]] <- about
  return(output)
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

getRecipeCategory <- function(link) {
  category <- read_html(link) %>% html_node(".descr_shrt li:nth-child(1)") %>% html_text() %>% str_trim(.)
  category <- gsub("[^A-Za-z]+", " ", category) %>% tolower() %>%
    strsplit(., " ")
  return(category)
}

getRecipeCategoryAndErrors <- function(url) {
  out <- tryCatch(
    {
      getRecipeCategory(url)
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

# flattens an Italian list, that has already been extracted by name
extractListText <- function(selectedList, listName, trim) {
  # inputs
  # listName: text '', listOfLists: expected = italianFood
  selectedList <- as.vector(unlist(selectedList))
  selectedListDF <- data.frame(listName = selectedList, 'freq'=1)
  selectedListDF <- selectedListDF %>% group_by(listName) %>% summarise(freq=sum(freq)) %>% arrange(freq)
  selectedListDF$freq <- (selectedListDF$freq*100)/(max(selectedListDF$freq))
  # Trims the last last 100 rows (most popular), if TRUE
  if (trim == TRUE) {
    # total nr of rows
    rown <- dim(selectedListDF)[1]
    # if a big df, 
    if (rown > 105) {
      selectedListDF <- selectedListDF[(rown -100): rown, ]
    }  
  }
  return(selectedListDF)
}


