### Scraping ###

## Retreive the URLs of all recipes listed per page

# There are some 9888 total pages on the website, 
# with each page holding links to various recipes

# Each page's URL is base URL + number
pageIT <- character()
base <-  "https://www.giallozafferano.it/ricette-cat/page"
for (i in seq(101, 350)) {
  print(i)
  # atPage is the page of the website we are on right now
  atPage <- paste0(base, i, '/')
  pageIT[i] <- lapply(atPage, getRecipesIT)
  }
  
# saveRDS(pageIT, 'pageIT.rds')
## Retrieval of pages with URLs of recipes in them

pagesIT <- unlist(pageIT)
# The total number of recipes is: 5095, which is almost same as the Dutch count
length(pagesIT)
# saveRDS(pagesIT, 'ITRecipeAndURLs.rds')
pagesIT <- readRDS('ITRecipeAndURLs.rds')

## Ingredients of the recipes from these pages
# Same as for NL, processing is done for ~ 5k (5095) recipes 

# Below code gets the ingredients mentioned in a recipe URL
# Manually and automated

# italianFood <- list() # one-time initialization
foodListNames <- c("category", "ingrds", "about", "name", "url")

counter <- c(1000, 2000, 3000, 4000)
for (c in counter) {
  j <- c + 999
  for (i in seq(c, j)) {
    name <- names(pagesIT[i])
    out <- getEverythingIT(name)
    out[[4]] <- ITrecipeCategory[name]
    out[[5]] <- name
    italianFood[[i]] <- out
    names(italianFood[[i]]) <- foodListNames
    print(i)
  }
  saveRDS(italianFood, 'italianFood.rds')
  Sys.sleep(10)
}  

for (i in seq(5000, 5095)) {
  name <- names(pagesIT[i])
  out <- getEverythingIT(name)
  out[[4]] <- ITrecipeCategory[name]
  out[[5]] <- name
  italianFood[[i]] <- out
  names(italianFood[[i]]) <- foodListNames
  print(i)
}
# saveRDS(italianFood, 'italianFood.rds')


