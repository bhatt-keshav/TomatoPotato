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
# Step 1: 1-999

c <- 0

ITrecipeCategory <- pagesIT
for (i in seq(1, 3)) {
  name <- names(pagesIT[i])
  put <- getEverythingIT(name)
  put[[4]] <- ITrecipeCategory[name]
  put[[5]] <- name
  c <- c + 1
  print(c)
}

outlist <- list()
outlist[[1]] <- put
outlist[[2]] <- p

outlist[[2]][3]

outlist <- append(outlist,list(resultsc))


saveRDS(ITrecipeCategory, 'ITrecipeCategory.rds')

n <- c(1000, 2000, 3000, 4000)
c <- n[1]
for (i in n) {
  j <- i + 999
  for (r in recipeURLs[i:j]) {
    # doing so, creates a named list per URL, which is quite useful
    recipeCategory[r] <- sapply(r, getRecipeCategoryAndErrors)
    c <- c + 1
    print(c)
  }  
}

# Fetching recipe categories e.g. indo, hoofdgerecht, spaans...
recipeCategory <- list()
# Step 1 1- 2000 manually
# Step 2 2000-4999 for loop
n <- c(2000, 3000, 4000)
c <- n[1]
for (i in n) {
  j <- i + 999
  for (r in recipeURLs[i:j]) {
    # doing so, creates a named list per URL, which is quite useful
    recipeCategory[r] <- sapply(r, getRecipeCategoryAndErrors)
    c <- c + 1
  }  
}

# Step 3 4999-5025 manually
for (r in recipeURLs[4999:5025]) {
  # doing so, creates a named list per URL, which is quite useful
  recipeCategory[r] <- sapply(r, getRecipeCategoryAndErrors)
}
saveRDS(recipeCategory, 'recipeCategory.rds')



# TODO:
# https://ricette.giallozafferano.it/Besciamella.html
# https://ricette.giallozafferano.it/Profiteroles-al-cioccolato.html
# https://ricette.giallozafferano.it/Ravioli-cinesi-al-vapore.html
