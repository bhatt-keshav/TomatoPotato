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
saveRDS(pagesIT, 'ITRecipeAndURLs.rds')

## Ingredients of the recipes from these pages
# There are 350779 recipes, fetching all of them with my current resources is not
# possible. But I do have ingredients from 5025 recipes (1.44% of total)
# percentage wise this is not huge, but is statistically significant 
# at 95% conf level and margin of error = 1.3% (https://www.surveymonkey.com/mp/sample-size-calculator/)


# Below code gets the ingredients mentioned in a recipe URL
# This is has been run already thus is toggled to off = 0
getIngredientsPerPage <- function(toggle) {
  if(toggle == 0) {
    # This is just created to stop the fetching
    return(NA)
  } else { 
    # This is the real fetching part, when toggle != 0
    c <- 0
    for (p in pages[1:10]) {
      # doing so, creates a named list per URL, which is quite useful
      ingredients[p] <- sapply(p, getIngredientsAndErrors)
      c <- c + 1
      print(c)
      saveRDS(ingredients, 'ingredients.rds')
    }
  }
  # c ## return(c) is implied here, but not needed so commented
}
# We set the toggle to zero, so as to not run the fetching
getIngredientsPerPage(toggle=0)

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



