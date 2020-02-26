### Scraping ###

### Retreive the URLs of all recipes listed per page
# There are some 9888 total pages on the website, 
# with each page holding links to various recipes

# Each page's URL is base URL + number
base <-  "https://www.smulweb.nl/recepten?page="

page <- character()
for (i in seq(1, 9888)) {
  print(i)
  # atPage is the page of the website we are on right now
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
saveRDS(pages, 'recipeURLs.rds')

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




