### Analysis
# Loading the URLs of recipes fetches per page of the website (optional)
# pages <- readRDS('recipeURLs.rds')

# Loading the fetched ingredients
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
