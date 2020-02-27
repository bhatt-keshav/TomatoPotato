### Analysis ###
# Loading the URLs of recipes fetches per page of the website (optional)
# pages <- readRDS('recipeURLs.rds')

# Loading the fetched ingredients
ingredients <- readRDS('ingredients.rds')

### Text Pre-processing
## Cleaning
# Make all entries lower case, other cleaning such as removing punctuation, nr is not needed as I did that while fetching by keeping only chars
ingredients <- lapply(ingredients, tolower)
# total ingredients are: 9135
unique(unlist(ingredients)) %>% length()

## Remove stopwords
# There are many frequently occuring words which should be removed
ingrdCounts <- freq_terms(unlist(ingredients), top = 100); ingrdCounts
# Of these 100, by manual inspection, these are also kind of stop-words
ingrdCommon <-  c("gram", "gr", "of", "g", "en", "el", "in", "de", "eetlepels", "van", "ml", "voor", "een", "dl", "ui", "tl", "rode", "verse", "grote", "eetl", "eetlepel", "theelepel", "gesneden", "witte", "met", "teentjes", "kleine", "geraspte", "plakjes", "te", "naar", "het", "smaak", "zwarte", "blokjes", "gehakt", "om", "t", "cm", "uit", "ei", "gemalen", "zakje", "fijn", "gedroogde", "stukjes", "theel", "blikje", "fijngesneden", "liter", "teentje", "ovenschaal", "op", "flinke", "blik", "pan", "vers", "groene", "klein", "theelepels", "je", "a", "s", "ca", "citroen", "gesnipperd", "tenen", "oven", "versgemalen", "kg", "gehakte")
# Standard Dutch stop-words
nlStopwords <- c('aan' , 'af' , 'al' , 'als' , 'bij' , 'dan' , 'dat' , 'die' , 'dit' , 'een' , 'en' , 'er' , 'had' , 'heb' , 'hem' , 'het' , 'hij' , 'hoe' , 'hun' , 'ik' , 'is' , 'je' , 'kan' , 'me' , 'men' , 'met' , 'mij' , 'nog' , 'nu' , 'of' , 'ons' , 'ook' , 'te' , 'tot' , 'uit' , 'van' , 'voor', 'was' , 'we' , 'wel' , 'wij' , 'zal' , 'ze' , 'zei' , 'zij' , 'zo' , 'zou' , 'in' , 'wat')
 
allStopWords <- c(nlStopwords, ingrdCommon)
allStopWords <- allStopWords %>% unique()

# HERE!
ingredientsClean <- rm_stopwords(ingredients, allStopWords, strip = TRUE, ignore.case = TRUE, unlist = TRUE)

ingrdCounts1 <- freq_terms(ingredientsClean, 50, at.least = 3); ingrdCounts1





# Convert text to a corpus using tm, it becomes a list of list, with two tags content and meta
ingredientsSource <- VectorSource(ingredients)
# A volatile corpus is created (i.e. it's in RAM)
ingredientsCorpus <- VCorpus(ingredientsSource)




### Preliminary analysis

plot(freq_terms(unlist(ingredients), 3))


# Tutorial End

### Pre-processing




# These are the most common words and should remove these stop words

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
