### Analysis ###

### Load data
## Loading the URLs of recipes fetches per page of the website (optional)
# pages <- readRDS('recipeURLs.rds')

## Loading the fetched ingredients from these pages
# This is a list of lists and can be considered as a PCorpus (?)
ingredients <- readRDS('ingredients.rds')

### Text Pre-processing
## Cleaning
# Make all entries lower case, other cleaning such as removing punctuation, number or whitespace is not needed as I did that while fetching by keeping only chars
ingredients <- lapply(ingredients, tolower)
# total ingredients are: 167110
unlist(ingredients) %>% length()
# while unique ingredients are 9135
unique(unlist(ingredients)) %>% length()

## Remove stopwords
# There are many frequently occuring words which should be removed
freq_terms(unlist(ingredients), top = 100)
# Of these 100, by manual inspection, these are also kind of stop-words
ingrdCommon <-  c("gram", "gr", "of", "g", "en", "el", "in", "de", "eetlepels", "van", "ml", "voor", "een", "dl", "ui", "tl", "rode", "verse", "grote", "eetl", "eetlepel", "theelepel", "gesneden", "witte", "met", "teentjes", "kleine", "geraspte", "plakjes", "te", "naar", "het", "smaak", "zwarte", "blokjes", "gehakt", "om", "t", "cm", "uit", "ei", "gemalen", "zakje", "fijn", "gedroogde", "stukjes", "theel", "blikje", "fijngesneden", "liter", "teentje", "ovenschaal", "op", "flinke", "blik", "pan", "vers", "groene", "klein", "theelepels", "je", "a", "s", "ca", "citroen", "gesnipperd", "tenen", "oven", "versgemalen", "kg", "gehakte", "gerookte", "wok", "zonder", "fraiche", "bruine", "diepvries", "bakje", "droge", "pakje", "eventueel", "takjes", "plakken", "snufje", "fijngehakt", "saus", "pak", "blaadjes", "dunne", "koekenpan", "niet", "mes", "reepjes", "bosje", "fijngehakte", "magere", "grof", "geperst", "manis", "bakpapier", "braadpan", "pot", "kilo", "beetje", "ringen", "snijplank", "stukken", "extra", "belegen", "mixer", "gekookte", "halve", "scheutje", "geraspt", "l", "mespunt", "lente", "half", "keukenmachine", "italiaanse", "potje", "andere", "paar", "schaal", "vulling", "springvorm", "staafmixer", "evt", "teen", "cr", "volle", "zelfrijzend", "zoete", "oude", "gesnipperde", "garnering", "geschild", "zure", "kom", "meer", "e", "dikke", "zachte", "kopje", "gepelde", "bakken", "middelgrote", "molen", "middelgrote", "molen", "liefst", "spaanse", "gele", "gemengde", "ongeveer", "zak", "fijne", "zijn", "plm", "groot", "keuze", "maar", "che", "lepel", "appel", "fra", "gepeld", "stukje", "zelf", "eigen", "heel", "bakplaat", "lekker", "vloeibare", "stengels", "mager", "rauwe", "tablet", "gesmolten", "mespuntje", "poeder", "blikjes", "garde", "enkele", "munt", "druppels", "grove", "stuks", "zakjes", "rood", "zongedroogde", "hapjespan", "pond", "scheut", "geen", "rasp", "schil", "bekertje", "bijv", "glas", "scherp", "gebruik", "wit", "handje", "uitgelekt", "kookpan", "losgeklopt", "zeef", "bodem", "goed", "iets", "koekepan", "pit", "mix", "kop", "per", "handvol", "hele", "nodig", "ma", "sneetjes", "platte", "gebruikt", "gezeefde", "vergiet", "blokje", "dressing", "koude", "pittige", "maken", "x", "ah", "gehalveerd", "goede", "door", "doorsnede", "stuk", "takje", "pure", "oelek", "rijpe")

# Standard Dutch stop-words
nlStopwords <- c('aan' , 'af' , 'al' , 'als' , 'bij' , 'dan' , 'dat' , 'die' , 'dit' , 'een' , 'en' , 'er' , 'had' , 'heb' , 'hem' , 'het' , 'hij' , 'hoe' , 'hun' , 'ik' , 'is' , 'je' , 'kan' , 'me' , 'men' , 'met' , 'mij' , 'nog' , 'nu' , 'of' , 'ons' , 'ook' , 'te' , 'tot' , 'uit' , 'van' , 'voor', 'was' , 'we' , 'wel' , 'wij' , 'zal' , 'ze' , 'zei' , 'zij' , 'zo' , 'zou' , 'in' , 'wat')
 
allStopWords <- c(nlStopwords, ingrdCommon)
allStopWords <- allStopWords %>% unique()

# removing stopwords using self-created function
ingredientsClean <- lapply(ingredients, removeStopWords, stopwords=allStopWords)

# freq_terms(unlist(ingredientsClean), top = 100)
freq_terms(unlist(ingredientsClean), top = 200)
freq_terms(unlist(ingredientsClean), top = 150)$WORD[100:150] %>% cat()
freq_terms(unlist(ingredientsClean), top = 100) %>% plot()

# Test

x=list(ingredients[[1]], ingredients[[2]], ingredients[[3]])
y=rapply(x, removeStopWords, how="list")

removeStopWords(x[[1]], allStopWords)

z[[1]] == y[[1]]

# 




 <- rm_stopwords(ingredients, allStopWords, strip = TRUE, ignore.case = TRUE, unlist = TRUE)

ingrdCounts1 <- freq_terms(ingredientsClean, 50, at.least = 3); ingrdCounts1



freq_terms(unlist(), top = 100)


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
