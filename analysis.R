### Analysis ###
# TODO: Make nrs scaled (x 100 ? )rather than absolute nrs, here and overall in the script

### NETHERLANDS ###
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
ingrdCommon <-  c("gram", "gr", "of", "g", "en", "el", "in", "de", "eetlepels", "van", "ml", "voor", "een", "dl", "ui", "tl", "rode", "verse", "grote", "eetl", "eetlepel", "theelepel", "gesneden", "witte", "met", "teentjes", "kleine", "geraspte", "plakjes", "te", "naar", "het", "smaak", "zwarte", "blokjes", "gehakt", "om", "t", "cm", "uit", "ei", "gemalen", "zakje", "fijn", "gedroogde", "stukjes", "theel", "blikje", "fijngesneden", "liter", "teentje", "ovenschaal", "op", "flinke", "blik", "pan", "vers", "groene", "klein", "theelepels", "je", "a", "s", "ca", "citroen", "gesnipperd", "tenen", "oven", "versgemalen", "kg", "gehakte", "gerookte", "wok", "zonder", "fraiche", "bruine", "diepvries", "bakje", "droge", "pakje", "eventueel", "takjes", "plakken", "snufje", "fijngehakt", "saus", "pak", "blaadjes", "dunne", "koekenpan", "niet", "mes", "reepjes", "bosje", "fijngehakte", "magere", "grof", "geperst", "manis", "bakpapier", "braadpan", "pot", "kilo", "beetje", "ringen", "snijplank", "stukken", "extra", "belegen", "mixer", "gekookte", "halve", "scheutje", "geraspt", "l", "mespunt", "lente", "half", "keukenmachine", "italiaanse", "potje", "andere", "paar", "schaal", "vulling", "springvorm", "staafmixer", "evt", "teen", "cr", "volle", "zelfrijzend", "zoete", "oude", "gesnipperde", "garnering", "geschild", "zure", "kom", "meer", "e", "dikke", "zachte", "kopje", "gepelde", "bakken", "middelgrote", "molen", "middelgrote", "molen", "liefst", "spaanse", "gele", "gemengde", "ongeveer", "zak", "fijne", "zijn", "plm", "groot", "keuze", "maar", "che", "lepel", "appel", "fra", "gepeld", "stukje", "zelf", "eigen", "heel", "bakplaat", "lekker", "vloeibare", "stengels", "mager", "rauwe", "tablet", "gesmolten", "mespuntje", "poeder", "blikjes", "garde", "enkele", "munt", "druppels", "grove", "stuks", "zakjes", "rood", "zongedroogde", "hapjespan", "pond", "scheut", "geen", "rasp", "schil", "bekertje", "bijv", "glas", "scherp", "gebruik", "wit", "handje", "uitgelekt", "kookpan", "losgeklopt", "zeef", "bodem", "goed", "iets", "koekepan", "pit", "mix", "kop", "per", "handvol", "hele", "nodig", "ma", "sneetjes", "platte", "gebruikt", "gezeefde", "vergiet", "blokje", "dressing", "koude", "pittige", "maken", "x", "ah", "gehalveerd", "goede", "door", "doorsnede", "stuk", "takje", "pure", "oelek", "rijpe", "deksel", "mag", "eidooier", "lange", "mengkom", "zoals", "zie", "heerlijk", "heerlijke", "makkelijk")

# Standard Dutch stop-words
nlStopwords <- c('aan' , 'af' , 'al' , 'als' , 'bij' , 'dan' , 'dat' , 'die' , 'dit' , 'een' , 'en' , 'er' , 'had' , 'heb' , 'hem' , 'het' , 'hij' , 'hoe' , 'hun' , 'ik' , 'is' , 'je' , 'kan' , 'me' , 'men' , 'met' , 'mij' , 'nog' , 'nu' , 'of' , 'ons' , 'ook' , 'te' , 'tot' , 'uit' , 'van' , 'voor', 'was' , 'we' , 'wel' , 'wij' , 'zal' , 'ze' , 'zei' , 'zij' , 'zo' , 'zou' , 'in' , 'wat')
 
allStopWords <- c(nlStopwords, ingrdCommon)
allStopWords <- allStopWords %>% unique()

# removing stopwords using self-created function
ingredientsClean <- lapply(ingredients, removeStopWords, stopwords=allStopWords)

# Total unique ingredients are 8852  
unlist(ingredientsClean) %>% unique() %>% length()
ingredientsClean <- as.vector(unlist(ingredientsClean))

# Stemming and subsetting tomatoes and potatoes
ingredientsClean[grepl("toma", ingredientsClean)] <- "tomaat"
ingredientsClean[grepl("aardap|kriel|patat|pieper", ingredientsClean)] <- "aardappel"

# Put all in a df for plotting
ingredientsDF <- freq_terms(ingredientsClean, top = 100) 
# make barplot
# TODO: Chnage y label and add plot label
plot(ingredientsDF[3 :100, ]) # start from 3 to omit zout en peper

## TODO: Categorize food, spices
## TODO: ingredients: local, south eu, asian

# saveRDS(ingredientsClean, "ingredientsClean.rds")

### Analysing recipe names
# Get all recipe names
recipeURLs <- names(ingredients)

# Extract recipe name from the URL, then keep only alphabetic chars which is a list
recipeNames <- gsub('https://www.smulweb.nl/recepten/[0-9]+', "", recipeURLs) %>% gsub("[^A-Za-z]+", " ", .) 

# break the string vectors into their constituent strings by " " 
# and then remove stop words 
# and then join the constituent strings back to the string vector (recipe name)
recipeNames <- recipeNames %>% tolower() %>% 
  strsplit(., " ") %>% 
  lapply(., removeStopWords, stopwords=allStopWords) %>% 
  sapply(., paste, collapse = " ") %>% 
  sub(" ", "", .)

# saveRDS(recipeNames, "recipeNames.rds")

## Recipe categories
mealType <- c("borrelhapje", "lunch", "brunch", "hoofdgerecht", "voorgerecht", "feestmaaltijd", "nagerecht", "bijgerecht", "banket", "ontbijt", "tussengerecht", "tussendoortje", "scandinavisch", "buffet", "amuse")

recipeCategoryDF <- freq_terms(as.vector(unlist(recipeCategory)), top = 100) 
# remove meal types from the recipe categories
recipeCategoryDF$WORD[which(recipeCategoryDF$WORD %in% mealType)] <- ""
# group words that are actually same
recipeCategoryDF$WORD[which(recipeCategoryDF$WORD %in% c("multi", "cultureel"))] <- "multi-cultureel"
recipeCategoryDF$WORD[which(recipeCategoryDF$WORD %in% c("goedkoop", "en", "snel"))] <- "goedkoop-en-snel"
recipeCategoryDF$WORD[which(recipeCategoryDF$WORD %in% c("saus", "dressing"))] <- "saus-dressing"
# Group to remove duplicates and sum them
recipeCategoryDF <- recipeCategoryDF %>% group_by(WORD) %>% summarise(FREQ=sum(FREQ))
recipeCategoryDF <- as.data.frame(recipeCategoryDF) 
# Delete the row which has empty word value
recipeCategoryDF <- recipeCategoryDF[!(recipeCategoryDF$WORD == ""), ] 
# Need to make a factor for the plot to work, it doesn't like char 
recipeCategoryDF$WORD <- as.factor(recipeCategoryDF$WORD)
# order for the plot to look nice
recipeCategoryDF <- recipeCategoryDF[order(recipeCategoryDF$FREQ),]
# make barplot
# TODO: Add y label and add plot label
barplot(height=recipeCategoryDF$FREQ, names.arg = recipeCategoryDF$WORD, horiz = TRUE, xlab = 'COUNT',
        las=1, cex.names=0.7, xlim = c(0, 1100), mar = c(3,8,3,3))


# Now to see the mealtypes
mealCategoryDF <- freq_terms(as.vector(unlist(recipeCategory)), top = 100) 
# keep only meal types from the recipe categories
mealCategoryDF$WORD[which(mealCategoryDF$WORD %ni% mealType)] <- ""
# combine lunch en brunch
mealCategoryDF$WORD[which(mealCategoryDF$WORD %in% c("lunch", "brunch"))] <- "lunch-brunch"

# Group to remove duplicates and sum them
mealCategoryDF <- mealCategoryDF %>% group_by(WORD) %>% summarise(FREQ=sum(FREQ))
# Delete the row which has empty word value
mealCategoryDF <- mealCategoryDF[!(mealCategoryDF$WORD == ""), ] 
# Need to make a factor for the plot to work, it doesn't like char 
recipeCategoryDF$WORD <- as.factor(recipeCategoryDF$WORD)
# order for the plot to look nice
mealCategoryDF <- mealCategoryDF[order(mealCategoryDF$FREQ),]
# make barplot
# TODO: Add y label and add plot label
barplot(height=mealCategoryDF$FREQ, names.arg = mealCategoryDF$WORD, horiz = TRUE, xlab = 'COUNT',
        las=1, cex.names=0.7, xlim = c(0, 2000), mar = c(3,8,3,3))

### ITALY ###
# TODO: For ppt
# https://ricette.giallozafferano.it/Besciamella.html
# https://ricette.giallozafferano.it/Profiteroles-al-cioccolato.html
# https://ricette.giallozafferano.it/Ravioli-cinesi-al-vapore.html

## Extract category, make DF and plot
recipeCategoryIT <- list.select(italianFood, category)
recipeCategoryDFIT <- extractListText(recipeCategoryIT, category)

par(mar=c(4,9,1,1))
barplot(height = recipeCategoryDFIT$freq, names.arg = recipeCategoryDFIT$listName, horiz = T, 
        las=1, cex.names=0.7, xlim = c(0,100))

## Extract ingredients, make DF and plot
recipeIngrdsIT <- list.select(italianFood, ingrds)
recipeIngrdsDFIT <- extractListText(recipeIngrdsIT, ingrds, trim = T)

barplot(height = recipeIngrdsDFIT$freq, names.arg = recipeIngrdsDFIT$listName, horiz = T, 
        las=1, cex.names=0.7, xlim = c(0,100))

## Extract text, make DF and plot
recipeAboutIT <- list.select(italianFood, about)
recipeAboutIT <- unlist(recipeAboutIT)
sheets <- data.frame('txt' = recipeAboutIT)
write.xlsx2((sheets, 'sheets.excel', row.names = F)
write_excel_csv()

library("xlsx")
