### Analysis ###

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
# saveRDS(ingredientsClean, "ingredientsClean.rds")

# Put all in a df for plotting
ingredientsDF <- freq_terms(ingredientsClean, top = 100) 
# write.xlsx(ingredientsDF, file = 'ingNL.xlsx', col.names = F)
ingredientsDF <- read.xlsx('translated.xlsx', sheetName = 'IngrdNL', as.data.frame = T, header = T)
ingredientsDF$freq <- (ingredientsDF$freq*100)/(max(ingredientsDF$freq))
ingredientsDF <- ingredientsDF %>% arrange(.,freq)
# start from 3 to omit zout en peper
ingredientsDF <- ingredientsDF[ingredientsDF$word %ni% c('salt', 'pepper'), ]
ingredientsDF <- tail(ingredientsDF, 50)
ingredientsDF$`listName` <- NULL

# make barplot of Ingredients
par(mar=c(4,8,2,2))

COLS = rep("#eaf2f8",length(ingredientsDF$word))
COLS[ingredientsDF$word=="tomato"] =  "#df6152"
COLS[ingredientsDF$word=="potato"] =  "#f0ce6d"  
COLS[ingredientsDF$word== "olive oil"] = "#708238"
COLS[ingredientsDF$word== "butter"] = "#fdf6cf"
COLS[ingredientsDF$word== "garlic"] = "#f2f1f1"


with(ingredientsDF, barplot(freq, names.arg = word, horiz = T, col = COLS,    main="Tomato vs Potato in NL", xlab = 'Scaled Count',
        las=1, cex.names=0.7, xlim = c(0,80)))

## TODO: Categorize food, spices
## TODO: ingredients: local, south eu, asian

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
# write.xlsx(recipeCategoryDF, file = "recipeCategoryDF.xlsx", row.names = F)
# from google translate
recipeCategoryDF <- read.xlsx('translated.xlsx', sheetName = 'catsNL', as.data.frame = T, header = T)
recipeCategoryDF$FREQ <- (recipeCategoryDF$FREQ*100)/(max(recipeCategoryDF$FREQ))
# make barplot
COLS = rep("#e5e7e9",length(recipeCategoryDF$WORD))
COLS[recipeCategoryDF$WORD=="dutch"] =  "#edbb99"
COLS[recipeCategoryDF$WORD=="italian"] = "#a9dfbf"
COLS[recipeCategoryDF$WORD=="american"] =  "#b22234"
COLS[recipeCategoryDF$WORD=="french"] =  "#0050a4"
COLS[recipeCategoryDF$WORD=="international"] =  "#f67280"

# 
barplot(height=recipeCategoryDF$FREQ, names.arg = recipeCategoryDF$WORD, horiz = TRUE, main = "Origin of recipes in NL", 
         xlab = 'Scaled Count',
        col = COLS,
        las=1, cex.names=0.7, xlim = c(0, 100), mar = c(3,8,3,3))

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
# recipeCategoryDF$WORD <- as.factor(recipeCategoryDF$WORD)
# order for the plot to look nice
mealCategoryDF <- mealCategoryDF[order(mealCategoryDF$FREQ),]
mealCategoryDF$FREQ <- (mealCategoryDF$FREQ*100)/(max(mealCategoryDF$FREQ))
# read in google translated file
mealCategoryDF <- as.data.frame(mealCategoryDF)
# write.xlsx(mealCategoryDF, file = "mealCategoryDF.xlsx", row.names = F)
mealCategoryDF <- read.xlsx('translated.xlsx', sheetName = 'mealsNL', as.data.frame = T, header = T)
# make barplot
COLS = rep("#94d7d5",length(mealCategoryDF$WORD))
COLS[mealCategoryDF$WORD=="dessert"] = "#dbb688"
COLS[mealCategoryDF$WORD=="main dish"] = "#eb7373"
COLS[mealCategoryDF$WORD=="borrelhapjes"] = "#ffba43"

par(mar=c(4,8,2,2))
barplot(height=mealCategoryDF$FREQ, names.arg = mealCategoryDF$WORD, horiz = TRUE, xlab = 'Scaled Count', main = "Meal Categories in NL",
        col=COLS,
        las=1, cex.names=1, xlim = c(0, 100))

### ITALY ###
# TODO: For ppt
# https://ricette.giallozafferano.it/Besciamella.html
# https://ricette.giallozafferano.it/Profiteroles-al-cioccolato.html
# https://ricette.giallozafferano.it/Ravioli-cinesi-al-vapore.html

## Extract meal type, make DF and plot
recipeCategoryIT <- list.select(italianFood, category)
recipeCategoryDFIT <- extractListText(recipeCategoryIT, category, trim=F)

# write.xlsx(as.data.frame(recipeCategoryDFIT), file = "recipeCategoryDFIT.xlsx", row.names = F)
# from google translate
recipeCategoryDFIT <- read.xlsx('translated.xlsx', sheetName = 'mealsIT', as.data.frame = T, header = T)
# make barplot
COLS = rep("#94d7d5",length(recipeCategoryDFIT$listName))
COLS[recipeCategoryDFIT$listName=="desserts"] = "#dbb688"
COLS[recipeCategoryDFIT$listName=="first dishes"] = "#eb7373"
COLS[recipeCategoryDFIT$listName=="pasta"] = "#fce4b1"

par(mar=c(4,9,1,1))
barplot(height = recipeCategoryDFIT$freq, names.arg = recipeCategoryDFIT$listName, horiz = T, col=COLS, xlab = 'Scaled Count', main = "Meal Categories in IT",
        las=1, cex.names=0.7, xlim = c(0,100))

## Extract ingredients, make DF and plot
recipeIngrdsIT <- list.select(italianFood, ingrds)
recipeIngrdsDFIT <- extractListText(recipeIngrdsIT, ingrds, trim = T)

# write.xlsx(as.data.frame(recipeIngrdsDFIT), file = "recipeIngrdsDFIT.xlsx", row.names = F)

recipeIngrdsDFIT <- read.xlsx('translated.xlsx', sheetName = 'IngrdIT', as.data.frame = T, header = T)
# to limit the size of the plot
recipeIngrdsDFIT <- tail(recipeIngrdsDFIT, 50)
# make barplot
par(mar=c(4,8,2,2))
COLS = rep("#eaf2f8",length(recipeIngrdsDFIT$listName))
COLS[grepl("toma", recipeIngrdsDFIT$listName)] = "#df6152"
COLS[recipeIngrdsDFIT$listName =="potatoes"] = "#f0ce6d"  
COLS[recipeIngrdsDFIT$listName == "extra virgin olive oil"] = "#708238"
COLS[recipeIngrdsDFIT$listName == "butter"] = "#fdf6cf"

with(recipeIngrdsDFIT, barplot(freq, names.arg = listName, horiz = T, col = COLS,    main="Tomato vs Potato in IT", xlab = 'Scaled Count',
                            las=1, cex.names=0.7, xlim = c(0,80)))


## Extract about text, make DF and plot
recipeAboutIT <- list.select(italianFood, about)
recipeAboutIT <- unlist(recipeAboutIT)
# one time
# sheets <- data.frame('txt' = recipeAboutIT)
# write.xlsx2(sheets, 'sheets.xlsx', row.names = F)

# get data translated from google sheets
translated <- read.xlsx2('translated.xlsx', header = T, as.data.frame = T, sheetName = 'Sheet1')
translatedAbout <- translated$trans %>% tolower() %>% str_extract_all(., '[a-z]+')

# Get world country/city data
data("world.cities")
world.cities$name <- tolower(world.cities$name) %>% gsub("[^a-zA-Z]", " ", .)
world.cities$country.etc <- tolower(world.cities$country.etc) %>% gsub("[^a-zA-Z]", " ", .)
# just get country and capital city
world <- world.cities %>% filter(capital == 1 & country.etc != 'italy') %>% select(name, country.etc ) %>% arrange(., country.etc)
# get everything Italian
italy <- world.cities %>% filter(country.etc == 'italy') %>% select(name, country.etc ) %>% arrange(., country.etc)
# then append it to the world data
world <- rbind(world, italy)

# Get nationalities and join them to world df
nationalities <- read.xlsx('nationalities.xlsx', header = T, as.data.frame = T, sheetName = 'Sheet1')
world <- merge(world, nationalities, all = T)

# got these from https://www.wikiwand.com/en/List_of_adjectivals_and_demonyms_for_cities
cityDemonym <- read.xlsx('cityDemonym.xlsm',  header = T, as.data.frame = T, sheetName = 'Sheet1')
# merge city demonyms to world df
world <- merge(world, cityDemonym, all.x = T)  
# remove gorgonzola to avoid confusion with cheese
world <- world[world$name != 'gorgonzola', ] 

translatedAbout <- rm_stopwords(translatedAbout, stopwords = Top200Words)

translatedAbout <- lapply(translatedAbout, unique)
## Categorize by nationality 
idx <- sapply(translatedAbout, function(y) y %in% world$nationality)
# idx1 <- sapply(idx, which)
nationalityCatsIT <- Map(`[`, translatedAbout, idx) #1 works
# lapply(seq_along(t1), function(x) t1[[x]][idx1[[x]]]) #2 works

## Categorize by Italian cities and other national capitals 
idx <- sapply(translatedAbout, function(y) y %in% world$name)
nameCatsIT <- Map(`[`, translatedAbout, idx) #1 works

## Categorize by country names
idx <- sapply(translatedAbout, function(y) y %in% world$country.etc)
countryCatsIT <- Map(`[`, translatedAbout, idx) #1 works

## Categorize by city demonyms 
idx <- sapply(translatedAbout, function(y) y %in% world$demonym)
# idx1 <- sapply(idx, which)
demonymCatsIT <- Map(`[`, translatedAbout, idx) #1 works

catsIT <- c(nationalityCatsIT %>% unlist(), nameCatsIT %>% unlist(), countryCatsIT %>% unlist(), demonymCatsIT %>% unlist())
catsITDF <- data.frame('listName' = catsIT, 'freq'=1)
# these words shouldn't be there
catsITDF <- catsITDF[catsITDF$listName %ni% c('parmesan', 'soul', 'penne', 'cayenne'),]
# TODO: Dunno why not working
# Join back the 1. city name
catsITDF <- merge(catsITDF, world[, c('name', 'country.etc')], by.x = 'listName', by.y = 'name', all.x = T)
dim(catsITDF)
# 2. then city demonym
catsITDF <- merge(catsITDF, world[, c('demonym', 'country.etc')], by.x = 'listName', by.y = 'demonym', all.x = T)
dim(catsITDF)
# 3. the country demonym to country from the world df
worldUniqueNats <- world[, c('nationality', 'country.etc')] %>% distinct(., nationality , .keep_all = T)
catsITDF <- merge(catsITDF, worldUniqueNats, by.x = 'listName', by.y = 'nationality', all.x = T)
rm(worldUniqueNats)
# write.xlsx(catsITDF, file = 'catsITDF.xlsx', col.names = T, row.names = F)
catsITDF <- read.xlsx('catsITDF.xlsx', sheetName = 'Sheet1', header = T, as.data.frame = T)
# 
# now coalesce to remove double country column with NAs
catsITDF <- catsITDF %>% mutate(country = coalesce(country.etc.x, country.etc.y, country.etc)) %>% select(listName, freq, country)

# since the country is not joined hence in the country column wherever there was a NA, it is filled by the value from the listName column
catsITDF$country[which(is.na(catsITDF$country))] <- catsITDF$listName[which(is.na(catsITDF$country))]

# Group and remove multiple entries
catsITDF <- catsITDF %>% group_by(country) %>% summarise(freq=sum(freq)) %>% arrange(freq)
catsITDF$freq <- (catsITDF$freq*100)/(max(catsITDF$freq))

COLS = rep("#e5e7e9",length(catsITDF$country))
COLS[catsITDF$country=="usa"] =  "#b22234"
COLS[catsITDF$country=="france"] =  "#0050a4"
COLS[catsITDF$country=="italy"] = "#a9dfbf"
# 
barplot(height = catsITDF$freq, names.arg = catsITDF$country, horiz = T,   
        main = "Origin of recipes in IT", xlab = 'Scaled Count',
        col = COLS,
        las=1, cex.names=0.7, xlim = c(0,100))

# saveRDS(catsITDF, file = 'catsITDF.rds')

# TODO: after project
# write.csv(world$country.etc[which(is.na(world$nationality))] %>% unique(), file = 'nat.csv', row.names = F)
# Maybe not needed
# get alternative names of these countries
# altCountryNames <- read.csv('country-names-cross-ref.csv', header = F)
# altCountryNames <- altCountryNames %>% rename(alt_name = V1, country.etc = V2)
# altCountryNames %>% head()
# altCountryNames <- sapply(altCountryNames, tolower)
# joinedCountrs$name <- NULL
# joinedCountrs <- merge(joinedCountrs, altCountryNames, by = 'country.etc', all.x = T)
