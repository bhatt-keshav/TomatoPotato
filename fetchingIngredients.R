# Script only for fetching ingredients, this must be let to run for long somewhere, hence separate

allIngredients <- list()
loop <- 0
byNr <- 1000
for (i in seq(5000, 20000, by = byNr)) {
  loop <- loop + 1
  print(c('loop at', loop))
  ingredientsNow <- list()
  # if (i == 1) {
  #             pageLoop <- pages[i:1000]
  #             for (p in pageLoop) {
  #                 allIngredients[[loop]] <- sapply(p, getIngredientsAndErrors)
  #                 }  }
  # else { 
  pageLoop <- pages[i:((i-1) + byNr)]
  for (p in pageLoop) {
    ingredientsNow[p] <- sapply(p, getIngredientsAndErrors)
  } 
  allIngredients[[loop]] <- ingredientsNow
  Sys.sleep(60)
  # }
}

saveRDS(allIngredients, 'allIngredients.rds')

length(allIngredients[[1]])