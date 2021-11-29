library(bakeoff)
library(tidyverse)
library(tidytext)

#' getFlavors
#'
#' @param challengeName type of bake-off challenge, i.e. signature or showstopper
#' @param nonFlavors character vector defining words that don't represent a specific flavor (e.g. "cake")
#' @param episodes tibble with bake-off episode data containing `signature` and `showstopper` columns with bake names
#'
#' @return A tibble with counts of all words contained in bake names
#' @export
#'
#' @examples
getFlavors <- function(challengeName, nonFlavors, episodes = bakeoff::episodes){
  episodes %>% 
    unnest_tokens(word, challengeName) %>%
    anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE) %>%
    anti_join(nonFlavors, by = "word")
}

# Define set of words that are types of baked goods rather than flavors
nonFlavors <- tibble(word = c("cake", "pie", "biscuits", "buns", "loaf", "pies", "bread", "tarte", "tatin", "tart", "meringue", "white", "black", "cream"))

# Calculate total number of bakes
totalBakes <- sum(!is.na(episodes$showstopper), !is.na(episodes$signature))
# Get top 13 flavorings used in showstopper and signature bakes
topFlavors <- getFlavors("showstopper", nonFlavors) %>%
  left_join(getFlavors("signature", nonFlavors), by = "word") %>%
  mutate(n = n.x + n.y) %>%
  select(word, n) %>%
  replace_na(list(n = 1)) %>%
  arrange(n) %>% 
  mutate(percent = n/totalBakes) %>%
  tail(n = 13)

calcRatios <- function(word, episodes){
  e <- episodes %>%
    filter(!is.na(showstopper), 
           !is.na(signature)) %>%
    mutate(episode = as.numeric(episode),
           series = as.numeric(series)) %>%
    left_join(episode_results, by =  c("episode", "series")) %>%
    mutate(eliminated = str_detect(baker, eliminated),
           wordPresent = ((str_detect(tolower(showstopper), word))|(str_detect(tolower(signature), word))),
           starBaker = str_detect(baker, sb_name)) %>%
    replace_na(list(eliminated = FALSE, starBaker = FALSE)) %>%
    mutate(eliminatedAndUsed = (eliminated & wordPresent),
           eliminatedAndDidNotUse= (eliminated & !wordPresent),
           starBakerAndUsed = (starBaker & wordPresent),
           starBakerAndDidNotUse = (starBaker & !wordPresent))
  return(tibble(
    flavor = word, 
    riskRatio = (sum(e$eliminatedAndUsed)/sum(e$wordPresent))/(sum(e$eliminatedAndDidNotUse)/(nrow(e) - sum(e$wordPresent))),
    rewardRatio = (sum(e$starBakerAndUsed)/sum(e$wordPresent))/(sum(e$starBakerAndDidNotUse)/(nrow(e) - sum(e$wordPresent))),
    n = sum(e$wordPresent),
    nElim = sum(e$eliminatedAndUsed),
    nSB = sum(e$starBakerAndUsed)
  ))
}

ratios <- map_dfr(topFlavors$word,
                  calcRatios,
                  episodes = bakeoff::episodes) %>%
  mutate(flavor = factor(flavor, levels = topFlavors$word)) 

# bootstrapping
rows <- map(1:20, 
            function(i, size,x) sample(x, size, TRUE),
            x = 1:nrow(episodes), 
            size = nrow(episodes))


subsetAndCalcRatios <- function(sampleIndex, rowIndices, words){
  resampledData <- bakeoff::episodes[rowIndices,]
  ratios <-  map_dfr(words,
                     calcRatios,
                     episodes = resampledData) %>%
    mutate(flavor = factor(flavor, levels = topFlavors$word), 
           sampleIndex = sampleIndex) 
}

bootstraps <- map2_dfr(
  1:length(rows),
  rows,
  subsetAndCalcRatios,
  words = topFlavors$word
)

moreRows <- map(1:100, 
            function(i, size,x) sample(x, size, TRUE),
            x = 1:nrow(episodes), 
            size = nrow(episodes))

moreBootstraps <- bootstraps <- map2_dfr(
  1:length(moreRows),
  moreRows,
  subsetAndCalcRatios,
  words = topFlavors$word
)



