# Initialise Environment --------------------------------------------------

install.packages("tidyverse")

install.packages("lexicon_nrc_vad")

library(tidyverse)

rm(list = ls())

# Initialise Functions and Data ---------------------------------------------


# Load dataset
# 
# Create V A and D functions
# 
# 
# Get 5 attributes
# List of V,A,D each
# Multiply by 1, 0.9. 0.
# 


# Load Data ---------------------------------------------------------------

df <- read_csv("data/dataset.csv")
vad <- read_tsv("data/NRC-VAD-Lexicon.tsv")

potato <- vad %>% with(which(word == 'potato'))
vad[potato,]["valence"]

find_index <- function(word_) {
  return(vad %>% with(which(word == word_)))
}

valence <- function(index) {
  index <- vad %>% with(which(word == word_))

  return(vad[index,]["valence"])
}

arousal <- function(index) {
  index <- vad %>% with(which(word == word_))

  return(vad[index,]["arousal"])
}

dominance <- function(index) {
  index <- vad %>% with(which(word == word_))

  return(vad[index,]["dominance"])
}

# print(4 + arousal('fish'))

append_vad <- function(row) {

  print(row)
  # v <- 0
  # a <- 0
  # d <- 0

  # i <- 0
  # col <- "attribute_" + 0
  # row[paste("attribute_", 0)] = 33
  # row["valence"] = 4.3
  
  # for (i in 1:5) {
  #   index = find_index(row[f'attribute_{i}'])
  #   v += 
  #   
  #   
  # }
  # line = "\n"
  # write_lines(line, "data/dataset.csv")
  # print(row["time"])
  # return(row["time"])
  return(row)
}

appended_df <- apply(df, 1, append_vad)