# Initialise Environment --------------------------------------------------

install.packages("tidyverse")

library(tidyverse)

rm(list = ls())


# Load Data ---------------------------------------------------------------

df <- read_csv("data/clothing_dataset.csv")
vad <- read_tsv("data/NRC-VAD-Lexicon.tsv")


# Initialise Functions ----------------------------------------------------

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

unknown_words <- c()
unknown_words_count <- 0


# Process Data ---------------------------------------------------------------

append_vad <- function(row) {
  v <- vector()
  a <- vector()
  d <- vector()
  
  for (i in 0:4) {
    col <- paste("att_manual_", i, sep="")
    word_ <- row[col][[1]]
    
    # NA value in dataset column
    if (is.na(word_) || is.na(nchar(word_)) || typeof(word_) != "character" || nchar(word_) == 0) {
      next
    }

    index <- find_index(word_)
    
    if (!exists("index") || identical(index, integer(0))) {
      print(paste("'", word_, "' not in lexicon", sep=""))
      unknown_words <- c(unknown_words, word_)
      unknown_words_count <- unknown_words_count + 1
      next
    }

    v <- c(v, vad[index,]["valence"][[1]])
    a <- c(a, vad[index,]["arousal"][[1]])
    d <- c(d, vad[index,]["dominance"][[1]])
  }

  valence <- num(mean(v), digits=3)
  arousal <- num(mean(a), digits=3)
  dominance <- num(mean(d), digits=3)
  
  line <- paste(
    row["date"],
    row["time"],
    row["file"],
    row["member"],
    row["att_manual_0"],
    row["att_manual_1"],
    row["att_manual_2"],
    row["att_manual_3"],
    row["att_manual_4"],
    row["att_model_0"],
    row["att_model_1"],
    row["att_model_2"],
    row["att_model_3"],
    row["att_model_4"],
    sprintf("%0.3f", valence),
    sprintf("%0.3f", arousal),
    sprintf("%0.3f", dominance),
    # as.character(valence),
    # as.character(arousal),
    # as.character(m(valence),
    # as.character(num(arousal, digits=3)),
    # as.character(num(dominance, digits=3)),
    # num(as.character(valence), digits=3),
    # num(as.character(arousal), digits=3),
    # num(as.character(dominance), digits=3),
    # as.numeric(valence),
    # as.numeric(arousal),
    # as.numeric(dominance),
    sep=","
  )
  
  write(line, "data/dataset.csv", append=TRUE)

  return(row)
}

df <- na.omit(df)

appended_df <- apply(df, 1, append_vad)

print(paste("There were", length(unique(unknown_words)), "different words in the dataset not known to the lexicon, used a total of", unknown_words_count, "times. The words are:", unique(unknown_words)))
