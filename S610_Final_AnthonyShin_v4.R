#  S610 Final Project
#  Anthony Shin 
#  Dec 12, 2019
#  Git repository address: https://github.com/anthonysshin/markov

# Removes all global variables
rm(list = ls())  # DELETE

library(readtext)
library(stringr)
library(random)
library(plyr)
library(dplyr)
library(tidyr)
library(tidytext)


# Load and clean data
## The text source is from HW1: The Project Gutenberg eBook, The Chimes, by Charles Dickens.
text <- readLines("https://github.com/anthonysshin/markov/blob/master/653-0.txt")


## Removed unnessary characters, such as number, puctuation, and html tags, if any.
test <- gsub("\\d", "", text)
text <- gsub("<.+?>", "", text)
text <- gsub("[:punct:]]", "", text)
text <- tibble(text)

## The order of words (n-grams); the number must be larger than 1
ngram <- 3 

## The length of output text
length_output <- 20   


# Generate sentences (Markov Function)
gen_sentence <- function(ngram, text, length_output){
  index <- NULL
  text_index <- NULL
  sentence_temp <- NULL  
  sentence <- NULL
  last_word <- NULL

  ## Separate text based on ngram 
  sep_text <- text_separation(text, ngram)  
  
  ## Randomly select initial word and then it's words set
  sampled_a_row <- initial_words(sep_text) 
  
  ## Unlist the sampled row and store in a temporary sentence variable 
  sentence_temp <- paste(unlist(sampled_a_row), collapse=' ') 
  
  ## Store a first sentence in the sentence variable
  sentence <- c(sentence, sentence_temp)               

  ## Create next words as many as the number of length_output
  for (i in 1:(length_output-ngram)){  
    ## Find the next word sets depends on the current word along with ngram
    for (i in 1:nrow(sep_text)){          
      text_index[i] <- all(paste(unlist(sep_text[i,1:(ngram-1)])) == paste(unlist(sampled_a_row[2:ngram])))
    }
    ## The next word sets
    selected_rows <- sep_text %>% filter(text_index)
    
    ## If there is no next word matched, randomly select a curren word again from text
    if (all(text_index == FALSE)) {   
      sampled_a_row <- initial_words(sep_text)  
    }
    
    # Randomly select a word set from the next word rows
    sampled_a_row <- selected_rows[sample(1:nrow(selected_rows), 1),] 
    
    # Select the last word from the next word set
    last_word <- as.character(sampled_a_row[,ngram])  
    
    # Attach the last word to sentence
    sentence <- c(sentence, last_word)                 
    sentence <- paste(unlist(sentence), collapse=' ')
    print(sentence)
  }
  return(sentence)
}


# Separate text based on ngram 
text_separation <- function(text, ngram){
    ## Generate a data frame for the state of words by ngram
    text <- text %>%
      unnest_tokens(text, text, token = "ngrams", n = ngram) %>%    
      as.matrix() %>% 
      strsplit(' ', fixed = TRUE)   ## separate columes by the number of ngram
    
    sep_text <- data.frame(matrix(unlist(text), nrow=length(text), byrow=T), stringsAsFactors = FALSE)
    return(sep_text)
}


# Randomly select a current word and then it's words set
initial_words <- function(sep_text){
  
  ## Randomly select a current word and then store the index of it
  index <- sample(1:nrow(sep_text), 1)    
 
  ## The current word
  first_word <- as.character(sep_text[index,1]) 
 
  ## Select all word sets that include the current word in the first column
  selected_rows <- sep_text %>% filter(sep_text$X1 == first_word) 
  
  ## Randomly select a word set from the selected word sets
  sampled_a_row <- selected_rows[sample(1:nrow(selected_rows), 1),] 
  return(sampled_a_row)
}


output <- gen_sentence(ngram, text, length_output)





