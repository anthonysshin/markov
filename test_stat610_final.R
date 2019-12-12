library(testthat)
source("S610_Final_AnthonyShin_v4.R")

context("check the markov function")

# generate data for a very simple markov function
text <- as.character("The Project Gutenberg eBook, The Chimes, by Charles Dickens  This eBook is for the use of anyone anywhere at no cost and with almost no restrictions whatsoever.  You may copy it, give it away or re-use it under the terms of the Project Gutenberg License included with this eBook or online at www.gutenberg.org There are not many people—and as it is desirable that a story-teller and a story-reader should establish a mutual understanding as soon as possible, I beg it to be noticed that I confine this observation neither to young people nor to little people, but extend it to all conditions of people: little and big, young and old: yet growing up, or already growing down again—there are not, I say, many people who would care to sleep in a church.  I don’t mean at sermon-time in warm weather (when the thing has actually been done, once or twice), but in the night, and alone.  A great multitude of persons will be violently astonished, I know, by this position, in the broad bold Day.  But it applies to Night.  It must be argued by night, and I will undertake to maintain it successfully on any gusty winter’s night appointed for the purpose, with any one opponent chosen from the rest, who will meet me singly in an old churchyard, before an old church-door; and will previously empower me to lock him in, if needful to his satisfaction, until morning. Tripe it was; and Meg, in high joy, protested he should say, in half a minute more, it was the best tripe ever stewed. 
‘And so,’ said Meg, busying herself exultingly with the basket, ‘I’ll lay the cloth at once, father; for I have brought the tripe in a basin, and tied the basin up in a pocket-handkerchief; and if I like to be proud for once, and spread that for a cloth, and call it a cloth, there’s no law to prevent me; is there, father?’ 
‘Not that I know of, my dear,’ said Toby.  ‘But they’re always a-bringing up some new law or other.’ ‘And according to what I was reading you in the paper the other day, father; what the Judge said, you know; we poor people are supposed to know them all.  Ha ha!  What a mistake!  My goodness me, how clever they think us!’ 
‘Yes, my dear,’ cried Trotty; ‘and they’d be very fond of any one of us that did know ’em all.  He’d grow fat upon the work he’d get, that man, and be popular with the gentlefolks in his neighbourhood.  Very much so!’ 
‘He’d eat his dinner with an appetite, whoever he was, if it smelt like this,’ said Meg, cheerfully.  ‘Make haste, for there’s a hot potato besides, and half a pint of fresh-drawn beer in a bottle.  Where will you dine, father?  On the Post, or on the Steps?  Dear, dear, how grand we are.  Two places to choose from!’ 
‘The steps to-day, my Pet,’ said Trotty.  ‘Steps in dry weather.  Post in wet.  There’s a greater conveniency in the steps at all times, because of the sitting down; but they’re rheumatic in the damp.")
test <- gsub("\\d", "", text)
text <- gsub("<.+?>", "", text)
text <- gsub("[:punct:]]", "", text)
text <- tibble(text)

## the order of words (n-grams); the number must be larger than 1
## the order of words (n-grams); the number must be larger than 1
ngram <- 3     

## the length of output text
length_output <- 30   



# Tests
test_that("Check whether the number of the column of a data frame is same with the number in ngram", {
  expect_equal( ncol(text_separation(text, ngram)), ngram )
}) 



test_that("Check whether a selected word set has the same number of columns as defined in ngram", {
  # Separate text based on ngram 
  sep_text <- text_separation(text, ngram) 
  
  expect_equal( ncol(initial_words(sep_text)), ngram  )
})



test_that("Check whether the last ngram-1 word(s) of a current word set is/are same with the first ngram-1 word(s) of a next word set", {
  # Separate text based on ngram 
  sep_text <- text_separation(text, ngram)  
  
  # Randomly select initial word and then it's words set
  sampled_a_row <- initial_words(sep_text) 
  text_index <- NULL
  
  # create the output text as many as the number of length_output
  ## For example, if ngram is set to 3, select all relevant word sets from text that include the first and second word 
  ## that are equal with the second and third word in the current words.
  for (i in 1:nrow(sep_text)){  
    text_index[i] <- all(paste(unlist(sep_text[i,1:(ngram-1)])) == paste(unlist(sampled_a_row[2:ngram])))
  }
  selected_rows <- sep_text %>% filter(text_index)
  
  # Randomly select a row from the selected rows above
  next_sampled_a_row <- selected_rows[sample(1:nrow(selected_rows), 1),]
  
  expect_equivalent( next_sampled_a_row[1:(ngram-1)], sampled_a_row[2:ngram] )  
})




test_that("Check all cells in a data frame have characters", {
  # Separate text based on ngram 
  sep_text <- text_separation(text, ngram) 
  
  expect_true(all(nchar(sep_text) >= 0))
})

