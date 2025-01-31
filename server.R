#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.  

Init_Prediction <- readRDS("./Firstthree.RDS")
freqbigram <- readRDS("./Bigram.RDS")
freqtrigram <- readRDS("./Trigram.RDS")
freqquadrigram <- readRDS("./Quadrigram.RDS")

# load bad words file
profanitylist <- readLines("http://www.bannedwordlist.com/lists/swearWords.txt", encoding = "UTF-8", skipNul = TRUE)
profanitylist <- iconv(profanitylist, "latin1", "ASCII", sub = "")

predictionM <- function(userInput, ngrams) {
  
  # For quad gram and higher
  if (ngrams > 3) {
    userInput3 <- paste(userInput[length(userInput) - 2],
                        userInput[length(userInput) - 1],
                        userInput[length(userInput)])
    Data_tokens <- freqquadrigram %>% filter(outcome == userInput3)
    if (nrow(Data_tokens) >= 1) {
      return(Data_tokens$outcome[1:3])
    }
# Back-off to tri gram
    
    return(predictionM(userInput, ngrams - 1))
  }
  
  # For tri gram
  
  if (ngrams == 3) {
    userInput1 <- paste(userInput[length(userInput) - 1], userInput[length(userInput)])
    Data_tokens <- freqtrigram %>% filter(outcome == userInput1)
    
    if (nrow(Data_tokens) >= 1) {
      return(Data_tokens$outcome[1:3])
    }
    # Back-off to bi gram
    return(predictionM(userInput, ngrams - 1))
  }
  
  # For bi gram and lower
  
  if (ngrams < 3) {
    userInput1 <- userInput[length(userInput)]
    Data_tokens <- freqbigram %>% filter(outcome == userInput1)
    
    return(Data_tokens$outcome[1:3])
    
    # Back-off completely, Unigrams not implemented in algorithm
  }
  
  return(NA)
}

Cleaninput <- function(input) {
  
  # Function to clean the sentence (data) given as input by the user
  
  if (input == "" | is.na(input)) {
    return("")
  }
  
  # Converting to lower case
  input <- tolower(input)
  
  # remove URL, email addresses, Twitter handles and hash tags
  input <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", input, ignore.case = FALSE, perl = TRUE)
  input <- gsub("\\S+[@]\\S+", "", input, ignore.case = FALSE, perl = TRUE)
  input <- gsub("@[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
  input <- gsub("#[^\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
  
  # remove Ordinal numbers
  input <- gsub("[0-9](?:st|nd|rd|th)", "", input, ignore.case = FALSE, perl = TRUE)
  
  # Filtering profanity to remove
  input <- removeWords(input, profanitylist)
  
  # remove punctuation
  
  input <- gsub("[^\\p{L}'\\s]+", "", input, ignore.case = FALSE, perl = TRUE)
  input <- gsub("[.\\-!]", " ", input, ignore.case = FALSE, perl = TRUE)
  
  # trim white spaces
  
  input <- gsub("^\\s+|\\s+$", "", input)
  input <- stripWhitespace(input)
  
  if (input == "" | is.na(input)) {
    return("")
  }
  
  input <- unlist(strsplit(input, " "))
  
  return(input)
  
}

predictNextWord <- function(input, word = 0) {
  
  input <- Cleaninput(input)
  
  if (input[1] == "") {
    output <- Init_Prediction
  } else if (length(input) == 1) {
    output <- predictionM(input, ngrams = 2)
  } else if (length(input) == 2) {
    output <- predictionM(input, ngrams = 3)
  } else if (length(input) > 2) {
    output <- predictionM(input, ngrams = 4)
  }
  
  if (word == 0) {
    return(output)
  } else if (word == 1) {
    return(output[1])
  } else if (word == 2) {
    return(output[2])
  } else if (word == 3) {
    return(output[3])
  }
  
}

shinyServer(function(input, output) {
  
  # User sentence
  output$userSentence <- renderText({input$userInput});
  
  observe({
    numPredictions <- input$numPredictions
    if (numPredictions == 1) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- NULL
      output$prediction3 <- NULL
    } else if (numPredictions == 2) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
      output$prediction3 <- NULL
    } else if (numPredictions == 3) {
      output$prediction1 <- reactive({predictNextWord(input$userInput, 1)})
      output$prediction2 <- reactive({predictNextWord(input$userInput, 2)})
      output$prediction3 <- reactive({predictNextWord(input$userInput, 3)})
    }
  })
})