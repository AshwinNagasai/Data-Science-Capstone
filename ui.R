#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
library(shiny)
library(markdown)
library(dplyr)
library(tm)

shinyUI(fluidPage(
  titlePanel("Word Prediction"),
  sidebarLayout(
  sidebarPanel(
    textInput("userInput", "Enter a phrase or sentence:",value =  "",
                                      placeholder = "Enter words here"),
              br(),
              sliderInput("numPredictions", "Number of Predictions",
                                        value = 1, min = 1, max = 3, step = 1)
              ),
      mainPanel(
          h4("Input text"),
          verbatimTextOutput("userSentence"),
          br(),
          h4("Possible Next Word Predicted"),
          verbatimTextOutput("prediction1"),
          verbatimTextOutput("prediction2"),
          verbatimTextOutput("prediction3")
          )
        )
      )
)