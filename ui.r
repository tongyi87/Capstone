library(shiny)
library(shinyBS)

shinyUI(fluidPage(theme = "bootstrap.css",
  
  title = "Word Predictor - I'll finish what you started",
  br(),
  br(),
  br(),
  br(),
  br(),
  fluidRow(
    column
    (8, offset =2,
      textInput("sentence", value = "", label = 
                  div
                  (style="vertical-align:middle;",
                    div(style="display:inline-block;vertical-align:bottom;",h1("Type something below")),
                    div(style="display:inline-block; margin-left:8px; margin-bottom:12px;",HTML("<a href='https://sg.linkedin.com/in/tongyikoh'>by Tongyi Koh</a>"))
                  ), width = "100%"),
      checkboxInput("removeProfanity", label = "Remove profanity from suggestions", value = TRUE),
      h4("Your next word could be..."),
      div
      (style="vertical-align:middle; margin-bottom:8px;",
        div(style="display:inline-block;vertical-align:middle;","Accuracy: "),
        div(class="strongCircle"),
        div(style="display:inline-block;vertical-align:middle;","Trigram (High)  "),
        div(class="mediumCircle"),
        div(style="display:inline-block;vertical-align:middle;","Bigram (Medium)  "),
        div(class="weakCircle"),
        div(style="display:inline-block;vertical-align:middle;","Unigram (Low)")
      ),
      uiOutput("suggestions")
    )
  )
))