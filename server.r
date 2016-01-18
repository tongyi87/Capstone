library(shiny)
library(tm)
library(tau)
library(shinyBS)

cleanedBigrams<-readRDS("cleanedBigrams.rds")
cleanedTrigrams<-readRDS("cleanedTrigrams.rds")
cleanedUnigrams<-readRDS("cleanedUnigrams.rds")
profanities<-readRDS("profanities.rds")

getSearchTerms<-function(sentence){
  
  #Cast text to lower case
  cleanText <- tolower(sentence)
  
  #Remove hashtags
  cleanText <- gsub(" #\\S*","",cleanText);
  
  #Remove URLs (http, https, ftp)
  cleanText <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", cleanText)
  
  #Remove blank text 
  cleanText <- cleanText[cleanText!=""]
  
  #Remove all punctuation
  cleanText <- removePunctuation(cleanText)
  
  #Remove all numbers
  cleanText <- removeNumbers(cleanText)
  
  #Multiple whitespace characters trimmed to a single blank
  cleanText <- stripWhitespace(cleanText)
  
  split <- strsplit(cleanText, " ")[[1]]
  
  return(tail(split,2))
}

isNaOrEmpty<-function(text){
  return (is.na(text) || text =="" || nchar(text) == 0)
}


predictWord<- function (sentence, removeProfanity = FALSE){
  
  predictions <- data.frame(result=character(0), gram=integer(0), stringsAsFactors = FALSE)
  
  #Empty sentence
  if(isNaOrEmpty(sentence)){
    return(predictions)
  }
  
  searchTerms<-getSearchTerms(sentence)
  firstSearchTerm <- searchTerms[1]
  secondSearchTerm <- searchTerms[2]
  
  #No search terms
  if(isNaOrEmpty(firstSearchTerm) && isNaOrEmpty(secondSearchTerm)){
    return(predictions)
  }
  
  #3gram search
  if(!isNaOrEmpty(firstSearchTerm) && !isNaOrEmpty(secondSearchTerm))
  {
    trigramSearchTerm <- paste(firstSearchTerm, secondSearchTerm, sep = " ")
    trigramsResult <- cleanedTrigrams[cleanedTrigrams$search == trigramSearchTerm,]
    trigramsResult <- as.data.frame(head(trigramsResult,15)[,"result"], stringsAsFactors = FALSE)
    if(nrow(trigramsResult)>0){
      trigramsResult$gram = 3
      colnames(trigramsResult)[1] <- "result"
      predictions <- rbind(predictions, trigramsResult)
    }
  }
  
  bigramSearchTerm <- ifelse(isNaOrEmpty(secondSearchTerm), firstSearchTerm, secondSearchTerm)
  
  #2gram search
  bigramsResult <- cleanedBigrams[cleanedBigrams$search == bigramSearchTerm,]
  bigramsResult <- bigramsResult[with(bigramsResult, order(-freq)), ]
  bigramsResult <- as.data.frame(head(bigramsResult,15)[,"result"], stringsAsFactors = FALSE)
  if(nrow(bigramsResult)>0){
    colnames(bigramsResult)[1] <- "result"
    bigramsResult$gram <- 2
    predictions <- rbind(predictions, bigramsResult)
  }
  
  #Append Unigram
  unigramsResult <- as.data.frame(head(cleanedUnigrams,15)[,"word"], stringsAsFactors = FALSE)
  unigramsResult$gram <- 1
  colnames(unigramsResult)[1] <- "result"
  predictions <- rbind(predictions, unigramsResult)
  
  #Unique
  predictions <- predictions[!duplicated(predictions[,c('result')]),]
  
  #Remove Profanity
  if(removeProfanity)
  {
     predictions <- predictions[!predictions$result %in% profanities$profanity, ,drop=F]
  }
  
  #Get unique and limit to 10
  predictions <- head(unique(predictions, fromLast = FALSE),10)
  
  return (predictions)
}

shinyServer(function(input, output, clientData, session) {
  
  predictions <- data.frame(result=character(0), gram=integer(0), stringsAsFactors = FALSE)
  
  output$suggestions <- renderUI({
    
    sentence <- as.character(input$sentence)
    removeProfanity <- input$removeProfanity
    predictions <<- predictWord(sentence, removeProfanity)
    
    if(nrow(predictions) != 0){
      lapply(1:nrow(predictions), function(i) {
        id <- paste0("addWord",i)
        gram <- predictions$gram[i]
        buttonClass<-"info"
        if(gram==3){
          buttonClass<-"success"
        }
        if(gram==2){
          buttonClass<-"warning"
        }
        
        bsButton(id, predictions$result[i], style = buttonClass)
      })
    }
  })
  
  observeEvent(input$addWord1, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[1] ))
  })
  
  observeEvent(input$addWord2, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[2] ))
  })
  
  observeEvent(input$addWord3, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[3] ))
  })
  
  observeEvent(input$addWord4, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[4] ))
  })
  
  observeEvent(input$addWord5, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[5] ))
  })
  
  observeEvent(input$addWord6, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[6] ))
  })
  
  observeEvent(input$addWord7, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[7] ))
  })
  
  observeEvent(input$addWord8, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[8] ))
  })
  
  observeEvent(input$addWord9, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[9] ))
  })
  
  observeEvent(input$addWord10, {
    updateTextInput(session, "sentence", value = paste(as.character(input$sentence), predictions$result[10] ))
  })
})