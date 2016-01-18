
#Load file
bFile <- file("cleaned/b.txt", "r")
bData <- readLines(bFile, encoding="UTF-8", skipNul = TRUE)
close(bFile)

tFile <- file("cleaned/t.txt", "r")
tData <- readLines(tFile, encoding="UTF-8", skipNul = TRUE)
close(tFile)

nFile <- file("cleaned/n.txt", "r")
nData <- readLines(nFile, encoding="UTF-8", skipNul = TRUE)
close(nFile)

r500kData <- c(bData[sample(899188, 100000)], nData[sample(1010241, 200000)], tData[sample(2360145, 200000)])

r10kData <- c(bData[sample(899188, 2000)], nData[sample(1010241, 4000)], tData[sample(2360145, 4000)])

r100kData <- c(bData[sample(899188, 20000)], nData[sample(1010241, 40000)], tData[sample(2360145, 40000)])

r300kData <- c(bData[sample(899188, 150000)], nData[sample(1010241, 150000)], tData[sample(2360145, 100000)])

allData <- c(bData, nData, tData)

r1mData <- c(bData[sample(899188, 400000)], nData[sample(1010241, 400000)], tData[sample(2360145, 200000)])


rm(bData)
rm(nData)
rm(tData)

library(tm)
library(RWeka)
library(tau)


r500kDataCorpus <- Corpus(VectorSource(r500kData))

r10kDataCorpus <- Corpus(VectorSource(r10kData))

save(r300kData, file="data/r300kData.rda")

allCorpus <- Corpus(VectorSource(allData))

save(r10kDataCorpus, file="corpus/r10kDataCorpus.rda")


load(file="corpus/r100kDataCorpus.rda")
load(file="gram/bigrams.rda")

OneGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 1, max = 1))
TwoGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2))
TriGramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))


dtm1Gram <- DocumentTermMatrix(corpus,control = list(tokenize = OneGramTokenizer))
saveRDS(dtm1Gram,paste0('dtm/dtm1Gram.rds'))
rm(dtm1Gram)

dtm2Gram <-DocumentTermMatrix(corpus,control = list(tokenize = TwoGramTokenizer))
saveRDS(dtm2Gram,paste0('dtm/dtm2Gram.rds'))
rm(dtm2Gram)

dtm3Gram <-DocumentTermMatrix(corpus,control = list(tokenize = TriGramTokenizer))
saveRDS(dtm3Gram,paste0('dtm/dtm3Gram.rds'))
rm(dtm3Gram)

searches<-unique(bigrams$search)

unigrams <- textcnt(data, method="string",n=1,split = "[[:space:]]+", decreasing=TRUE) 

bigrams <- textcnt(data, method="string",n=2,split = "[[:space:]]+", decreasing=TRUE); 
bigrams <- data.frame(freq = unclass(bigrams))
bigrams <- data.frame(word=rownames(as.data.frame(bigrams)),freq = unclass(bigrams))

library(doSNOW)

cl<-makeCluster(3)
registerDoSNOW(cl)


bigrams$word <- as.character(bigrams$word)

bigrams$result<-""

bigrams$search<-""

library(stringi)

foreach(i=1:nrow(bigrams)) %do%
{
  word<-bigrams$word[i]
  splited<-stri_split_fixed(word, " ")[[1]]
  bigrams$search[i] <-splited[1]
  bigrams$result[i] <-splited[2]
}


stopCluster(cl)

cleanedBigrams <- data.frame(word=character(0) , freq= integer(0), result = character(0), search = character(0))

cleanedBigrams <- data.frame(word=character(0) , freq= integer(0), result = character(0), search = character(0), stringsAsFactors = FALSE)


cleanedTrigrams <- data.frame(word=rep(NA,2646610) , freq= rep(NA,2646610), result = rep(NA,2646610), search = rep(NA,2646610), stringsAsFactors = FALSE)

for(i in 1:nrow(searches))
{
  if(i %% 1000 == 0){
    print(i)
  }
  search<-searches$searches[i]
  group<-bigrams[bigrams$search == search,]
  sortedGroup <- group[with(group, order(-freq)), ]
  cleanedBigrams<-rbind(cleanedBigrams, head(sortedGroup,15))
}

addIndex <- 0

dfList<-list()

for(i in 1:nrow(searches))
{
  if(i %% 100 == 0){
    print(i)
  }
  search<-searches$searches[i]
  group<-trigrams[trigrams$search == search,]
  sortedGroup <- head(group,15)
  cleanedTrigrams<-bind_rows(cleanedTrigrams, sortedGroup)
}

#########

load(file="gram/cleanedBigrams.rda")
load(file="gram/cleanedTrigrams.rda")
load(file="gram/cleanedUnigrams.rda")

cleanedBigrams <- readRDS(gzcon("https://github.com/tongyi87/Capstone/blob/f948bd64232cabd61dad5411a66a3fea3b36db72/gram/cleanedBigrams.rds?raw=true"))


library(tm)
library(tau)

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
  return (is.na(text) || text =="" || text == character(0))
}


predictWord<- function (sentence){
  
  predictions <- data.frame(result=character(0), stringsAsFactors = FALSE)
  
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
      colnames(trigramsResult)[1] <- "result"
      predictions <- rbind(predictions, trigramsResult)
  }
  
  bigramSearchTerm <- ifelse(isNaOrEmpty(secondSearchTerm), firstSearchTerm, secondSearchTerm)
  
  #2gram search
  bigramsResult <- cleanedBigrams[cleanedBigrams$search == bigramSearchTerm,]
  bigramsResult <- bigramsResult[with(bigramsResult, order(-freq)), ]
  bigramsResult <- as.data.frame(head(bigramsResult,15)[,"result"], stringsAsFactors = FALSE)
  colnames(bigramsResult)[1] <- "result"
  predictions <- rbind(predictions, bigramsResult)
 
  #Append Unigram
  unigramsResult <- as.data.frame(head(cleanedUnigrams,15)[,"word"], stringsAsFactors = FALSE)
  colnames(unigramsResult)[1] <- "result"
  predictions <- rbind(predictions, unigramsResult)
  
  #Get unique and limit to 10
  predictions <- head(unique(predictions, fromLast = FALSE),10)
  
  return (predictions)
}


http://www.bannedwordlist.com/lists/swearWords.csv - profanity=
  
  read.csv(“swearWords.csv”,header=F); corpus=tm_map(corpus, removeWords,
                                                     
                                                     profanity); -







