Word Predictor
========================================================
author: Tongyi Koh
date: 18 Jan 2016

Predicts the next 10 words for you
========================================================

![alt text](image1.png)

The app predicts the next 10 possible words for a given partial phrase or sentence. Click on the word to append it to your text. Inappropriate or explicit words can be optionally removed from the suggestions.

Ordered by Accuracy
========================================================

![alt text](image2.png)

The suggestion are coloured in green, yellow and blue to the signify the accuracy of the prediction. 

Algorithm
========================================================

This App uses n-gram models for the next word prediction algorithm.

<small>1 million lines of text data(Twitter, Blogs and News) were used and cleaned by removing punctuations, urls, non-alpha numeric characters and numbers. Unigram, Bigram and Trigram were extracted from the cleaned data and the count(frequency) of the terms is computed. For a more efficient search, the extract ngram were processed and reduced to top 15 frequent result. Eg. For the Trigram terms "eat something now", "eat something later", only the top 15 following words, based on frequency, of the search term "eat something" is stored for use with the app.<br><br>Search is done using recursively backoff with a weaker gram each time. The app will try to search with trigram, followed by bigram and then join with unigram. The unique results is extracted and the top 10 words is rendered to the app.</small>

Thank you and Enjoy
========================================================
<br>
<br>
https://tongyikoh.shinyapps.io/WordPredictor/

