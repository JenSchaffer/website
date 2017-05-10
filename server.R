library(shiny)
library(tm)
library(wordcloud)
library(twitteR)
library(syuzhet) 

shinyServer(function(input, output, session) {
  consumer_key <- "hh3tbRo5PpFpd0qEfmGXenSsS"
  consumer_secret <- "GVcQOuQ8eLzmxYfqyEU6dYvzzGFJPYKc2aR4Lp5JK195Xb7NSs"
  access_token <- "482413194-JfqqJe7Vh0MAnZnfraGfQ5BvcTOoIoF1Ocm0FGPQ"
  access_secret <- "HkE9exoVyqngXbaYe04912NEOPmPX6wHlm292D1GzsmMW"
  library(twitteR)
  setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
  token <- get("oauth_token", twitteR:::oauth_cache)
  token$cache()
  output$currentTime <- renderText({invalidateLater(1000, session)
    paste("Current time is: ",Sys.time())})
  observe({
    invalidateLater(60000,session)
    count_positive = 0
    count_negative = 0
    count_neutral = 0
    positive_text <- vector()
    negative_text <- vector()
    neutral_text <- vector()
    vector_users <- vector()
    vector_sentiments <- vector()
    tweets_result = ""
    tweets_result = searchTwitter("Trump", n=1500, resultType = "recent")
    for (tweet in tweets_result){
      print(paste(tweet$screenName, ":", tweet$text))
      usableText = gsub("[^[:graph:]]", " ",tweet$text)
      clean_tweet = gsub("&amp", "", usableText)
      clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
      clean_tweet = gsub("@\\w+", "", clean_tweet)
      clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
      clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
      clean_tweet = gsub("http\\w+", "", clean_tweet)
      clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
      clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
      clean_tweet = gsub("http://t.co/[a-z,A-Z,0-9]*{8}","", clean_tweet)
      s_v <- as.character(clean_tweet)
      vector_users <- c(vector_users, as.character(tweet$screenName));
      if ((get_sentiment(s_v, method = "syuzhet") >0) == TRUE){
        count_positive = count_positive + 1
        print("positivo")
        vector_sentiments <- c(vector_sentiments, "Positive")
        positive_text <- c(positive_text, as.character(tweet$text))
      } else if ((get_sentiment(s_v, method = "syuzhet") <0) == TRUE) { 
        count_negative = count_negative + 1
        print("negativo")
        vector_sentiments <- c(vector_sentiments, "Negative")
        negative_text <- c(negative_text, as.character(tweet$text))
      } else {
        count_neutral = count_neutral + 1
        print("neutral")
        vector_sentiments <- c(vector_sentiments, "Neutral")
        neutral_text <- c(neutral_text, as.character(neutral_text))
      }
    }
    df_users_sentiment <- data.frame(vector_users, vector_sentiments)
    output$tweets_table = renderDataTable({
      df_users_sentiment
    })
    
    output$distPlot <- renderPlot({
      results = data.frame(tweets = c("Positive", "Negative", "Neutral"), numbers = c(count_positive,count_negative,count_neutral))
      barplot(results$numbers, names = results$tweets, xlab = "Sentiment", ylab = "Counts", col = c("Green","Red","Blue"))
      if (length(positive_text) > 0){
        positive_corpus = Corpus(VectorSource(paste(positive_text, collapse = " ")))
        tweets_transformed1 <- tm_map(positive_corpus, content_transformer(function(x)
                                      iconv(x, to="UTF-8")))
        tweets_transformed2 <- tm_map(tweets_transformed1,
                                      content_transformer(tolower))
        tweets_transformed3 <- tm_map(tweets_transformed2,
                                      removePunctuation)
        tweets_transformed4 <- tm_map(tweets_transformed3,function(x) 
                                      x=removeWords(x,stopwords("english")))
        tweets_transformed5 <- tm_map (tweets_transformed4, removeNumbers) 
        tweets_transformed6 <- gsub("rt", "", tweets_transformed5)
        tweets_transformed7 <- gsub("@\\w+", "", tweets_transformed6)
        tweets_transformed8 <- gsub("http\\w+", "", tweets_transformed7)
        tweets_transformed9 <- gsub("[ |\t]{2,}", "", tweets_transformed8)
        tweets_transformed10 <- gsub("^ ", "", tweets_transformed9)
        pos_tweets <- gsub(" $", "", tweets_transformed10)
        output$positive_wordcloud <- renderPlot({ wordcloud(pos_tweets, min.freq = 0, random.color=TRUE, max.words=75 ,colors=brewer.pal(8, "Dark2"))  })
      }
      if (length(negative_text) > 0) {
        negative_corpus = Corpus(VectorSource(paste(negative_text, collapse = " ")))
        tweets_transformed1 <- tm_map(negative_corpus, content_transformer(function(x)iconv(x, to="UTF-8")))
        tweets_transformed2 <- tm_map(tweets_transformed1,
                                      content_transformer(tolower))
        tweets_transformed3 <- tm_map(tweets_transformed2,
                                      removePunctuation)
        tweets_transformed4 <- tm_map(tweets_transformed3,function(x) 
          x=removeWords(x,stopwords("english")))
        tweets_transformed5 <- tm_map (tweets_transformed4, removeNumbers) 
        tweets_transformed6 <- gsub("rt", "", tweets_transformed5)
        tweets_transformed7 <- gsub("@\\w+", "", tweets_transformed6)
        tweets_transformed8 <- gsub("http\\w+", "", tweets_transformed7)
        tweets_transformed9 <- gsub("[ |\t]{2,}", "", tweets_transformed8)
        tweets_transformed10 <- gsub("^ ", "", tweets_transformed9)
        neg_tweets <- gsub(" $", "", tweets_transformed10)
        output$negative_wordcloud <- renderPlot({ wordcloud(neg_tweets, random.color=TRUE,  min.freq = 0, max.words=75 ,colors=brewer.pal(8,"Set3"))  })
      }
      if (length(neutral_text) > 0){
        neutral_corpus = Corpus(VectorSource(paste(neutral_text, collapse = " ")))
        tweets_transformed1 <- tm_map(neutral_corpus, content_transformer(function(x)iconv(x, to="UTF-8")))
        tweets_transformed2 <- tm_map(tweets_transformed1,
                                      content_transformer(tolower))
        tweets_transformed3 <- tm_map(tweets_transformed2,
                                      removePunctuation)
        tweets_transformed4 <- tm_map(tweets_transformed3,function(x) 
          x=removeWords(x,stopwords("english")))
        tweets_transformed5 <- tm_map (tweets_transformed4, removeNumbers) 
        tweets_transformed6 <- gsub("rt", "", tweets_transformed5)
        tweets_transformed7 <- gsub("@\\w+", "", tweets_transformed6)
        tweets_transformed8 <- gsub("http\\w+", "", tweets_transformed7)
        tweets_transformed9 <- gsub("[ |\t]{2,}", "", tweets_transformed8)
        tweets_transformed10 <- gsub("^ ", "", tweets_transformed9)
        neutral_tweets <- gsub(" $", "", tweets_transformed10)
        output$neutral_wordcloud <- renderPlot({ wordcloud(neutral_tweets, min.freq = 0, random.color=TRUE , max.words=75 ,colors=brewer.pal(8, "Dark2"))  })
      }
    })
  })
})