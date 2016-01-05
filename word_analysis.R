### -----------------------------------------------------
###  This wordAnalysisFn generates TFIDF for each review
### -----------------------------------------------------

wordAnalysisFn <- function(reviewsTitles){
  library(tm)
  library(dplyr)
  library(parallel)

  ## --------------------------
  ##       Functions area
  ## --------------------------
  # A) Function for generating vector of words in each review (return is class char vector)
  reviewToWordsFn <- function(text){
    text_source <- VectorSource(text)
    textCorpus <- Corpus(text_source)
    textCorpus <- tm_map(textCorpus, content_transformer(tolower))
    textCorpus <- tm_map(textCorpus, removePunctuation)
    textCorpus <- tm_map(textCorpus, stripWhitespace)
    dtm <- as.matrix(DocumentTermMatrix(textCorpus, control=list(wordLengths=c(1,Inf))))
    return(dtm)
  }

  # B) Function for generating good OR bad TF-IDF from list of words, returns list of TF-IDF (good or bad)
  wordsToSentiTfidfFn <- function(listOfWordsFreq, maxTF, sentiDict){
    listOfSentiWords <- intersect(listOfWordsFreq$words, sentiDict$words) # char vector of words that are good/bad
    listOfSentiWordsTFIDF <- subset(listOfWordsFreq, words %in% listOfSentiWords) # df of good/bad words with their term frequencies
    listOfSentiWordsTFIDF$TF <- round(listOfSentiWordsTFIDF$freq/maxTF, 5) # raw TF of word divided by total word count in document
    listOfSentiWordsIDF <- subset(sentiDict, words %in% listOfSentiWords)
    listOfSentiWordsTFIDF$IDF <- listOfSentiWordsIDF$IDF
    rm(listOfSentiWordsIDF)
    listOfSentiWordsTFIDF$TFIDF <- mapply(function(x,y) {x*y},
                                          listOfSentiWordsTFIDF$TF, listOfSentiWordsTFIDF$IDF) # TF*IDF
    return(listOfSentiWordsTFIDF)
  }

  # C) Function for generating total TF-IDF from reviews. Calls functions A & B.
  #    Returns a vector with sum of good words and bad words TFIDF (for each input review).
  reviewToTfidfFn <- function(text){
    listOfWordsFreq <- reviewToWordsFn(text) # matrix of all words (as colname) with their frequencies
    maxTF <- max(listOfWordsFreq) # numeric, maximum frequency of above matrix
    listOfWords <- colnames(listOfWordsFreq) # char vector of all words
    listOfWordsFreq <- cbind(words=listOfWords, freq=listOfWordsFreq[1,]) # matrix of words & freq
    row.names(listOfWordsFreq) <- NULL
    rm(listOfWords)
    listOfWordsFreq <- as.data.frame(listOfWordsFreq, stringsAsFactors = FALSE) # df of words & freq
    listOfWordsFreq$freq <- as.numeric(listOfWordsFreq$freq)
  
    # call for good words TF-IDF list
    goodTFIDFList <- wordsToSentiTfidfFn(listOfWordsFreq, maxTF, goodDict)
    # call for bad words TF-IDF list
    badTFIDFList <- wordsToSentiTfidfFn(listOfWordsFreq, maxTF, badDict)
  
    goodTFIDF <- ifelse(dim(goodTFIDFList)[1]!=0, sum(goodTFIDFList$TFIDF), 0)
    badTFIDF <- ifelse(dim(badTFIDFList)[1]!=0, sum(badTFIDFList$TFIDF), 0)

    return(c(goodTFIDF, badTFIDF))
  }  
  ## ----------------------Functions area END


  # -------------------------------------------------------------
  # Load good/bad sentiment words list and convert regex symbols
  # -------------------------------------------------------------
  goodDict <- read.csv("data/positive words.csv", header=FALSE,
                       stringsAsFactors = FALSE, col.names="words") # char data frame
  badDict <- read.csv("data/negative words.csv", header=FALSE,
                       stringsAsFactors = FALSE, col.names="words") # char data frame
  tempList <- lapply(goodDict$words, function(x) {gsub("\\+", "\\\\\\\\+", x)})
  tempList <- lapply(tempList, function(x) {gsub("\\*", "\\\\\\\\*", x)})
  tempList <- lapply(tempList, function(x) {gsub("\\|", "\\\\\\\\|", x)})
  goodDict$regex <- tempList
  tempList <- lapply(badDict$words, function(x) {gsub("\\+", "\\\\\\\\+", x)})
  tempList <- lapply(tempList, function(x) {gsub("\\*", "\\\\\\\\*", x)})
  tempList <- lapply(tempList, function(x) {gsub("\\|", "\\\\\\\\|", x)})
  badDict$regex <- tempList
  rm(tempList)
  # Remove any duplicate entries in both dictionaries
  goodDict <- subset(goodDict, !duplicated(goodDict$words))
  badDict <- subset(badDict, !duplicated(badDict$words))
  # -----


  # Parallel processing
  numCores <- min(3, detectCores() - 1)
  cluster <- makeCluster(numCores)
  # parLapply used instead of lapply for parallel processing

  # -----------------------------
  # Create vector of all reviews 
  # -----------------------------
  # Account for reviews that forgot to put spaces after punctuation, 
  # so there will still be space between words after punctuation is removed.
  tempReviews <- paste(reviewsTitles$reviewText, reviewsTitles$summary, sep=" ")
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub(",", ", ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub("\\.", "\\. ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub("\\?", "\\? ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub("!", "! ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub(";", "; ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub(":", ": ", x)})
  tempReviews <- parLapply(cluster, tempReviews, function(x) {gsub("&", "& ", x)})
  textAllReviews <- tolower(removePunctuation(unlist(tempReviews)))
  rm(tempReviews)
  # -----
  # Turn off parallel
  stopCluster(cluster)
  rm(cluster)


  # -----------------------
  # Adding IDF to goodDict
  # -----------------------
  totalDoc <- length(textAllReviews) # total number of documents

  goodDictRegex <- goodDict$regex
  goodDictContain <- numeric(length(goodDictRegex))

  # Parallel processing & exporting data needed in this segment
  numCores <- min(3, detectCores() - 1)
  cluster <- makeCluster(numCores)
  clusterExport(cluster, "textAllReviews")
  # parLapply used instead of lapply for parallel processing

  # --Done in chunks to ensure data is saved if anything happens
  # Count number of documents(reviews) that contain the good word (loops through goodDict)
  limit <- length(goodDictRegex)
  begin <- 1
  end <- 50

  while (begin <= limit){
    realEnd = min(limit, end)
    
    results <- parLapply(cluster, goodDictRegex[begin:realEnd], function(x) {
      sum(grepl(paste("\\<", x, "\\>", sep = ""), textAllReviews))
    })
    goodDictContain[begin:realEnd] <- results
  
    # For monitoring progress
    print(paste(end, " out of ", limit))
  
    begin <- begin + 50
    end <- end + 50
  }
  goodDict$contain <- goodDictContain
  # Turn off parallel
  stopCluster(cluster)
  rm(cluster)
  
  # Calculate inverse document frequency with denominator +1
  goodDict$IDF <- lapply(goodDict$contain, function(x) {round(log10(totalDoc/(1 + x)),4)})
  # -----


  # -----------------------
  # Adding IDF to badDict
  # -----------------------
  badDictRegex <- badDict$regex
  badDictContain <- numeric(length(badDictRegex))

  # Parallel processing & exporting data needed in this segment
  numCores <- min(3, detectCores() - 1)
  cluster <- makeCluster(numCores)
  clusterExport(cluster, "textAllReviews")
  # parLapply used instead of lapply for parallel processing

  # Done in chunks to ensure data is saved if anything happens
  # Count number of documents(reviews) that contain the bad word (loops through badDict)
  limit <- length(badDictRegex)
  begin <- 1
  end <- 50

  while (begin <= limit){
    realEnd = min(limit, end)
  
    results <- parLapply(cluster, badDictRegex[begin:realEnd], function(x) {
      sum(grepl(paste("\\<", x, "\\>", sep = ""), textAllReviews))
    })
    badDictContain[begin:realEnd] <- results
  
    # For monitoring progress
    print(paste(end, " out of ", limit))
  
    begin <- begin + 50
    end <- end + 50
  }
  badDict$contain <- badDictContain
  # Turn off parallel
  stopCluster(cluster)
  rm(cluster)

  # Calculate inverse document frequency with denominator +1
  badDict$IDF <- lapply(badDict$contain, function(x) {round(log10(totalDoc/(1 + x)),4)})
  # -----

  # -------------------------------------
  # Run reviewToTfidfFn() on all reviews
  # -------------------------------------
  finalTFIDF <- data.frame(numeric(), numeric())
  for (i in 1:length(textAllReviews)){
    finalTFIDF <- rbind(finalTFIDF, reviewToTfidfFn(textAllReviews[i]))
    
    if (i %% 1000 == 0) {
      print(paste(i, " out of ", length(textAllReviews)))
    }
  }
  colnames(finalTFIDF) <- c("goodTFIDF", "badTFIDF")
  
  return(finalTFIDF)
}
