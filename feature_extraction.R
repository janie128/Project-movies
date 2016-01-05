### ------------------------------------------------------------
###  This function performs feature extraction grouped by movie
### ------------------------------------------------------------

extractFeature <- function(reviewsTitles){
  library(dplyr)
  library(tidyr)
  
  # Reviews word count
  words <- as.data.frame(sapply(gregexpr("\\S+", reviewsTitles$reviewText), length))
  colnames(words) <- "wordCount"
  reviewsTitles <- cbind(reviewsTitles, words)
  rm(words)
  
  # Generate table with review word count info including quantiles and average
  wordCountInfo <- reviewsTitles %>% group_by(movieTitle) %>%
    summarize(wordCountLow = quantile(wordCount, probs=0.25), wordCountMid = quantile(wordCount, probs=0.5), 
              wordCountHigh = quantile(wordCount, probs=0.75), wordCountAvg = round(mean(wordCount),1))
  
  # Generate table with review count and overall score grouped by each movieTitle
  reviewCount <- reviewsTitles %>% group_by(movieTitle) %>% 
    summarize(reviewCount=n(), reviewScore=round(mean(overall),2))
  
  # Generate table with review count grouped by rating stars
  reviewStar <- reviewsTitles %>% group_by(movieTitle, overall) %>%
    summarize(reviewStarCount=n())
  reviewStar$overall <- as.factor(reviewStar$overall)
  reviewStar <- spread(reviewStar, overall, reviewStarCount)
  colnames(reviewStar) <- c("movieTitle", "star1Count", "star2Count", "star3Count", "star4Count", "star5Count")
  reviewStar[is.na(reviewStar)] <- 0
  # Percentage of total count
  reviewStar$totalCount <- reviewCount$reviewCount
  reviewStar$star1Count <- round(reviewStar$star1Count/reviewStar$totalCount, 4)
  reviewStar$star2Count <- round(reviewStar$star2Count/reviewStar$totalCount, 4)
  reviewStar$star3Count <- round(reviewStar$star3Count/reviewStar$totalCount, 4)
  reviewStar$star4Count <- round(reviewStar$star4Count/reviewStar$totalCount, 4)
  reviewStar$star5Count <- round(reviewStar$star5Count/reviewStar$totalCount, 4)
  reviewStar$totalCount <- NULL
  
  # Call wordAnalysisFn to generate good and bad words TFIDF for each review
  finalTFIDF <- wordAnalysisFn(reviewsTitles)
  reviewsTitles <- cbind(reviewsTitles, finalTFIDF)
  
  # Generate table with average good and bad TFIDF grouped by each movieTitle
  reviewsTitles <- group_by(reviewsTitles, movieTitle)
  TFIDF <- reviewsTitles %>% group_by(movieTitle) %>%
    summarize(goodAvgTFIDF = round(mean(goodTFIDF), 4), badAvgTFIDF = round(mean(badTFIDF), 4))
  
  # Inner join reviewCount, boxOffice, reviewStar, wordCountInfo, TFIDF
  final <- inner_join(reviewCount, boxOffice, by = c("movieTitle" = "title"))
  final <- inner_join(final, reviewStar, by = "movieTitle")
  final <- inner_join(final, wordCountInfo, by = "movieTitle")
  final <- inner_join(final, TFIDF, by = "movieTitle")
  
  return(final)
}