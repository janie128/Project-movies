### ------------------------------------------------------------
###  This function performs feature extraction grouped by movie
### ------------------------------------------------------------

extractFeatures <- function(reviewsTitles){
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)

  # ---------Reviews word count
  words <- as.data.frame(sapply(gregexpr("\\S+", reviewsTitles$reviewText), length))
  colnames(words) <- "wordCount"
  reviewsTitles <- cbind(reviewsTitles, words)
  rm(words)
  
  # ---------Generate table with review word count info including quantiles and average
  wordCountInfo <- reviewsTitles %>% group_by(movieTitle) %>%
    summarize(wordCountLow = quantile(wordCount, probs=0.25), wordCountMid = quantile(wordCount, probs=0.5), 
              wordCountHigh = quantile(wordCount, probs=0.75), wordCountAvg = round(mean(wordCount),1))
  
  # ---------Generate table with review count and overall score grouped by each movieTitle
  reviewCount <- reviewsTitles %>% group_by(movieTitle) %>% 
    summarize(reviewCount=n(), reviewScore=round(mean(overall),2))
  
  # ---------Generate table with review count grouped by rating stars
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
  
  # ---------Call wordAnalysisFn to generate good and bad words TFIDF for each review
  finalTFIDF <- wordAnalysisFn(reviewsTitles)
  reviewsTitles <- cbind(reviewsTitles, finalTFIDF)
  
  # ---------Generate table with average good and bad TFIDF grouped by each movieTitle
  reviewsTitles <- group_by(reviewsTitles, movieTitle)
  TFIDF <- reviewsTitles %>% group_by(movieTitle) %>%
    summarize(goodAvgTFIDF = round(mean(goodTFIDF), 4), badAvgTFIDF = round(mean(badTFIDF), 4))
  
  # ---------Inner join reviewCount, boxOffice, reviewStar, wordCountInfo, TFIDF
  final <- inner_join(reviewCount, boxOffice, by = c("movieTitle" = "title"))
  final <- inner_join(final, reviewStar, by = "movieTitle")
  final <- inner_join(final, wordCountInfo, by = "movieTitle")
  final <- inner_join(final, TFIDF, by = "movieTitle")

  # ---------Exploratory Graphs
  # Histogram of review counts
  png(filename="review_count.png", width=600, height=480)
  
  gg <- ggplot(final, aes(reviewCount))
  print(gg + geom_histogram(breaks=seq(0,2000,by=50), col="blue",fill="green", alpha=0.5) +
    xlim(c(0,2000)) + labs(x="Review Count", y="Frequency", title="Distribution of movie reviews count") +
    geom_vline(xintercept=50, colour="red", size=1) +
    theme(plot.title = element_text(size=20, face="bold"),
          axis.title.x = element_text(size=16), axis.text.x = element_text(size=14),
          axis.title.y = element_text(size=16), axis.text.y = element_text(size=14)))
  dev.off()
  
  # Sample plots of star distribution
  movie1 <- subset(reviewStar, movieTitle=="Ocean's Twelve")
  movie2 <- subset(reviewStar, movieTitle=="Pearl Harbor")
  movie3 <- subset(reviewStar, movieTitle=="Mean Girls")
  movieSample <- rbind(movie1,movie2, movie3); rm(movie1,movie2, movie3)
  png(filename="review_ratings.png", width=800, height=480)
  
  gg <- ggplot(movieSample, aes(x=overall, y=reviewStarCount, width=0.75))
  print(gg + geom_bar(stat="identity", position="identity", fill="dodgerblue3") + facet_wrap(~movieTitle, nrow=1, scales="free") +
          labs(x="Review Rating", y="Count") + 
          theme(axis.title.x = element_text(size=16), axis.text.x = element_text(size=14),
                axis.title.y = element_text(size=16), axis.text.y = element_text(size=14)))
  dev.off()
  rm(movieSample)
  
  # TFIDF vs. average score
  png(filename="TFIDF.png", width=1000, height=480)
  gg <- ggplot(filter(final, (reviewCount>50 & reviewCount<2000)), aes(x=reviewScore, y=goodAvgTFIDF, colour=reviewCount))
  plot1 <- gg + geom_point() + scale_color_gradient(low="red2", high="green") +
          labs(x="Review Average Score", y="Average Good TFIDF", title="Good Words TFIDF") + 
          theme(axis.title.x = element_text(size=16), axis.text.x = element_text(size=14),
                axis.title.y = element_text(size=16), axis.text.y = element_text(size=14),
                plot.title = element_text(size=20, face="bold"))
  gg <- ggplot(filter(final, (reviewCount>50 & reviewCount<2000)), aes(x=reviewScore, y=badAvgTFIDF, colour=reviewCount))
  plot2 <- gg + geom_point() + scale_color_gradient(low="red2", high="green") +
          labs(x="Review Average Score", y="Average Bad TFIDF", title="Bad Words TFIDF") + 
          theme(axis.title.x = element_text(size=16), axis.text.x = element_text(size=14),
                axis.title.y = element_text(size=16), axis.text.y = element_text(size=14),
                plot.title = element_text(size=20, face="bold"))
  grid.arrange(plot1, plot2, ncol=2)
  dev.off()
  
  return(final)
  
}