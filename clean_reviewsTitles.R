cleanReviewsTitles <- function(noTvMetadata){

  library(jsonlite)
  library(dplyr)
  
  metaData <- noTvMetadata
  reviews <- stream_in(file("./data/reviews_Movies_and_TV_strict.json"))

  # Remove unnecessary variables
  metaData$price <- NULL; metaData$description <- NULL; metaData$categories <- NULL
  # Remove movie titles with no match
  metaData <- subset(metaData, !(metaData$movieTitle %in% "N/A"))
  # Create subset where movie titles have characater length > 4, to minimize mismatching
  metaData <- subset(metaData, nchar(metaData$movieTitle)>4)
  ## Creating subset removing movie titles that are only one word (with "The" removed)
  ## metaData$check <- sapply(gregexpr("\\S+", sub("^The", "", metaData$movieTitle)), length)!=1
  ## colnames(metaData)[6] <- "check"
  ## metaData <- subset(metaData, metaData$check %in% "TRUE")
  ## metaData$check <- NULL


  # Inner join the reviews and metadata tables, by "asin"
  reviewsTitles <- inner_join(reviews, metaData, by="asin")
  # Remove unnecessary variables
  reviewsTitles$reviewerName <- NULL; reviewsTitles$helpful <- NULL;
  reviewsTitles$unixReviewTime <- NULL; reviewsTitles$title <- NULL;
  # realTitle is class list. Convert to class char
  reviewsTitles$movieTitle <- unlist(reviewsTitles$movieTitle)

  return(reviewsTitles)
}