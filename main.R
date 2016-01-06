# Call script with scraping box office function
source('./box_office_mojo_scraping.R', echo=FALSE)
boxOfficeRaw <- scrapingBOFn()

# Call script to clean box office data
source('./clean_explore_BOdata.R', echo=FALSE)
boxOffice <- cleaningBOFn(boxOfficeRaw)

# Call script to match product titles in metadata file to movie titles in box office data
noTvMetadata <- 
  
# Call script to clean reviews + metadata files
source('./clean_reviewsTitles.R', echo=FALSE)
reviewsTitles <- cleanReviewsTitles(noTvMetadata)

# Call script to extract features
source('./feature_extraction.R', echo=FALSE)
final <- extractFeatures(reviewsTitles)

# Call script for modelling and prediction




#g <- ggplot(filter(final, (reviewCount>50 & reviewCount<2000)), aes(x=goodAvgTFIDF, y=log10(adjGross), colour=adjGross))
#g + geom_point() + scale_color_gradient(low="green", high="red")