# Call script with scraping box office function
source('./box_office_mojo_scraping.R', echo=FALSE)
boxOfficeRaw <- scrapingBOFn()

# Call script to clean box office data
source('./clean_explore_BOdata.R', echo=FALSE)
boxOffice <- cleaningBOFn(boxOfficeRaw)

# Call script to match product titles in metadata file to movie titles in box office data
source('./parse_real_title.R', echo=FALSE)
noTvMetadata <- realTitle(boxOffice)
  
# Call script to clean reviews + metadata files
source('./clean_reviewsTitles.R', echo=FALSE)
reviewsTitles <- cleanReviewsTitles(noTvMetadata)

# Call script to extract features
source('./feature_extraction.R', echo=FALSE)
final <- extractFeatures(reviewsTitles)

# Call script for modelling and prediction
source('./machine_learning.R', echo=FALSE)

