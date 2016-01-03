# Call script with scraping box office function
source('./box_office_mojo_scraping.R', echo=FALSE)
boxOfficeRaw <- scrapingBOFn()

# Call script to clean box office data
source('./clean_explore_BOdata.R', echo=FALSE)
boxOffice <- cleaningBOFn(boxOfficeRaw)
