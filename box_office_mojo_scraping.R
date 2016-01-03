scrapingBOFn <- function(){

  library(rvest); library(lubridate)

  # The following URL and CSS templates rely on the layout of the website. They may change over time. 
  pageUrlTemplate <- "http://www.boxofficemojo.com/daily/?view=bymovie&yr=%s&page=%d&sort=title&order=ASC"
  years <- c("2015", "2014", "2013", "2012", "2011",
            "2010", "2009", "2008", "2007", "2006",
            "2005", "2004", "2003", "2002", "pre2002")
  remainingPageLinkCss <- "font:nth-child(4) a" # how many page remaining
  movieTitleCss <- "tr+ tr td:nth-child(1) font" # movie title
  studioCss <- "tr+ tr td:nth-child(2) font" # studio
  numDaysCss <- "tr+ tr td:nth-child(3) font" # number of days
  releaseGrossCss <- "tr+ tr td:nth-child(4) font" # release gross
  releaseDateCss <- "tr+ tr td:nth-child(5) font" # release date

  # Empty data frame for accommodating scraping result.
  boxOffice <- data.frame(
    title = character(),
    gross = numeric(),
    releaseDate = character())

  # Function for parsing the html page and extracting data of interest.
  extractBoxOfficeFn <- function(page, boxOffice) {
    extractedTitles <- html_text(html_nodes(page, movieTitleCss))
    extractedReleaseDates <- html_text(html_nodes(page, releaseDateCss))
    # Extract release gross, strip "$" and ",", and cast to number.
    extractedReleaseGrosses <- html_text(html_nodes(page, releaseGrossCss))
    extractedReleaseGrosses <- gsub("\\$", "", extractedReleaseGrosses)
    extractedReleaseGrosses <- gsub(",", "", extractedReleaseGrosses)
    extractedReleaseGrosses <- as.numeric(extractedReleaseGrosses)

    extractedFromPage <- data.frame(
        title = extractedTitles,
        gross = extractedReleaseGrosses,
        releaseDate = extractedReleaseDates)
    return(extractedFromPage)
  }

  for (year in years) {
    print(sprintf("parsing %s...", year))
    pageUrl <- sprintf(pageUrlTemplate, year, 1);
    page <- read_html(pageUrl)
    numRemainingPages = length(html_text(html_nodes(page, remainingPageLinkCss)))
  
    boxOffice <- rbind(boxOffice, extractBoxOfficeFn(page, boxOffice))
    for (pageIndex in 1:numRemainingPages) {
      pageUrl <- sprintf(pageUrlTemplate, year, pageIndex + 1);
      page <- read_html(pageUrl)
      boxOffice <- rbind(boxOffice, extractBoxOfficeFn(page, boxOffice))
    }
  }

  # Change factor columns to string type.
  boxOffice$title <- as.character(boxOffice$title)
  boxOffice$releaseDate <- as.character(boxOffice$releaseDate)
  boxOffice$releaseDate <- as.Date(boxOffice$releaseDate, format="%m/%d/%y")

  return(boxOffice)
}