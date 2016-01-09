realTitle <- function(boxOffice){
  library(jsonlite)

  stripLeadingWordFn <- function(title, word) {
    if (regexpr(word, tolower(title)) == 1) {
      return(substr(title, start = nchar(word) + 1, stop = nchar(title)))
    }
    return(title)
  }

  latinNumberToNumberFn <- function(input) {
    output <- input
    output = gsub("\\<II\\>", "2", output, ignore.case = TRUE) # 2
    output = gsub("\\<III\\>", "3", output, ignore.case = TRUE) # 3
    output = gsub("\\<IV\\>", "4", output, ignore.case = TRUE) # 4
    return(output)
  }

  # Original box office title.
  boxOfficeTitles <- boxOffice$title

  metadata <- stream_in(file("./data/meta_Movies_and_TV_strict.json"))
  metadata <- metadata[c("asin","description","title","price","categories")]
  # Remove categories that are only TV
  noTvMetadata <- metadata[!(metadata$categories %in% metadata$categories[18]),]

  # Processed matadata movie titles for matching:
  # Latin numbers handled, no punctuation, all lower cases.
  processedMetadataTitles <- noTvMetadata$title
  processedMetadataTitles <- latinNumberToNumberFn(processedMetadataTitles)
  processedMetadataTitles <- removePunctuation(processedMetadataTitles)
  processedMetadataTitles <- tolower(processedMetadataTitles)

  # Processed box office titles for matching:
  # No leading "the", Latin numbers handled, no punctuation, all lower cases.
  processedBoxOfficeTitles <- boxOffice$title
  processedBoxOfficeTitles <- sapply(processedBoxOfficeTitles, stripLeadingWordFn, word = "the ")
  processedBoxOfficeTitles <- latinNumberToNumberFn(processedBoxOfficeTitles)
  processedBoxOfficeTitles <- removePunctuation(processedBoxOfficeTitles)
  processedBoxOfficeTitles <- tolower(processedBoxOfficeTitles)

  boxOfficeTitleLengths <- nchar(processedBoxOfficeTitles)

  matchingFn <- function(title) {
    matchTitle <- "N/A"
    matchLength <- -1
    for (boxOfficeIndex in 1:length(boxOfficeTitles)) {
      boxOfficeTitleLength <- boxOfficeTitleLengths[boxOfficeIndex]
      if (matchLength >= boxOfficeTitleLength) {
        next
      }
    
      processedBoxOfficeTitle <- processedBoxOfficeTitles[boxOfficeIndex]
      if (grepl(paste("\\<", processedBoxOfficeTitle, "\\>", sep = ""), title)) {
        matchTitle <- boxOfficeTitles[boxOfficeIndex]
        matchLength <- boxOfficeTitleLength
      }
    }
    return(matchTitle)
  }

  ptm <- proc.time()

  begin <- 1
  end <- 24000
  
  limit <- length(processedMetadataTitles)

  while (begin <= limit) {
    realEnd = min(limit, end)

    results <- lapply(processedMetadataTitles[begin:realEnd], matchingFn)
    realTitles[begin:realEnd] <- results
  
    print(end)
    
    begin <- begin + 1000
    end <- end + 1000
  }

  et <- proc.time() - ptm

  noTvMetadata$movieTitle <- realTitles

  return(noTvMetadata)
}