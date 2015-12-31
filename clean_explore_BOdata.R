# Data Cleaning & Exploratory data analysis on box office data
library(lubridate); library(ggplot2); library(dplyr)

# Loading data
boxOffice <- read.csv("data/boxoffice.csv", header=FALSE, col.names = c("movie", "studio", "days", "gross", "date"),
                      stringsAsFactors = FALSE)
# Removing unneeded columns
boxOffice <- select(boxOffice, -(studio:days))
# Converting date to class date
boxOffice$date <- as.Date(boxOffice$date, format="%m/%d/%Y")
# Filter data after May 1996, add "year" column with only year of release date
boxOffice <- boxOffice %>% filter(date > as.Date("1996-5-1")) %>%
  mutate(year = year(date))


#--------------------Adjusting for inflation
# Calculating cumulative inflation rate since 1995
inflation <- read.csv("data/inflation.csv")

inflation$multiply <- inflation$Rate/100 + 1
inflation$cume[1] <- inflation$multiply[1]
for (i in 2:dim(inflation)[1]){
  inflation$cume[i] <- inflation$multiply[i]*inflation$cume[i-1]
}
inflation$cume <- round(inflation$cume, 2)
inflation$multiply <- NULL

# Calculate all boxoffice values in 1995 equivalent. ie. divide by cumulative rate
for (i in 1:dim(boxOffice)[1]){
  yr <- boxOffice$year[i]
  rate <- filter(inflation, Year == yr)$cume
  boxOffice$adjGross[i] <- round(boxOffice$gross[i] / rate, 1)
}
#--------------------

# Add "logGross" column with log base 10 of box office gross, as the highest grossing movies are far too much more than the lowest
# add "buckets" column to divide logGross into 4 buckets for comparison
boxOffice <- boxOffice %>% mutate(logGross = log10(adjGross)) %>%
  mutate(buckets = cut(logGross, breaks = c(1,3,5,7,9)))

# Calculate the frequency of each grossing bucket, and calculate a percent out of that year's total frequency of all buckets
boxFreq <- boxOffice %>% group_by(year, buckets) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)*100)

gg <- ggplot(boxFreq, aes(x=year, y=freq, color=buckets))
gg + geom_line(lwd=2) + labs(title="Percentage of log of box office over year", x="Year", y="Percentage")
