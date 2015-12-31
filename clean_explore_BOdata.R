# Exploratory data analysis on box office data
library(lubridate); library(ggplot2); library(dplyr)

# Loading data
boxOffice <- read.csv("boxoffice_processed.csv", header=FALSE, col.names = c("movie", "studio", "days", "gross", "date"))
# Removing unneeded columns
boxOffice <- select(boxOffice, -(studio:days))
# Converting date to class date
boxOffice$date <- as.Date(boxOffice$date, format="%m/%d/%Y")

# Filter data after May 1996, add "year" column with only year of release date,
# add "logGross" column with log base 10 of box office gross, as the highest grossing movies are far too much more than the lowest
# add "buckets" column to divide logGross into 4 buckets for comparison
boxOffice <- boxOffice %>% filter(date > as.Date("1996-5-1")) %>%
  mutate(year = year(date), logGross = log10(gross)) %>%
  mutate(buckets = cut(logGross, breaks = c(1,3,5,7,9)))

# Calculate the frequency of each grossing bucket, and calculate a percent out of that year's total frequency of all buckets
boxFreq <- boxOffice %>% group_by(year, buckets) %>% summarize(n=n()) %>% mutate(freq=n/sum(n)*100)

gg <- ggplot(boxFreq, aes(x=year, y=freq, color=buckets))
gg + geom_line(lwd=2) + labs(title="Percentage of log of box office over year", x="Year", y="Percentage")
