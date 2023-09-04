# Importing important libraries for reading , transforming and visualizing data
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)


# Importing the dataset and storing in a variable
IMDB_Movies <- read.csv(file.choose())
View(IMDB_Movies)



#Getting to know our dataset / Dataset description 
glimpse(IMDB_Movies)
head(IMDB_Movies)
skimr::skim_without_charts(IMDB_Movies)



# Relationship between movie ratings and box office gross Earnings
rating_vs_gross <- IMDB_Movies %>% 
  select(IMDB_RATING, GROSS)

ggplot(data=rating_vs_gross) + 
geom_point(mapping = aes(x = IMDB_RATING , y = GROSS, color = "classic")) +
ggtitle("THE CHART SHOWING THE CORRELATION BETWEEN GROSS REVENUE (IN MILLION DOLARS) AND IMDB_RATING") 
cor.test(IMDB_Movies$IMDB_RATING, IMDB_Movies$GROSS)



#Impact of Votes and Box office Gross Earnings:
votes_gross <- IMDB_Movies %>% 
  select(NO_OF_VOTES, GROSS)

ggplot(data=votes_gross) + 
  geom_point(mapping = aes(x = NO_OF_VOTES , y = GROSS, color = "classic")) +
  ggtitle("THE CHART SHOWING THE RELATIONSHIP BETWEEN GROSS REVENUE (IN MILLION DOLARS) AND NUMBER OF VOTES") 
cor.test(IMDB_Movies$NO_OF_VOTES, IMDB_Movies$GROSS)




# The effect of time on box office gross & computing correlation test
time_vs_gross <- IMDB_Movies %>% 
  select(RELEASED_YEAR, GROSS)

ggplot(data = IMDB_Movies, aes(x = RELEASED_YEAR, y = GROSS)) + 
geom_line() +
ggtitle("THE CHART SHOWING THE LINE GRAPH OF GROSS REVENUE (IN MILLION DOLARS) AGAINST RELEASED YEAR")
cor.test(IMDB_Movies$GROSS, IMDB_Movies$RELEASED_YEAR)



#examining a relationship between IMDB Rating and Votes
rating_votes <- IMDB_Movies %>%  
  select(IMDB_RATING, NO_OF_VOTES)

ggplot(data=rating_votes) + 
  geom_point(mapping = aes(x = IMDB_RATING , y = NO_OF_VOTES, color = "classic")) +
  ggtitle("THE CHART SHOWING THE RELATIONSHIP BETWEEN NO_OF_VOTES AND IMDB_RATING") 
cor.test(IMDB_Movies$IMDB_RATING, IMDB_Movies$NO_OF_VOTES)



#Directors who made at least half a billion dollar in a dataset and number movies they have directed
directors <- IMDB_Movies %>% 
  select(DIRECTOR, GROSS) %>%
  filter(GROSS >= 500000000)
  
ggplot(data = directors) +
  geom_bar(mapping = aes(y = GROSS , x = DIRECTOR, fill = DIRECTOR), stat = "identity") +
  ggtitle("THE CHART SHOWING THE DIRECTORS WHO MADE HALF A BILLION DOLLAR GROSS REVENUE") +
  labs(x = "DIRECTORS", y = "GROSS REVENUES")

ggplot(data = directors) +
  geom_bar(mapping = aes(x = DIRECTOR, fill = DIRECTOR), stat = "count") +
  ggtitle("THE CHART SHOWING THE DIRECTORS WHO MADE HALF A BILLION DOLLAR GROSS REVENUE") +
  labs(x = "DIRECTORS", y = "GROSS REVENUES")







