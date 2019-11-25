arena_airports=read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/Arena_airports.csv")

nba_schedule=read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/NBA_2018_19.csv")

#Import IATA airport codes
airport_codes <- read.csv("C:/Users/Seth/Documents/airport_codes.csv", header = TRUE)
airport_codes <- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/airport_codes.csv", header=T)

library(tidyverse)

nba_schedule_2 <- right_join(arena_airports,nba_schedule, by="Location")

View(nba_schedule_2)

