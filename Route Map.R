###################  MAPPING THE NBA SEASON    #####################
library(magrittr)
library(ggmap)
library(tidyverse)

nba_schedule<- read.csv(("C:/Users/Seth/Documents/nba/NBA_2018_19.csv"), stringsAsFactors = FALSE)
nba_schedule<- read.csv(("C:/Users/AsusW10/Documents/Aviation/NBA/NBA_2018_19.csv"), stringsAsFactors = FALSE)

arena_airports<- read.csv("C:/Users/Seth/Documents/nba/Arena_airports.csv")
arena_airports=read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/Arena_airports.csv")

#Import IATA airport codes
air_codes <- read.csv("C:/Users/Seth/Documents/airport_codes.csv", header = TRUE)
air_codes <- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/airport_codes.csv", header=T)


nba_schedule_2 <- right_join(arena_airports,nba_schedule, by="Location")

View(nba_schedule_2)

Toronto <- nba_schedule_2 %>% filter(Home.Team=="Toronto Raptors"|Away.Team=="Toronto Raptors")
View(Toronto)

#Create an origin and destination column
Toronto %<>% mutate(origin=Airport) 
Toronto %<>%mutate(destination=lead(origin)) #use lead/lag to mutate a new column offset by one

View(Toronto)

Toronto$origin <- as.character(Toronto$origin)
Toronto$destination <- as.character(Toronto$destination)

#Replace NA in column with the most frequent value in the column which should be homecourt
#This ensures a flight home at the end of the season
#BUt does not work on my home laptop??!
Toronto$destination[is.na(Toronto$destination)] <- names(which.max(table(Toronto$destination)))

View(Toronto)

#Remove rows where origin is same as destination (no movement)
Toronto2<- Toronto[!duplicated(Toronto[,c('origin', 'destination')]),]

Toronto3<- Toronto2 %>% distinct(origin, destination, .keep_all = TRUE)

#For some reason I'm unable to remove the first row using "distinct"
#Remove the first row using base R
Toronto3<- Toronto3[-1,]
View(Toronto3)
setwd("C:/Users/AsusW10/Documents/Aviation/NBA")

write.csv(Toronto3, "Toronto.csv")
Toronto4<- read.csv("C:/Users/Seth/Documents/nba/Toronto.csv")
Toronto4<- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/Toronto.csv")
View(Toronto4)


typeof(Toronto$origin)
typeof(Toronto$destination)


#######################  NOW ADD MAP FOR SEASON   ############################

library(ggrepel)
library(cowplot)
library(maps)

Toronto_flights<- merge(Toronto3, air_codes, by.x="origin", by.y="aircode")
Toronto_flights2<- merge(Toronto_flights, air_codes, by.x="destination", by.y="aircode")



worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
flight_map<- ggplot() + worldmap + 
  geom_curve(data=Toronto_flights2, aes(x = long.x, y = lat.x, xend = long.y, yend = lat.y), 
             col = "#b29e7d", alpha=0.2, size = 0.3, curvature = .2, ncp=1000, lineend="round") + 
  #geom_point(data=flights2, aes(x = long.x, y = lat.x), col = "#970027") + 
  #geom_text_repel(data=air_codes, aes(x = long, y = lat, label = air_codes), col = "black", size = 2, segment.color = NA) + 
  theme(panel.background = element_rect(fill="white"), 
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

flight_map

save_plot("flight_map.tiff", flight_map, base_aspect_ratio = 1, 
          dpi=600)

##################################################################
###############     TRY TO CLEAN ALL TEAMS AT ONCE   #############
##################################################################

#First, make a df with all the team names

Teams<-unique(nba_schedule_2$Home.Team)

View(Teams)

#Now make a new df for each Team from the schedule
#Use sapply or lapply????


#Create an origin and destination column



Toronto %<>% mutate(origin=Airport) 
Toronto %<>%mutate(destination=lead(origin)) #use lead/lag to mutate a new column offset by one


Toronto$origin <- as.character(Toronto$origin)
Toronto$destination <- as.character(Toronto$destination)

Toronto$destination[is.na(Toronto$destination)] <- names(which.max(table(Toronto$destination))) #Replace NA with most common (home team) so last game of season there is a return trip

#Remove rows where origin is same as destination (no movement)
Toronto2<- Toronto[!duplicated(Toronto[,c('origin', 'destination')]),]

Toronto3<- Toronto2 %>% distinct(origin, destination, .keep_all = TRUE)

#For some reason I'm unable to remove the first row using "distinct"
#Remove the first row using base R
Toronto3<- Toronto3[-1,]
