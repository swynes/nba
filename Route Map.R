######################  ROUTE MAP  #######################



getwd()

# Read flight list
flights <- read.csv(("C:/Users/Seth/Documents/nba/flights.csv"), stringsAsFactors = FALSE)

# Lookup coordinates
library(ggmap)
library(tidyverse)

air_codes<- read.csv("C:/Users/Seth/Documents/nba/airport_codes.csv")
View(air_codes)

flights2<- merge(flights, air_codes, by.x="From", by.y="aircode")
flights2<- merge(flights2, air_codes, by.x="To", by.y="aircode")

View(flights2)

# Plot flight routes
library(ggrepel)
library(cowplot)
library(maps)
install.packages("cowplot")

worldmap <- borders("world", colour="#efede1", fill="#efede1") # create a layer of borders
flight_map<- ggplot() + worldmap + 
  geom_curve(data=flights2, aes(x = long.x, y = lat.x, xend = long.y, yend = lat.y), 
             col = "#b29e7d", size = 0.3, curvature = .2, ncp=1000, lineend="round") + 
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

?geom_curve
