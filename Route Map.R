######################  ROUTE MAP  #######################




# Read flight list
flights <- read.csv("flights.csv", stringsAsFactors = FALSE)

# Lookup coordinates
library(ggmap)
airports <- unique(c(flights$From, flights$To))
coords <- geocode(airports)
airports <- data.frame(airport=airports, coords)