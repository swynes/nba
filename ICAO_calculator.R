library(tidyverse)
library(magrittr)

airport_codes<- read.csv("C:/Users/Seth/Documents/airport_codes.csv", header = TRUE)
airport_codes <- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/airport_codes.csv", header=T)


fuel<- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/Fuel_burn_757.csv")
View(fuel)


fuel %>% ggplot(aes(x=km, y=kg_co2))+
  geom_point()+
  geom_smooth(method="lm")

fit = lm(fuel$kg_co2~fuel_757$km)
summary(fit)

#A linear model for fuel use (not ideal) is
# y=12.9419x+7019.63

############################  CONVERT DEGREES TO RADIANS   #########################

deg2rad <- function(deg) return(deg*pi/180) #Change degrees into radians



###########################  FIND DISTANCE FROM COORDINATES  ########################

gcd <- function(long1, lat1, long2, lat2) {
  
  
  # Convert degrees to radians
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1)
  long2 <- deg2rad(long2)
  lat2 <- deg2rad(lat2)
  
  
  # WGS-84 ellipsoid parameters
  a <- 6378137         # length of major axis of the ellipsoid (radius at equator)
  b <- 6356752.314245  # length of minor axis of the ellipsoid (radius at the poles)
  f <- 1/298.257223563 # flattening of the ellipsoid
  
  L <- long2-long1 # difference in longitude
  U1 <- atan((1-f) * tan(lat1)) # reduced latitude
  U2 <- atan((1-f) * tan(lat2)) # reduced latitude
  sinU1 <- sin(U1)
  cosU1 <- cos(U1)
  sinU2 <- sin(U2)
  cosU2 <- cos(U2)
  
  cosSqAlpha <- NULL
  sinSigma <- NULL
  cosSigma <- NULL
  cos2SigmaM <- NULL
  sigma <- NULL
  
  lambda <- L
  lambdaP <- 0
  iterLimit <- 100
  while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    sinLambda <- sin(lambda)
    cosLambda <- cos(lambda)
    sinSigma <- sqrt( (cosU2*sinLambda) * (cosU2*sinLambda) +
                        (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda) )
    if (sinSigma==0) return(0)  # Co-incident points
    cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    sigma <- atan2(sinSigma, cosSigma)
    sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    cosSqAlpha <- 1 - sinAlpha*sinAlpha
    cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    if (is.na(cos2SigmaM)) cos2SigmaM <- 0  # Equatorial line: cosSqAlpha=0
    C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    lambdaP <- lambda
    lambda <- L + (1-C) * f * sinAlpha *
      (sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    iterLimit <- iterLimit - 1
  }
  if (iterLimit==0) return(NA)  # formula failed to converge
  uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
  A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
  B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
  deltaSigma = B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM^2) -
                                             B/6*cos2SigmaM*(-3+4*sinSigma^2)*(-3+4*cos2SigmaM^2)))
  s <- b*A*(sigma-deltaSigma) / 1000
  
  return(s) # Distance in km
}



############################   CALCULATE CO2 FROM DISTANCE   #####################
#Have included the uplift factor recommended by the ICAO here
#For trips below 550km add 50km etc.
#Also added linear formula for fuel burn to CO2 for 557 calculated above

co2calculator <- function(distance) {
  co2e<- if(distance<=550){
    ((distance+50)*12.9419)+7019.63
  }   else if (distance>550 && distance<5500) {
    ((distance+100)*12.9419)+7019.63
  } else if (distance>=5500) {
    ((distance+125)*12.9419)+7019.63
  } 
  return(co2e)
}



co2calculator <- function(distance) {
  co2e<- if(distance<=0){           
    (distance*0)
  }   else if (distance>0 && distance<=231.5) {  #Use higher fuel burn rate for short flights per ICAO fuel chart
    (distance+50)*(6822.44/231.5)
  }   else if (distance>231.5 && distance<=550) {  #Use highest fuel burn rate for this segment of flights, until 550km when uplift factor for distance is applied
    (distance+50)*(14014.6/463)                   #!!Leaves a jump discontinuity though
  }   else if (distance>550 && distance<5500) {
    ((distance+100)*12.9419)+7019.63
  } else if (distance>=5500) {
    ((distance+125)*12.9419)+7019.63
  } 
  return(co2e)
}

co2calculator <- Vectorize(co2calculator, vectorize.args = "distance")


distance<- c(0,10,50,75,100,181.5,182,200,230,233,300,400,413,500,540,550,560,600,700,800,900,1000)
test_df<- data.frame(distance)

test_df %<>% mutate(co2=co2calculator(distance))
View(test_df)


######################   COMBINE ALL FUNCTIONS IN CALCULATOR   #####################


carboncalculator <- function(origin, destination) {
  
  lat1 <- airport_codes %>%
    filter(aircode == origin ) %$% c(lat)
  
  long1 <- airport_codes %>% 
    filter(aircode == origin ) %$% c(long)
  
  lat2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(lat)
  
  long2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(long)
  
  
  co2calculator(
    gcd(
      (long1),
      (lat1),
      (long2),
      (lat2)))
}




###  TEST  ###
carboncalculator("YYZ","YVR")

carboncalculator("YVR","YNC")
carboncalculator("YVR","LGW")
carboncalculator("YVR","HKG")
carboncalculator("LGW","LHR")


co2calculator(3028.83)

co2calculator(6812.92)

co2calculator(10219.38)

#Now use this code to make sure the function can apply to a vector
#https://community.rstudio.com/t/the-condition-has-length-1-and-only-the-first-element-will-be-used/16737/5
co2calculator <- Vectorize(co2calculator, vectorize.args = "distance")

#Must do this before using mutate
carboncalculator <- Vectorize(carboncalculator, vectorize.args=c("origin","destination")) 
carboncalculator <- Vectorize(carboncalculator) #Have tried this and above to fix code error 
gcd <- Vectorize(gcd)
deg2rad <- Vectorize(deg2rad)


############ NEW DISTANCE FUNCTION ####################


dist_calc <- function(origin, destination) {
  
  lat1 <- airport_codes %>%
    filter(aircode == origin ) %$% c(lat)
  
  long1 <- airport_codes %>% 
    filter(aircode == origin ) %$% c(long)
  
  lat2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(lat)
  
  long2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(long)
  
  
  gcd(
    (long1),
    (lat1),
    (long2),
    (lat2))
}

dist_calc <- Vectorize(dist_calc)


############### ALLOWING dist_calc to be okay with blanks ##############

dist_calc <- function(origin, destination) {
  
  
  lat1 <- airport_codes %>%
    filter(aircode == origin ) %$% c(lat)
  
  long1 <- airport_codes %>% 
    filter(aircode == origin ) %$% c(long)
  
  lat2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(lat)
  
  long2 <- airport_codes %>% 
    filter(aircode == destination ) %$% c(long)
  
  ifelse(origin=="NA", 0,
         ifelse(destination=="NA",0,
                gcd(
                  (long1),
                  (lat1),
                  (long2),
                  (lat2))))
}

dist_calc <- Vectorize(dist_calc)



######################  TEST ON TORONTO DF  #####################
Toronto4<- read.csv("C:/Users/AsusW10/Documents/Aviation/NBA/Toronto.csv")
View(Toronto4)

View(Toronto3)
Toronto3<-na.omit(Toronto3)

#Convert to characters to avoid error code: "Error in Ops.factor(aircode, origin) : level sets of factors are different
Toronto4$origin<- as.character(Toronto4$origin)
Toronto4$destination<- as.character(Toronto4$destination)

#Mutate a CO2 column
Toronto4 %<>% mutate(kg_co2 = carboncalculator(origin,destination))

#Mutate a distance column
Toronto4 %<>% mutate(km = dist_calc(origin,destination))


View(Toronto4)