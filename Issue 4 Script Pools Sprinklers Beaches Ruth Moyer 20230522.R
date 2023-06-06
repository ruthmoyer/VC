##############################################################
setwd("C:/Users/ramoyer/Documents/Vital City/Pools Beaches Cooling Centers Sprinklers")
library(sf)
library(lubridate)

#RUTH MOYER
#MAY 2023

#https://council.nyc.gov/data/pools/ - doublechecked against
pools <- read.csv("NYC_Parks_Pools.csv", header=TRUE) #from open data
pools1 <- pools #these should all be active (no indicator that closed)

pools1$y <- gsub(pattern = ".*\\((-?[0-9.]+).*", replacement= "\\1", pools1$polygon)
pools1$x <- gsub(pattern = ".*\\s(-?[0-9.]+).*", replacement= "\\1", pools1$polygon)
pools1$Name <- paste0(pools1$NAME, " (", pools1$POOLTYPE, ")")

poolsuse <- subset(pools1, select=c(Name, y, x, LOCATION))

#Sprinklers
sprinklers <- read.csv("Cool_It__NYC_2020_-_Spray_Showers.csv", header=TRUE)
sprinklers <- subset(sprinklers, ï..Status=="Activated")
sprinklers$NAME <- sprinklers$PropertyName
sprinklers$LOCATION <- "Sprinkler"
sprinklers1 <- subset(sprinklers, !(is.na(x)) & !(is.na(y)))
sprinklers1$x <- as.numeric(gsub(",", "", sprinklers1$x))
sprinklers1$y <- as.numeric(gsub(",", "", sprinklers1$y))

#nyc <- subset(map, COUNTYFP %in% c("005", "047", "061", "081", "085"))
sprinklerpoints <- st_as_sf(sprinklers1, 
                            coords = c("x", "y"),
                            crs = 2263)

sprinklerpoints1 <- st_transform(sprinklerpoints, crs= "+proj=longlat +ellps=WGS84 +no_defs") # back to lat/lon
sprinklerpoints1$coordinate <- st_coordinates(sprinklerpoints1)
sprinklerpoints1$y <- sprinklerpoints1$coordinate[,1]
sprinklerpoints1$x <- sprinklerpoints1$coordinate[,2]
sprinklerpoints1$Name <- sprinklerpoints1$NAME
sprinklersuse <- subset(as.data.frame(sprinklerpoints1), select=c("LOCATION", "Name", "x", "y"))

beaches <- read.csv("BeachesDownloaded.csv", header=TRUE)
table(beaches$FEATURESTATUS) #all active
beaches$y <- gsub(pattern = ".*\\((-?[0-9.]+).*", replacement= "\\1", beaches$multipolygon)
beaches$x <- gsub(pattern = ".*\\s(-?[0-9.]+).*", replacement= "\\1", beaches$multipolygon)
beaches$Name <- beaches$NAME
beachesuse <- subset(beaches, select=c(Name, x, y))
beachesuse$LOCATION <- "Beach"

combinedcooling <- rbind(poolsuse, sprinklersuse, beachesuse)
combinedcooling$LOCATION[combinedcooling$LOCATION=="Outdoor"] <- "Outdoor pool"
combinedcooling$LOCATION[combinedcooling$LOCATION=="Indoor"] <- "Indoor pool"

write.csv(combinedcooling, "combinedcooling.csv")

beachesandpoolsonly <- subset(combinedcooling, LOCATION!="Sprinkler")
write.csv(beachesandpoolsonly, "beachesandpoolsonly.csv")

##################################################################
#Swimming Beach attendance
attendance <- read.csv("Swimming_Beach_Attendance.csv", header=TRUE)
attendance$datex <- mdy(attendance$ï..Date)
attendance$Month <- strftime(attendance$datex, format="%Y-%m")
attendance$Year <- year(attendance$datex)
attendance$Attendance <- gsub(",", "", attendance$Attendance)
attendance$Attendance <- as.numeric(attendance$Attendance)
attendance$Beach[attendance$Beach=="South beach"] <- "South Beach"

beachattendanceag <- aggregate(Attendance~Year + Beach, FUN=sum, data=attendance)

library(reshape2)
beachattendance1 <- dcast(beachattendanceag, Year~Beach)

write.csv(beachattendance1, "beachattendance.csv")
