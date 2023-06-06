
############################################################################################
#SHOOTINGS 
library(stringr)
library(DBI)
library(dplyr)
library(dbplyr)
library(odbc)
library(lubridate)
library(sf)

myconn <- DBI::dbConnect(odbc::odbc(),
                         "DataHub", 
                         uid="RMOYER", 
                         pwd='VitalCity123!')

a <- DBI::dbGetQuery(myconn,"SELECT * FROM PRD.CLEAN.OPENDATA_SHOOTINGS_YTD")

a$date <- ymd_hms(a$OCCUR_TS)
a$date1 <-  ymd(strftime(a$date,format="%Y-%m-%d"))
a$date2 <- strftime(a$date1, format="%Y-%m")
a$year <- year(a$date1)
a$Month <- month(a$date1)

recentshootings <- subset(a, year %in% c("2022"))
recentshootings1 <- subset(recentshootings, select=c(date1, date2, year, Month, LAT_LON, STATISTICAL_MURDER_FLAG))

b <- DBI::dbGetQuery(myconn,"SELECT * FROM PRD.CLEAN.OPENDATA_SHOOTINGS_HISTORIC")

b$date <- ymd_hms(b$OCCUR_TS)
b$date1 <-  ymd(strftime(b$date,format="%Y-%m-%d"))
b$date2 <- strftime(b$date1, format="%Y-%m")
b$year <- year(b$date1)
b$Month <- month(b$date1)
historicshootings <- subset(b, year <=2021 & year>=2013)
historicshootings1 <- subset(historicshootings,  select=c(date2, date1, year, Month, LAT_LON, STATISTICAL_MURDER_FLAG))

shootinguse <- rbind(historicshootings1, recentshootings1)
shootinguse$count <- 1

shootinguse$Fatal <- 0
shootinguse$Fatal[shootinguse$STATISTICAL_MURDER_FLAG %in% c("Y", "true")] <- 1

head(shootinguse)

shootinguse <- subset(shootinguse, !(LAT_LON==""))

pattern <- "(\\[.*?\\])"
shootinguse$matches <- gregexpr(pattern, shootinguse$LAT_LON)
shootinguse$overlap <- regmatches(shootinguse$LAT_LON, shootinguse$matches)
shootinguse$overlap1 <- gsub("\n", "", shootinguse$overlap)
shootinguse$overlap1 <- gsub("[", "", shootinguse$overlap1, fixed = TRUE) # "Fixed = TRUE" disables regex
shootinguse$overlap1 <- gsub("]", "", shootinguse$overlap1, fixed = TRUE)

x <- strsplit(as.character(shootinguse$overlap1), ",")
x1 <- matrix(unlist(x),ncol=2,byrow=TRUE)
x2 <- as.data.frame(x1)

shootinguse$longitude <- as.numeric(x2$V1)
shootinguse$latitude <- as.numeric(x2$V2)

setwd("C:/Users/ramoyer/Documents/Vital City")
map <- st_read("geo_export_f64441c2-bad6-4b92-bc0e-42c64e3c4c5a.shp") #NTA shapefile
head(map)

nyc1 <- st_transform(map, crs= 2263) #  2263

shootinguse$ID <- paste0("X", 1:nrow(shootinguse))
shootingsuse1 <- subset(shootinguse, select=c(latitude, longitude, ID, year, Month, count))

shootings2022 <- subset(shootingsuse1, year==2022)

sum(shootings2022$count, na.rm=T) #1707

shootingpoints <- st_as_sf(shootings2022, 
                           coords = c("longitude", "latitude"),
                           crs = "+proj=longlat +ellps=WGS84 +no_defs")

shootingpoints1 <- st_transform(shootingpoints, crs= st_crs(nyc1)) # back to lat/lon
shootingpoints1$coordinate <- st_coordinates(shootingpoints1)
shootingpoints1$longitude <- shootingpoints1$coordinate[,2]
shootingpoints1$latitude <- shootingpoints1$coordinate[,1]
#coordinate.Y - latitude
#coordinate.X - longitude

#merge 
linkedpoints <- st_join(shootingpoints1, nyc1) 
linkedpoints$count <- 1

linkedpoints$Category <- "All 2022"  
linkedpoints$Category[linkedpoints$Month %in% c(7,8)] <- "July/August 2022"

all2022 <- aggregate(count~ntacode + ntaname + Category, FUN=sum, data=subset(linkedpoints, Category=="All 2022"))
summer2022 <- aggregate(count~ntacode + ntaname + Category, FUN=sum, data=subset(linkedpoints, Category=="July/August 2022"))

mapdata <- unique(summer2022) #105

write.csv(mapdata, "shootingmapdata2022.csv")

#for point overlay 
summer2022points <- subset(linkedpoints, Category=="July/August 2022")
summer2022pointsX <- as.data.frame(subset(summer2022points, 
                                          select= c(ID, count, longitude, latitude, ntacode, ntaname)))
i <- match(summer2022pointsX$ID, shootingpoints1$ID)
table(is.na(i))
summer2022pointsX$latitude <- as.character(shootingpoints1$latitude[i])
summer2022pointsX$longitude <- as.character(shootingpoints1$longitude[i])
nrow(summer2022pointsX) #381
write.csv(summer2022pointsX, "overlayshootings.csv")

############################################################################################
#SHOOTING TIME SERIES - GRANULAR

head(shootinguse)

#date2 (Y-m); date1 (Y-m-d)

agtotalshooting <- aggregate(count~date2, data=shootinguse, FUN=sum)
agshootingfatal <- aggregate(count~date2, data=subset(shootinguse, Fatal==1), FUN=sum)
agshootingnonfatal <- aggregate(count~date2, data=subset(shootinguse, Fatal==0), FUN=sum)

frame <- expand.grid(Date=unique(shootinguse$date2), FatalCount=NA, NonFatalCount=NA, TotalCount=NA)

i <- match(frame$Date, agshootingfatal$date2)
frame$FatalCount <- agshootingfatal$count[i]

i <- match(frame$Date, agshootingnonfatal$date2)
frame$NonFatalCount <- agshootingnonfatal$count[i]

i <- match(frame$Date, agtotalshooting$date2)
frame$TotalCount <- agtotalshooting$count[i]

frame$Date <- ym(frame$Date)
frame$Month <- month(frame$Date, label=T)

write.csv(frame, "shootinguselong.csv")

################################################################
#CONDENSED BAR GRAPH 

sum(frame$TotalCount[frame$Month=="Jun"])/10
sum(frame$TotalCount[frame$Month=="Oct"])/10

fatalwhole <- aggregate(FatalCount~Month, FUN=mean, data=frame)
nonfatalwhole <- aggregate(NonFatalCount~Month, FUN=mean, data=frame)
shootingtable <- aggregate(TotalCount~Month, FUN=mean, data=frame)

i <- match(shootingtable$Month, fatalwhole$Month)
shootingtable$Fatal <- fatalwhole$FatalCount[i]

i <- match(shootingtable$Month, nonfatalwhole$Month)
shootingtable$Nonfatal <- nonfatalwhole$NonFatalCount[i]

write.csv(shootingtable, "shootingtable.csv")
