#ISSUE 4 DATA STORY VISUALIZATIONS (USED VISUALIZATIONS ONLY)
#RUTH MOYER 
#MAY 2023
##########################################################################

library(lubridate)
#HISTORIC TEMPS
august <- read.csv("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00094728/tavg/1/8/1895-2023.csv")
august$Month <- "August"
august$Year <- substr(august$New.York..Central.Park., 1, 4) 
august$Average.Temperature <- august$New.York
august1 <- subset(august, !(Average.Temperature %in% c("", "Value")))
august2 <- subset(august1, select= -c(August))
august2$`August Temperature` <- as.numeric(august2$Average.Temperature)

july <- read.csv("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00094728/tavg/1/7/1895-2023.csv")
july$Month <- "July"
july$Year <- substr(july$New.York..Central.Park., 1, 4)
july$Average.Temperature <- july$New.York
july1 <- subset(july, !(Average.Temperature %in% c("", "Value")))
july2 <- subset(july1, select= -c(July))
july2$`July Temperature` <- as.numeric(july2$Average.Temperature)


june <- read.csv("https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/city/time-series/USW00094728/tavg/1/6/1895-2023.csv?base_prd=true&begbaseyear=1991&endbaseyear=2020")
june$Month <- "June"
june$Year <- substr(june$New.York..Central.Park., 1, 4)
june$Average.Temperature <- june$New.York
june1 <- subset(june, !(Average.Temperature %in% c("", "Value")))
june2 <- subset(june1, select= -c(June))
june2$`June Temperature` <- as.numeric(june2$Average.Temperature)

historictemps <- cbind(june2, july2, august2)

historictemps$`Summer Mean` <- (historictemps$`July Temperature` + historictemps$`August Temperature`)/2

mean(subset(historictemps, Year>=1895 & Year<=1930)$`Summer Mean`, na.rm=T)
mean(subset(historictemps, Year>1931 & Year<=1960)$`Summer Mean`, na.rm=T)
mean(subset(historictemps, Year>1961 & Year <=1990)$`Summer Mean`, na.rm=T)
mean(subset(historictemps, Year>1990)$`Summer Mean`, na.rm=T)

write.csv(historictemps, "historictemps.csv")
#############################################################################
#DAYS HOTTER THAN 90 DEGREES F 
setwd("C:/Users/ramoyer/Documents/Vital City/Heat and Temperature")

#https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month - source
morethan90 <- read.csv("daysmaxthan90.csv", header=TRUE)
is(morethan90$DATE)

morethan90$date <- ym(morethan90$DATE)
morethan90$year <- year(morethan90$date)

morethan90 <- subset(morethan90, year<=2022 & year>=1940)
#DX90 - https://www.ncei.noaa.gov/access/search/data-search/global-summary-of-the-month?dataTypes=DX90&bbox=40.965,-74.257,40.465,-73.757&pageNum=1&startDate=1972-01-01T00:00:00&endDate=2022-12-31T23:59:59
dayshotterthan90 <- as.data.frame(aggregate(DX90~year, FUN=sum, data=morethan90))

write.csv(dayshotterthan90, file="dayshotterthan90.csv") #file for import into Flourish

#####################################################################################
#HEAT DATA - stress deaths, HVI, and surface temperature 

setwd("C:/Users/ramoyer/Documents/Vital City/Heat and Temperature")
library(raster)
library(stars)
library(sf)

#https://github.com/NewYorkCityCouncil/heat_map - source of .tif files 
tif <- read_stars("f_deviation_smooth.tif")  #read in .tif 
tifsf <- st_as_sf(tif)

#NTA shapefile from NYC Opendata 
map <- st_read("C:/Users/ramoyer/Documents/Vital City/geo_export_f64441c2-bad6-4b92-bc0e-42c64e3c4c5a.shp")

map1 <- st_transform(map, crs=2263)

rasterpoints <- st_as_sf(tifsf, 
                         coords = c("Longitude", "Latitude"),
                         crs = "+proj=longlat +ellps=WGS84 +no_defs")

tifsftrans1 <- st_transform(rasterpoints, crs= st_crs(map1)) #  

#st_write(tifsftrans1, "heatlandsat.geojson")

linkedframe <- st_join(map1, tifsftrans1)

aglinked <- aggregate(f_deviation_smooth.tif~ ntaname + ntacode, FUN=mean, data=linkedframe)

temptif <- read_stars("f_mean_temp.tif")
tempsf <- st_as_sf(temptif)

temppoints <- st_as_sf(tempsf, 
                       coords = c("Longitude", "Latitude"),
                       crs = "+proj=longlat +ellps=WGS84 +no_defs")

temptrans1 <- st_transform(temppoints, crs= st_crs(map1)) #  

#st_write(tifsftrans1, "heatlandsat.geojson")

linkedframe <- st_join(map1, temptrans1)

templinked <- aggregate(f_mean_temp.tif~ ntaname + ntacode, FUN=mean, data=linkedframe)

i <- match(aglinked$ntacode, templinked$ntacode)
aglinked$temperature <- templinked$f_mean_temp.tif[i]

aglinked$ntaname[aglinked$ntanam=="park-cemetery-etc-Bronx"] <- "Park/Cemetery/Other (Bronx)"
aglinked$ntaname[aglinked$ntanam=="park-cemetery-etc-Manhattan"] <- "Park/Cemetery/Other (Manhattan)"
aglinked$ntaname[aglinked$ntanam=="park-cemetery-etc-Staten Island"] <- "Park/Cemetery/Other (Staten Island)"
aglinked$ntaname[aglinked$ntanam=="park-cemetery-etc-Queens"] <- "Park/Cemetery/Other (Queens)"
aglinked$ntaname[aglinked$ntanam=="park-cemetery-etc-Brooklyn"] <- "Park/Cemetery/Other (Brooklyn)"

#link in heat stress death and HVI data from 2022 report 
#https://nyccas.cityofnewyork.us/nyccas2022/report/1
hvidata <- read.csv("HVIdata20230516.csv", header=TRUE)

i <- match(aglinked$ntacode, hvidata$NTACode)
aglinked$`Heat Stress Deaths` <- hvidata$count_of_H[i]
aglinked$`Heat Vulnerability Index` <- hvidata$HVI_Rank[i]

aglinked$Neighborhood <- aglinked$ntaname
aglinked$`Smoothed Deviation` <- aglinked$f_deviation_smooth.tif

heatvariationdata <- aglinked

write.csv(heatvariationdata, "heatvariationdata.csv")
