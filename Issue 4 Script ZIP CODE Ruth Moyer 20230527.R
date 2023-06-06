#######################################################################
#ZIP Code - CHANGE OF ADDRESS
#Ruth Moyer
#Issue 4 - Vital City
#May 2023

setwd("C:/Users/ramoyer/Documents/Vital City/Issue 4 COA")
library(sf)
library(lubridate)
library(readxl)
library(readr)

###################################################################
#ZIP CODE POPULATION 
#https://data.census.gov/
#table?q=B01001+American+Community+Survey&g=040XX00US36$8600000&tid=ACSDT5Y2021.B01001&moe=false
pop <- read_csv("ZIPPOPACS2021.csv", col_names = FALSE)
pop1 <- pop[1:3,]
pop2 <- as.data.frame(t(pop1))
head(pop2)

pop3 <- as.data.frame(pop2)

pop3$Zip <- gsub("ZCTA5 ", "", pop3$V1)

##########################################################################
#USE 2022 only 
coa2022 <- read.csv("Y2022.csv")
coa2022$ZIPCODE <- as.character(gsub("=", "", coa2022$ZIPCODE))

map <- st_read("ZIP_CODE_040114.shp") #zip code from opendata
zipvector <- map$ZIPCODE

coanyc <- subset(coa2022, ZIPCODE %in% zipvector)

coanyc$year <- substr(coanyc$YYYYMM, 1, 4)
coanyc$month <- substr(coanyc$YYYYMM, 5, 6)
coanyc$month1 <-  sub("^0+", "", coanyc$month)

table(coanyc$month, coanyc$month1)

aggregate(TOTAL.TEMP~month, FUN=sum, data=coanyc)
################################################################
#ZILLOW DATA
#https://www.zillow.com/research/data/
#used the smoothed, seasonablly adjusted - all property type (first option)
homevalue <- read.csv("zipcodezillowdata.csv")
homevalue$Zip <- as.character(homevalue$RegionName)
i <- match(coanyc$ZIPCODE, homevalue$Zip)
coanyc$HomePrice <- homevalue$X12.31.2022[i]
coanyc$Region <- homevalue$Metro[i]
coanyc$HomePrice <- round(coanyc$HomePrice)

coanyc$HomePrice1 <- prettyNum(coanyc$HomePrice, big.mark = ",", scientific = FALSE)
coanyc$HomePrice2 <- paste0("$", coanyc$HomePrice1)  
coanyc$HomePrice2[is.na(coanyc$HomePrice2)] <- ""
coanyc$`Above $1.5 Million` <- "No"
coanyc$`Above $1.5 Million`[coanyc$HomePrice>1500000] <- "Yes"

i <- match(coanyc$ZIPCODE, pop3$Zip)
coanyc$Pop <- pop3$V3[i]
coanyc$Pop <- as.numeric(gsub(",", "", coanyc$Pop))
coanyc$`Requests Per 1,000` <- (coanyc$TOTAL.TEMP/coanyc$Pop)*1000

nycZIPmean <- aggregate(`Requests Per 1,000`~month,
                        data=coanyc, FUN=mean)

wealthyZIPmean <- aggregate(`Requests Per 1,000`~month, data=subset(coanyc, 
                                                                    `Above $1.5 Million`=="Yes"), FUN=mean)

i  <- match(nycZIPmean$month, wealthyZIPmean$month)
nycZIPmean$`Rate per 1,000, home value > $1.5 million, ZIPS` <- wealthyZIPmean$`Requests Per 1,000`[i]

nycZIPmean$`Rate per 1,000, all ZIPS` <- nycZIPmean$`Requests Per 1,000`

nycZIPmean$Month <- c("Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

write.csv(nycZIPmean, file="graphzipcoa.csv")
