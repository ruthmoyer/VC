#ISSUE 4 SCRIPTS
#Ruth Moyer
#May 2023

########################################################
#SUMMER SCHOOL ENROLLMENT DATA (email request)

setwd("C:/Users/ramoyer/Documents/Vital City")
library(readxl)
library(lubridate)

summerschool <- as.data.frame(read_xlsx("Summer Enrollment since 2015_20230523.xlsx"))

names(summerschool) <- c("Grade Band", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015")
summerschool <- summerschool[-1,]
summerschooluse <- summerschool[1:3,]
summerschooluse1 <- as.data.frame(summerschooluse)

summerschool2 <- as.data.frame(t(summerschooluse1))

names(summerschool2) <- c("K-8", "9-12", "Citywide K-12")
summerschool2 <- summerschool2[-1,]
summerschool2$`Citywide K-12` <- gsub("[^0-9]", "", summerschool2$`Citywide K-12`)
summerschool2$`Citywide K-12` <- as.numeric(summerschool2$`Citywide K-12`)
summerschool2$`K-8` <- as.numeric(summerschool2$`K-8`)
summerschool2$`9-12` <- as.numeric(summerschool2$`9-12`)

write.csv(summerschool2, "summerschoolflourish.csv")

##############################################################
setwd("C:/Users/ramoyer/Documents/Vital City")
#NY Dept of Vital Statistics 

marriageX <- read.csv("Marriage Stats 20102020.csv")

marriage <- subset(marriageX, Year!="Average" & !(is.na(Year)))
marriage$April[marriage$Year==2020] <- 0

months <- colnames(marriage[,1:13])
for(i in 1:length(months))
{
  marriage[,i] <- as.numeric(gsub(",", "", marriage[,i]))
}

marriage <- subset(marriage, !(is.na(Year)))
marriagetotal <- colSums(marriage[,-1])
marriageaverage <- marriagetotal/11 #calculate average 
write.csv(marriageaverage, "marriage.csv")

########################################################
setwd("C:/Users/ramoyer/Documents/Vital City/Baseball")

library(readxl)
yankees <- read.csv("yankees1.csv", header=TRUE)
mets <- read.csv("mets1.csv", header=TRUE)
yankees <- subset(yankees, ï..Year>=2009 & ï..Year<=2022)
mets <- subset(mets, ï..Year>=2009 & ï..Year<=2022)

yankees1 <- subset(yankees, select=c("ï..Year", "Tm", "W", "Finish", "Attend.G", "Playoffs"))
yankees1$year <- yankees1$ï..Year
yankees1$YankeesWin <- yankees1$W
yankees1$YankeesFinish <- yankees1$Finish
yankees1$YankeesAvgAttend <- yankees1$Attend.G
yankees1$YankeesAvgAttend[yankees1$year==2020] <- 0
yankees2 <- subset(yankees1, select=c(year, Tm, Finish, Attend.G, Playoffs))

##enter numbers from ESPN site 
yankees2$AttendESPN[yankees2$year==2009] <- 45364
yankees2$AttendESPN[yankees2$year==2010] <- 46491 
yankees2$AttendESPN[yankees2$year==2011] <- 45107
yankees2$AttendESPN[yankees2$year==2012] <- 43733
yankees2$AttendESPN[yankees2$year==2013] <- 40488
yankees2$AttendESPN[yankees2$year==2014] <- 42520
yankees2$AttendESPN[yankees2$year==2015] <- 39922
yankees2$AttendESPN[yankees2$year==2016] <- 37819
yankees2$AttendESPN[yankees2$year==2017] <- 39835
yankees2$AttendESPN[yankees2$year==2018] <- 42998
yankees2$AttendESPN[yankees2$year==2019] <- 41827
yankees2$AttendESPN[yankees2$year==2020] <- 0
yankees2$AttendESPN[yankees2$year==2021] <- 24498
yankees2$AttendESPN[yankees2$year==2022] <- 40207 
yankees2$Playoffs1 <- "Playoffs"
yankees2$Playoffs1[yankees2$Playoffs==""] <- "No Playoffs"
yankees2$Playoffs1[yankees2$year==2009] <- "World Series Winner"
yankees3 <- subset(yankees2, select=c(year, Tm, Finish, AttendESPN, Playoffs1))

mets1 <- subset(mets, select=c("ï..Year", "Tm", "W", "Finish", "Attend.G", "Playoffs"))
mets1$year <- mets1$ï..Year
mets1$MetsWin <- mets1$W
mets1$MetsFinish <- mets1$Finish
mets1$MetsAvgAttend <- mets1$Attend.G
mets1$MetsAvgAttend[mets1$year==2020] <- 0
mets2 <- subset(mets1, select=c(year, Tm, Finish, Attend.G, Playoffs))

mets2$AttendESPN[mets2$year==2009] <- 38941
mets2$AttendESPN[mets2$year==2010] <- 32401
mets2$AttendESPN[mets2$year==2011] <- 30108
mets2$AttendESPN[mets2$year==2012] <- 28035
mets2$AttendESPN[mets2$year==2013] <- 26695
mets2$AttendESPN[mets2$year==2014] <- 26860
mets2$AttendESPN[mets2$year==2015] <- 31725
mets2$AttendESPN[mets2$year==2016] <- 34870
mets2$AttendESPN[mets2$year==2017] <- 30757
mets2$AttendESPN[mets2$year==2018] <- 28164
mets2$AttendESPN[mets2$year==2019] <- 30531
mets2$AttendESPN[mets2$year==2020] <- 0
mets2$AttendESPN[mets2$year==2021] <- 20620
mets2$AttendESPN[mets2$year==2022] <- 33308

mets2$Playoffs1 <- "Playoffs"
mets2$Playoffs1[mets2$Playoffs==""] <- "No Playoffs"  
mets2$Playoffs1[mets2$year==2015] <- "World Series Appearance"

mets3 <- subset(mets2, select=c(year, Tm, Finish, AttendESPN, Playoffs1))

baseball <- rbind(yankees3, mets3)
baseball$PlayoffsUse[baseball$Playoffs1=="No Playoffs"] <- 0
baseball$PlayoffsUse[baseball$Playoffs1!="No Playoffs"] <- 1

write.csv(baseball, "baseballuse.csv")
#############################################################
#delete all R code lines for ice cream prices (Federal Reserve)
#delete all R code lines for bicycles/pedestrians 

##########################################################################
#ISSUE 4 DATA STORY VISUALIZATIONS (USED VISUALIZATIONS ONLY)
#RUTH MOYER 
#MAY 2023
##########################################################################
