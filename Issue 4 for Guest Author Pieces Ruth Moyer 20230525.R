################################################
#Ruth Moyer
#May 2023
#ISSUE 4 GUEST AUTHORS


setwd("C:/Users/ramoyer/Documents/Vital City/Refuse and Recycling")

trash <- read.csv("DSNY_Monthly_Tonnage_Data.csv", header=TRUE)
#https://data.cityofnewyork.us/City-Government/DSNY-Monthly-Tonnage-Data/ebb7-mvp5
trash$date <- ym(trash$ï..MONTH)
trash$year <- year(trash$date)
trashviz <- subset(trash, year=="2022")
trashviz$Link <- as.numeric(paste0(trashviz$BOROUGH_ID, trashviz$COMMUNITYDISTRICT))

#aggregate
trashviz1 <- aggregate(REFUSETONSCOLLECTED~Link + COMMUNITYDISTRICT + BOROUGH, data=trashviz, FUN=sum)
table(trashviz1$COMMUNITYDISTRICT)

trashviz1$cdnumber <-  sub("^0+", "", trashviz1$COMMUNITYDISTRICT)  
trashviz1$LinkX <- paste0(trashviz1$BOROUGH, trashviz1$cdnumber)

table(trashviz1$cdnumber)

#https://data.cityofnewyork.us/City-Government/New-York-City-Population-By-Community-Districts/xi7c-iiu2
cdpop <- read.csv("New_York_City_Population_By_Community_Districts.csv")
table(cdpop$CD.Number)
cdpop$LinkX <- paste0(cdpop$ï..Borough, cdpop$CD.Number)

i <- match(trashviz1$LinkX, cdpop$LinkX)
trashviz1$`2010 Population` <- cdpop$X2010.Population[i]
#for doublechecking
trashviz1$BOROUGHdoublecheck <- cdpop$ï..Borough[i]
trashviz1$Name <- cdpop$CD.Name[i]

trashviz1$`Population` <- as.numeric(gsub(",", "", trashviz1$`2010 Population`))

trashviz1$`Tons of Refuse Per 1,000 Residents` <- 
(trashviz1$REFUSETONSCOLLECTED/trashviz1$Population) * 1000

write.csv(trashviz1, "trash2022.csv")

#read in type data
trashtype <- read.csv("DSNY_Waste_Characterization__Mainsort (1).csv", header=TRUE)
#Refuse Percent - percentage of a material in the refuse stream. 
trashtype <- subset(trashtype, Location=="Citywide")

trashtype1 <- subset(trashtype, select=c(Material.Group, Aggregate.Percent, Refuse.Percent, MGP.Percent, Paper.Percent, Organic.Percent, DSNY.Diversion.Summary.Category))
agrefuse <- aggregate(Refuse.Percent~DSNY.Diversion.Summary.Category, FUN=sum, data=trashtype)

sum(agrefuse$Refuse.Percent)

###################################################################
setwd("C:/Users/ramoyer/Documents/Vital City/Trees")

treetype <- read.csv("canopy_jurisdiction_landuse_borough.csv", header=TRUE)

treetype$Use <- NA #create condensed usage/owner column 

treetype$Use[treetype$assumed_owner_type=="Assumed PROW"] <- "Assumed Public Right of Way"
treetype$Use[treetype$assumed_owner_type=="FED"] <- "Federal"
treetype$Use[treetype$assumed_owner_type=="Private"] <- "Private"
treetype$Use[treetype$assumed_owner_type=="NYS"] <- "NY State"
treetype$Use[treetype$assumed_owner_type=="City" & 
               treetype$landuse_description=="NYC Parks"] <- "NYC City Parks"
treetype$Use[treetype$assumed_owner_type=="City" &
               treetype$landuse_description!="NYC Parks"] <- "Other NYC"

canopy_by_borough_use <- aggregate(canopy2017_acres~boroname + Use, FUN=sum, data=treetype)
#for each landuse/owner type in given borough, what is total canopy (in acres) there?

canopy_by_borough_use$ID <- paste0(canopy_by_borough_use$boroname, "x", canopy_by_borough_use$Use)

borough_use_acres <- aggregate(acreage~boroname + Use, FUN=sum, data=treetype) 
#total land for each landuse/owner type in given borough
borough_use_acres$ID <- paste0(borough_use_acres$boroname, "x", borough_use_acres$Use)

i <- match(canopy_by_borough_use$ID, borough_use_acres$ID)
canopy_by_borough_use$totalboroughuse <- borough_use_acres$acreage[i]

canopy_by_borough_use$percent_canopy_use_borough <- 
  round(((canopy_by_borough_use$canopy2017_acres/canopy_by_borough_use$totalboroughuse)*100), digits=1)

########################################################
#Regardless of use ........by borough 
allborougharea <- aggregate(acreage~boroname, FUN=sum, data=treetype) #total acreage of a borough 
#total canopy for each borough 
canopyagborough <- aggregate(canopy2017_acres~boroname, data=treetype, FUN=sum)

i <- match(allborougharea$boroname, canopyagborough$boroname)
allborougharea$canopyacreage <- canopyagborough$canopy2017_acres[i]
allborougharea$percentcanopyallborough <- (allborougharea$canopyacreage/allborougharea$acreage)*100

allborougharea$`Percent Canopy` <- allborougharea$percentcanopyallborough
canopy_by_borough_use$`Percent Canopy` <- canopy_by_borough_use$percent_canopy_use_borough

allborougharea$Use <- "All borough"

byuse <- subset(canopy_by_borough_use, select=c(boroname, `Percent Canopy`, Use))
byborough <- subset(allborougharea, select=c(boroname, `Percent Canopy`, Use))

treedata <- rbind(byuse, byborough)

treedata$Borough <- treedata$boroname
write.csv(treedata, "treeboroughacres.csv")


######################################################################
setwd("C:/Users/ramoyer/Documents/Vital City/Heat and Temperature")

daysabove90 <- read.csv("NYCCSC annual days above 90.csv", header=TRUE)
#observed data ends 2012

daysabove90$`Modeled mean` <- daysabove90$Modeled.Mean.
daysabove90$`Modeled mean (projected)`[daysabove90$ï..Year<=2012] <- NA
daysabove90$`Modeled mean (projected)`[daysabove90$ï..Year>2012] <- daysabove90$`Modeled mean`[daysabove90$ï..Year>2012] 
daysabove90$`Modeled mean`[daysabove90$ï..Year>2012] <- NA

write.csv(daysabove90, "NYCCSCabove90flourish.csv")

julytemp <- read.csv("NYCCSC July average temp.csv", header=TRUE)
julytemp$year <- julytemp$ï..Year
julytemp$`Modeled mean` <- julytemp$Modeled.Mean.Â.F
julytemp$`Modeled mean (projected)`[julytemp$ï..Year<=2012] <- NA
julytemp$`Modeled mean (projected)`[julytemp$ï..Year>2012] <- julytemp$`Modeled mean`[julytemp$ï..Year>2012] 
julytemp$`Modeled mean`[julytemp$ï..Year>2012] <- NA

julytempflourish <- subset(julytemp, select=c(year, `Modeled mean`, `Modeled mean (projected)`))
write.csv(julytempflourish, "julytempNYCCSCflourish.csv")


