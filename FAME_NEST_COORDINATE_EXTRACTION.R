############################################################################################################
####### IDENTIFYING NEST LOCATIONS FOR FAME TRACKING DATA #######################################
############################################################################################################

### written by Steffen Oppel, based on code by Bethany Clark
### UPDATE 10 April 2017: passed on to Jessica Finan for processing and further development


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD PACKAGES
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(maptools)
require(geosphere)
require(sp)
library(rgdal)
library(raster)
library(plyr)
library(trip)
library(readxl)
library(maps)
library(rgeos)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD RAW DATA AND MODIFY FOR PROCESSING
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Marine\\IBA\\FAME")


### Load FAME data from file

alldat<-read.table("20102015v2.csv", header=T, sep=",")
alldat$DG<-paste(alldat$Species, alldat$Site, sep="_")

### Convert Dates and Times and calculate the time difference (in s) between each location and the first location of that deployment
alldat$Time<-format(paste(alldat$Hour,alldat$Minute, alldat$Second, sep=":"),format="%H:%M:%S")
alldat$Date<-as.Date(paste(alldat$Year,alldat$Month, alldat$Day, sep="-"),format="%Y-%m-%d")
alldat$Loctime<-as.POSIXlt(paste(alldat$Date, alldat$Time), format = "%Y-%m-%d %H:%M:%S")
alldat$DateTime <- as.POSIXct(strptime(alldat$Loctime, "%Y-%m-%d %H:%M:%S"), "GMT")
alldat$TrackTime <- as.double(alldat$DateTime)
alldat$TrackTime <- alldat$TrackTime-min(alldat$TrackTime)
names(alldat)
head(alldat)
TRACKDATA<-alldat[,c(22,4:6,27)]
head(TRACKDATA)

### Load TABLE THAT HAS METADATA FOR EACH SPECIES AND COLONY SUBSET
procdat<-read_excel("FAME_IBAanalysis_input_data_v2.xlsx", "input")

#### CONDENSE INTO COLONIES ######
colonies<-aggregate(n_locations~Species+Site+Col_lat+Col_long+Col_size, procdat, FUN=sum)
colonies$DG<-paste(colonies$Species, colonies$Site, sep="_")
datasets<-unique(colonies$DG)					### this is the list of species * colony * breeding status combinations for which we need to run analysis

### Load TABLE WITH UPDATED LAT AND LONG DATA
newcoords<-read.table("Colony_locs.csv", header=T, sep=',')
colonies$Col_lat<-newcoords$LAT[match(colonies$Site,newcoords$SiteCode)]
colonies$Col_long<-newcoords$LON[match(colonies$Site,newcoords$SiteCode)]
head(colonies)

COLONY<-colonies[,c(7,3,4)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE / RELOAD WORKSPACE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(alldat,colonies,datasets,datetime2011,newcoords,procdat)
save.image("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Marine\\IBA\\FAME\\FAME_INPUT_DATA.RData")
load("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Marine\\IBA\\FAME\\FAME_INPUT_DATA.RData")






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IDENTIFY INDIVIDUAL NEST SITES FOR ALL TRACKED INDIVIDUALS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ALL_NESTS<-data.frame()


for (DG in COLONY$DG){					### start loop over each colony

x<-TRACKDATA[TRACKDATA$DG==DG,]



########################### JESSICA TO DEVELOP THIS FROM HERE ####################################


### code from Bethany Clark

#To locate the nest sites, I first labelled trips using a larger than normal radius (2km for kittiwakes)
#And then used the following code to find nests within that 2km radius, labelled as trip=0
#by rounding the lat & lon and picking the modal value for each individual
#You can then use the nest to relabel the trips afterwards

#Identify the nest site for each bird - Beth Clark 30/1/17

#I did this indivudally for each site, but you could loop through sites (I didn't as I wanted to visually check each one)
site <- "STM"    ### Pick  ###
dat <- read.csv(paste(filepath,"Trips_",site,".csv",sep="")) #I kept a different csv for each site

#Rounds the lat & lon to 4dp, then extracts the modes & id
nestlat <- function(bird){
  x5 <- round(bird$Latitude[bird$trip == 0],  digits = 4)
  uniqx <- unique(x5)
  as.numeric(uniqx[which.max(tabulate(match(x5, uniqx)))])
}
nestlon <- function(bird){
  x5 <- round(bird$Longitude[bird$trip == 0],  digits = 4)
  uniqx <- unique(x5)
  as.numeric(uniqx[which.max(tabulate(match(x5, uniqx)))])
}

ids <- unique(dat$id)
nestlocs <- as.data.frame(ids)
for (i in 1:length(ids)){
  bird <- subset(dat,dat$id == ids[i])
  nestlocs$lat[i] <- nestlat(bird)
  nestlocs$lon[i] <- nestlon(bird)
}

#add to tracking dataframe
dat$nestlat <- nestlocs$lat[match(dat$id, nestlocs$id)]
dat$nestlon <- nestlocs$lon[match(dat$id, nestlocs$id)]


ALL_NESTS<-rbind(ALL_NESTS,...)



} ## close loop over each colony


write.csv(ALL_NESTS,"FAME_nest_locations.csv",row.names=F)

