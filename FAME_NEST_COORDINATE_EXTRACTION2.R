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
# LOAD WORKSPACE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load("S:\\ConSci\\DptShare\\SteffenOppel\\RSPB\\Marine\\IBA\\FAME\\FAME_INPUT_DATA.RData")


### YOU NEED TO MANUALLY DOWNLOAD THE FILE https://www.dropbox.com/s/eozki6x202v7mld/FAME_INPUT_DATA.RData?dl=0

load("FAME_INPUT_DATA.RData")


head(TRACKDATA)
head(COLONY)




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

