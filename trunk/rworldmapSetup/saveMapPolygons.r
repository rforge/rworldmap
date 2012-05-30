#saveMapPolygons.r
#developed from naturalEarthDataCompare.r

#andy south 23/5/12

#enables updating of rworldmap polygon mapfiles

#to load & compare 3 resolutions data
#from http://www.naturalearthdata.com/downloads/

library(rworldmap)


setwd("c:\\rworldmapdata\\naturalEarthData\\")

inLow <- "ne_110m_admin_0_countries.shp"
inMed <- "ne_50m_admin_0_countries.shp"
inHigh <- "ne_10m_admin_0_countries.shp"

#sPDFlow <- readShapePoly(inLow)      
#sPDFmed <- readShapePoly(inMed)
#sPDFhigh <- readShapePoly(inHigh)
countriesCoarse <- readShapePoly(inLow) 
countriesCoarseLessIslands <- readShapePoly(inLow) 
countriesLow <- readShapePoly(inMed)
countriesHigh <- readShapePoly(inHigh)

#adding ISO3 columns as a copy of ISO_A3
countriesCoarseLessIslands$ISO3 <- countriesCoarseLessIslands$ISO_A3
countriesCoarse$ISO3 <- countriesCoarse$ISO_A3      
countriesLow$ISO3 <- countriesLow$ISO_A3
countriesHigh$ISO3 <- countriesHigh$ISO_A3

tcoarse <- countriesCoarse@data
tlow <- countriesLow@data
thigh <- countriesHigh@data

#Bouvet seems not to be in data
#grep("Bouvet", sPDFhigh@data$ADMIN, ignore.case="TRUE")

nrow(tcoarse) #177
nrow(tlow) #242
nrow(thigh)#253

#testing which countries are in low but not in coarse
#some don't have an ISO_A3 so get lost
matchPosns <- match(countriesLow@data$ISO_A3, countriesCoarse@data$ISO_A3)
countriesLow@data$ADMIN[which(is.na(matchPosns))]
#mostly islands
#I wonder if I could add these on to the coarse map
#or even add the high ones
#matchPosns2 <- match(countriesHigh@data$ISO_A3, countriesLow@data$ISO_A3)
#matchPosns2 <- match(countriesHigh@data$ADMIN, countriesLow@data$ADMIN)
#countriesHigh@data$ADMIN[which(is.na(matchPosns2))]
#

#to add the extra low countries into the coarse map
matchPosns <- match(countriesLow@data$ADMIN, countriesCoarse@data$ADMIN)
extraPosns <- which(is.na(matchPosns))
countriesLow[extraPosns,]@data$ADMIN #shows the extra countries
#tst <- rbind(countriesCoarse,countriesLow[extraPosns,])
#gives invalid class "SpatialPolygons" object: non-unique Polygons ID slot values
#I could set Polygon IDs to ADMIN or ADMIN_A3
countriesLow <- spChFIDs(countriesLow, as.character(countriesLow$ADMIN))
countriesCoarse <- spChFIDs(countriesCoarse, as.character(countriesCoarse$ADMIN))
countriesCoarse <- rbind(countriesCoarse,countriesLow[extraPosns,])
#to add Tuvalu from the high map
#but problem that high map has 2 extra columns ne_10m_adm & OID_
countriesHigh <- spChFIDs(countriesHigh, as.character(countriesHigh$ADMIN))
#countriesCoarse <- rbind(countriesCoarse,countriesHigh[which(countriesHigh$ADMIN=="Tuvalu"),])
#how can I select out those extra columns
#this selects the columns I don't want !
#countriesHigh[which(countriesHigh$ADMIN=="Tuvalu"),][c("ne_10m_adm","OID_")]@data
badRows <- which(names(countriesHigh)=="ne_10m_adm" | names(countriesHigh)=="OID_")
tuvalu <- countriesHigh[which(countriesHigh$ADMIN=="Tuvalu"),][-badRows]
countriesCoarse <- rbind(countriesCoarse,tuvalu)
countriesLow <- rbind(countriesLow,tuvalu)

#now both low & course should have 243 countries (those in low + Tuvalu)

#testing some joining
#sPDF <- joinCountryData2Map(tlow,joinCode='NAME',nameJoinColumn='NAME')

#checking fixing problems in polygon geometry
countriesCoarseLessIslands@polygons=lapply(countriesCoarseLessIslands@polygons, checkPolygonsHoles)
countriesCoarse@polygons=lapply(countriesCoarse@polygons, checkPolygonsHoles)
countriesLow@polygons=lapply(countriesLow@polygons, checkPolygonsHoles)
#countriesHigh@polygons=lapply(countriesHigh@polygons, checkPolygonsHoles)

#join each map to the regions file #can't use joinCOuntryData2Map because the map data isn't in there yet !
data(countryRegions)
#matchPosns <- match(countriesCoarse@data$ISO3, countryRegions$ISO3)
#countriesCoarse@data <- cbind(countriesCoarse@data, countryRegions[matchPosns,])
countriesCoarse@data <- merge(countriesCoarse@data, countryRegions, by='ISO3', all.x=TRUE )
countriesCoarseLessIslands@data <- merge(countriesCoarseLessIslands@data, countryRegions, by='ISO3', all.x=TRUE )
countriesLow@data <- merge(countriesLow@data, countryRegions, by='ISO3', all.x=TRUE )

save(countriesCoarseLessIslands, file="C://rworldmapWorkingCopy//rworldmap//data//countriesCoarseLessIslands.rda")
save(countriesCoarse, file="C://rworldmapWorkingCopy//rworldmap//data//countriesCoarse.rda")
save(countriesLow, file="C://rworldmapWorkingCopy//rworldmap//data//countriesLow.rda")
#i think this one is too big
#save(countriesMed, file="C://rworldmapWorkingCopy//rworldmap//data//countriesMed.rda")

#create the documentation files - may need editing
#DONT run these after having edited the files
#prompt(countriesCoarseLessIslands, file="c://rworldmapWorkingCopy//rworldmap//man//countriesCoarseLessIslands.Rd")
#prompt(countriesCoarse, file="c://rworldmapWorkingCopy//rworldmap//man//countriesCoarse.Rd")
#prompt(countriesLow, file="c://rworldmapWorkingCopy//rworldmap//man//countriesLow.Rd")
