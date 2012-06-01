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

#setting any POP_EST of -99 to NA
countriesCoarseLessIslands$POP_EST[ which(countriesCoarseLessIslands$POP_EST < 0)] <- NA
countriesCoarse$POP_EST[ which(countriesCoarse$POP_EST < 0)] <- NA
countriesLow$POP_EST[ which(countriesLow$POP_EST < 0)] <- NA
countriesHigh$POP_EST[ which(countriesHigh$POP_EST < 0)] <- NA

#converting curacao that can be a pain later otherwise due to non-ascii
tmp <- countriesLow@data
for(i in seq(tmp))
   {
    levels(tmp[,i])[levels(tmp[,i])=="Cura�ao"] <- "Curacao"
   }
countriesLow@data <- tmp



#Bouvet seems not to be in data
#grep("Bouvet", sPDFhigh@data$ADMIN, ignore.case="TRUE")

# tcoarse <- countriesCoarse@data
# tlow <- countriesLow@data
# thigh <- countriesHigh@data
# nrow(tcoarse) #177
# nrow(tlow) #242
# nrow(thigh)#253

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
#testing that num polygons and num rows of data are the same
if ( length(countriesLow@polygons) != nrow(countriesLow@data) 
  |  length(countriesCoarse@polygons) != nrow(countriesCoarse@data) )
   warning("seems that number of polygons different from num data rows")

#testing some joining
#sPDF <- joinCountryData2Map(tlow,joinCode='NAME',nameJoinColumn='NAME')

#checking fixing problems in polygon geometry
countriesCoarseLessIslands@polygons=lapply(countriesCoarseLessIslands@polygons, checkPolygonsHoles)
countriesCoarse@polygons=lapply(countriesCoarse@polygons, checkPolygonsHoles)
countriesLow@polygons=lapply(countriesLow@polygons, checkPolygonsHoles)
#countriesHigh@polygons=lapply(countriesHigh@polygons, checkPolygonsHoles)

#adding the polygon centroids on, as columns LON and LAT (does work)
countriesCoarseLessIslands$LON <- coordinates(countriesCoarseLessIslands)[,1]
countriesCoarseLessIslands$LAT <- coordinates(countriesCoarseLessIslands)[,2]
countriesCoarse$LON <- coordinates(countriesCoarse)[,1]
countriesCoarse$LAT <- coordinates(countriesCoarse)[,2]
countriesLow$LON <- coordinates(countriesLow)[,1]
countriesLow$LAT <- coordinates(countriesLow)[,2]

#join each map to the regions file #can't use joinCOuntryData2Map because the map data isn't in there yet !
data(countryRegions)

#matchPosns <- match(countriesCoarse@data$ISO3, countryRegions$ISO3)
#countriesCoarse@data <- cbind(countriesCoarse@data, countryRegions[matchPosns,])
#previous problem due to 2 repeated countries in countryRegions
#!!DANGEROUS due to NAs
countriesCoarse@data <- merge(countriesCoarse@data, countryRegions, by='ISO3', all.x=TRUE )
countriesCoarseLessIslands@data <- merge(countriesCoarseLessIslands@data, countryRegions, by='ISO3', all.x=TRUE )
countriesLow@data <- merge(countriesLow@data, countryRegions, by='ISO3', all.x=TRUE )

#how do I deal with non-ascii strings ? they cause warnings in check
#package(tools) showNonASCII() iconv()
#iconv(as.character(countriesCoarse@data),"ASCII","UTF-8")
#but problem that this converts all numeric data to characters
# * checking data for non-ASCII characters ... WARNING
# Warning: found non-ASCII string(s)
# 'Curagao' in object 'countriesCoarse'
# 'Eland Islands' in object 'countriesCoarse'
# 'Fxoroyar (Faroe Is.)' in object 'countriesCoarse'
# 'Ivory Coast (Ctte d'Ivoire)' in object 'countriesCoarse'
#   'Ivory Coast (Ctte d'Ivoire)' in object 'countriesCoarseLessIslands'
#   'Curagao' in object 'countriesLow'
#   'Eland Islands' in object 'countriesLow'
#   'Fxoroyar (Faroe Is.)' in object 'countriesLow'
#   'Ivory Coast (Ctte d'Ivoire)' in object 'countriesLow'
#i had to add an if not numeric bit in
countriesCoarseLessIslands@data <- data.frame(lapply(countriesCoarseLessIslands@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","UTF-8") else x))
countriesCoarse@data <- data.frame(lapply(countriesCoarse@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","UTF-8") else x))
countriesLow@data <- data.frame(lapply(countriesLow@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","UTF-8") else x))
# countriesCoarseLessIslands@data <- data.frame(lapply(countriesCoarseLessIslands@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","ASCII") else x))
# countriesCoarse@data <- data.frame(lapply(countriesCoarse@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","ASCII") else x))
# countriesLow@data <- data.frame(lapply(countriesLow@data, function(x)  if(!is.numeric(x)) iconv(x,"ASCII","ASCII") else x))
#Cura�ao remains a problem it gets converted to Curagao but still causes problem at check




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
