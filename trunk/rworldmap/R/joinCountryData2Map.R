`joinCountryData2Map` <-
function( dF
                          , joinCode = "ISO3" #options "ISO2","ISO3","FIPS","NAME","UN"
                          , nameJoinColumn = "ISO3V10"
                          , nameCountryColumn = "Country"
                          , suggestForFailedCodes = FALSE 
                          , projection="EqualArea"  #options "none", "EqualArea"
                          , mapResolution="low" #options low, medium, only for projection='none' initially
                          , verbose = TRUE #if set to FALSE it doesn't print progress messages to console                         
                          )
   {
    #so that it can get at map data
    #3/2/09 adding option for low resolution map data, initially just for projection='none'
    #will need to decide on default options
    #! need to add error trap
    if (projection == "none" & mapResolution == 'low') 
        { data(wrld_simpl_lessIslands)
          mapWithData <- wrld_simpl_lessIslands } else
    if (projection == "none") 
        { data(wrld_simpl)
          mapWithData <- wrld_simpl } else              
    if (projection == "EqualArea" || projection=="equalArea" ) 
        { data(wrld_simpl_Mollweide)
          mapWithData <- wrld_simpl_Mollweide } 

    #test whether user joinCode is one of permitted
    listJoinCodes <- c("ISO2","ISO3","FIPS","NAME","UN") 
    if (joinCode %in% listJoinCodes == F)
       {
        warning("your joinCode (",joinCode,") in joinCountryData2Map() is not one of those supported. Options are :",listJoinCodes,"\n")
        return(F)
       }

    #dF2 <- merge.data.frame(dF, dFlookupCodes, by=nameJoinColumn)
    
    #3/7/08 Roger Bivand suggested using match rather than merge
    #to enable greater reporting of success & failure
    #& because merge is flakey, and match is fast
    
    #match returns a vector of the positions of (first) matches of its first argument in its second. 
    #so perhaps I would also want to check that codes aren't repeated
    #!also want to find a way of coping with Namibia, the code NA gets interpreted as no data
    #for user codes that don't match I could prompt user for ones that are similar
       
    
    #copy the users nameJoinColumn to a new column named the same as the column in the map for the join code
    #e.g if user has ISO3166_3 it will be copied to ISO3
    dF[[joinCode]] <- dF[[nameJoinColumn]]
    
    
    matchPosnsInLookup <- match(as.character(dF[[joinCode]])
                              , as.character(mapWithData@data[[joinCode]]))


    #count the NAs to find user countries that have failed to match
    failedCodes <- dF[[joinCode]][is.na(matchPosnsInLookup)]
    numFailedCodes <- length(failedCodes) 
    
    #count num successful matches
    numMatchedCountries <- nrow(dF) - numFailedCodes
    #printing info to console    
    cat(numMatchedCountries,"codes from your data successfully matched countries in the map\n")
           

    #failedCountries : relies on their being a column in user data called country
    #!could offer an option for the user to set the name of a country column in the function call
    #nameCountryColumn <- "Country"
    failedCountries <- dF[[nameCountryColumn]][is.na(matchPosnsInLookup)]
    failedCountries <- cbind(failedCodes,failedCountries)
    
    #printing info to console    
    cat(numFailedCodes,"codes from your data failed to match with a country code in the map\n")
    if (verbose) print(failedCountries)
    
 #     failedCodes failedCountries                 
 #[1,] "CIV"       "Ivory Coast"                   
 #[2,] "COD"       "Congo, Democratic Republic"    
                       
    #!could create an optional loop here to go through the failed codes 
    #& prompt the user for a choice fro a suggested list 
    if ( suggestForFailedCodes )
       {
        for( i in 1 : numFailedCodes)
           {
            #search for similar codes/countried & ask user to choose one
            
           }
       }
    #can also get at countries in the lookup that don't appear in user data, by reversing match arguments
    matchPosnsInUserData <- match(as.character(mapWithData@data[[joinCode]])
                                , as.character(dF[[joinCode]])) 
                                
    #these are the codes in lookup that aren't found in user data
    codesMissingFromUserData <- as.character( mapWithData@data[[joinCode]][is.na(matchPosnsInUserData)] )                            
    countriesMissingFromUserData <- as.character( mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)] )
    #  
    numMissingCodes <- length(codesMissingFromUserData) 
    
    #printing info to console
    cat(numMissingCodes,"codes from the map weren't represented in your data\n")
    if (verbose) #if (verbose) print more messages to console
       {
        if (nameJoinColumn!="NAME")                             
        {   print(cbind(codesMissingFromUserData,countriesMissingFromUserData))
        }else #if joined on NAME don't want to print names twice 
            print(codesMissingFromUserData)
       } # 


    ###############################################################
    #merging lookup table onto user data for those codes that match
    #dF2 <- cbind(dFlookupCodes[matchPosnsInLookup,],dF)
    
    #this is the other way around to before, i.e. joining data onto map
    
    mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData,])


    #test colouring map by region & subregion seems to show order has been retained
    #plot(mapWithData,col=mapWithData@data$REGION)
    #plot(mapWithData,col=mapWithData@data$SUBREGION)


    return(mapWithData)
   
   } #end of join2countryCodesAndCentroids

