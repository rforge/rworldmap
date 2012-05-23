#

`getMap` <-

#function(resolution="low",projection="none"){
function(resolution="low"){
  
  #23/5/12 3 new resolutions
  #scrapped projections
  


  #coarsest resolution map
 if (resolution == "coarse") {
    #data("wrld_coarse", envir = environment(),package = "rworldmap")
    #mapWithData <- get("wrld_coarse")
    data("countriesCoarse", envir = environment(),package = "rworldmap")
    mapWithData <- get("countriesCoarse")    
  }  else if (resolution == "low") {
    #data("wrld_simpl", envir = environment(), package = "rworldmap")
    #mapWithData <- get("wrld_simpl")
    data("countriesLow", envir = environment(),package = "rworldmap")
    mapWithData <- get("countriesLow")    
  }
#  else if (projection == "EqualArea" || projection == "equalArea") {
#    data("wrld_simpl_Mollweide", envir = environment(), package = "rworldmap")
#    mapWithData <- get("wrld_simpl_Mollweide")
#  }
 
  #trying eez map : does work - temporarily removed 31/8 to get permission
  #if (resolution == "eez") {
  #  data("eezMap", envir = environment(), package = "rworldmap")
  #  mapWithData <- get("eezMap")
  #} 
  #else
  
  return(mapWithData)
}
