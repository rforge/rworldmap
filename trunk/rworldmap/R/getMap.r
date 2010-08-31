#30/7/2010 maybe add another option to access other maps
#or just allow to be passed in the resolution param ?
#option for map would be better
#it could assume 'worldCountries' by default

`getMap` <-

function(resolution="low",projection="none"){

  #trying eez map : does work - temporarily removed 31/8 to get permission
  #if (resolution == "eez") {
  #  data("eezMap", envir = environment(), package = "rworldmap")
  #  mapWithData <- get("eezMap")
  #} 
  #else 
  if (projection == "none" & resolution == "low") {
    data("wrld_simpl_lessIslands", envir = environment(),package = "rworldmap")
    mapWithData <- get("wrld_simpl_lessIslands")
  }
  #coarsest resolution map
  else if (projection == "none" & resolution == "coarse") {
    data("wrld_coarse", envir = environment(),package = "rworldmap")
    mapWithData <- get("wrld_coarse")
  }  
  else if (projection == "none") {
    data("wrld_simpl", envir = environment(), package = "rworldmap")
    mapWithData <- get("wrld_simpl")
  }
  else if (projection == "EqualArea" || projection == "equalArea") {
    data("wrld_simpl_Mollweide", envir = environment(), package = "rworldmap")
    mapWithData <- get("wrld_simpl_Mollweide")
  }
 
  
  return(mapWithData)
}
