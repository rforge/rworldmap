`getMap` <-
function(resolution="low",projection="none"){

  if (projection == "none" & resolution == "low") {
    data("wrld_simpl_lessIslands", envir = environment(),package = "rworldmap")
    mapWithData <- get("wrld_simpl_lessIslands")
  }
  #coarsest resolution map
  if (projection == "none" & resolution == "coarse") {
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
