#' Translate paternity output into segments for geom_segments
#' 
#' This is a convienence function that takes the output of a \code{paternity()} 
#'  function and the raw \code{data.frame} with the coordinates and turns it
#'  into a \code{data.frame} that can be used in \code{geom_segments}.
#' @param pat The output from the \code{paterntiy()} function.
#' @param df A \code{data.frame} with columns for the ID of the fathers and 
#'  spatial coordinates.
#' @param ID The name of the column in \code{df} that has the ID of the fathers 
#'  (default="ID")
#' @param OffID The column name in \code{df} that has the offspring id value
#' @param longitude The label for the x-coordinate for the adults as it is found 
#'  in the \code{df} object (default="Longitude")
#' @param latitude The label for the y-coordinate for the adults as it is found
#'  in the \code{df} object (default="Latitude")
#' @return A \code{data.frame} with the segments correctly formated for 
#'  \code{geom_segment}.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
spiderplot_data <- function( pat, df, ID="ID", OffID="OffID", longitude="Longitude", latitude="Latitude"){
  
  if( !is(pat,"data.frame") | !all(names(pat) == c("MomID","OffID","DadID","Fij")))
    stop("The spiderplot_data function needs a data frame from the paternity() function.")
  
  if( !is(df,"data.frame") | !(any(c(ID,longitude,latitude) %in% names(df))))
    stop("You need to pass df that has the ID and spatial coordinate columns.")
  
  ret <- pat
  
  ret$Yend <- ret$Y  <- ret$Xend <- ret$X <- NA
  adults <- df[ df[[OffID]]==0,]
  for( i in 1:nrow(ret)){
    mom <- adults[ adults[[ID]]==ret$MomID[i] ,]
    dad <- adults[ adults[[ID]]==ret$DadID[i] ,]
    ret$X[i] <- mom[[longitude]]
    ret$Y[i] <- mom[[latitude]]
    ret$Xend[i] <- dad[[longitude]]
    ret$Yend[i] <- dad[[latitude]]
  }
  
  
  return(ret)
}