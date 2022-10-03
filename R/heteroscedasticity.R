#' Heteroscedasticity
#' 
#' This function takes a set of multilocus genotypes and estimates a vector divergence from stratum centroids.  It
#'  can then 
#' @imports dplyr
#'  
#' @examples 
#' library( dplyr)  
#' data(arapat)
#' arapat |> 
#'  filter( Species == "Mainland" ) |> 
#'  select( -Species, -Cluster, -ID, -Latitude, -Longitude ) |>
#'  heteroscedasticity()

heteroscedasticity <- function(data, stratum="Population") { 
}


data( arapat ) 
library( dplyr )

arapat |>
  filter( Species == "Mainland" ) |>
  select( -Species, -Cluster, -ID, -Latitude, -Longitude ) -> data 

mv <- to_mv( data )
  
frequency_matrix( data ) |>
  rename("Population" = "Stratum") |>
  merge( data ) |>
  select( !names(data) ) -> freqs

div <- mv-freqs 


divergence <- apply( div, 1, function(x) { return(sqrt( sum( (x)^2 )) ) } )

df <- data.frame( Stratum = data$Population,
                  Divergence = divergence )
kruskal.test( Divergence ~ Stratum, data=df)





