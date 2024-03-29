#' Heteroscedasticity
#' 
#' This function takes a set of multilocus genotypes and estimates a vector divergence from stratum centroids.  It
#'  can then 
#' @import dplyr
#' @examples 
#' library( dplyr)  
#' data(arapat)
#' arapat |> 
#'  filter( Species == "Mainland" ) |> 
#'  select( -Species, -Cluster, -ID, -Latitude, -Longitude ) |>
#'  heteroscedasticity()


het_data <- function() { 
  data( arapat ) 
  library( dplyr )
  arapat |>
    dplyr::filter( Species == "Mainland" ) |>
    droplevels() |>
    dplyr::select( -Species, -Cluster, -ID, -Latitude, -Longitude ) -> data 
  return( data)
}


heteroscedasticity <- function(x, stratum="Population", N = 999, plot=FALSE, as.princomp = FALSE) { 

  if( missing(x) || !is(x, "data.frame") ) { 
    stop("You must pass a data.frame object to the heteroscedasticity function.")
  }
  
  if( !(stratum %in% names( x ) ) ) { 
    stop(paste("You asked to partition by",stratum,"which is not a column in your data.frame...  What am I supposed to do just guess on what you really askede for?"))
  }

  # Pull the frequency matrix
  
  get_div <- function( data, stratum, raw = FALSE ) { 
    frequency_matrix( data, 
                      stratum = stratum ) |> 
      base::merge( data, by.x = "Stratum", by.y = stratum ) %>%
      dplyr::select( !column_class(data,"locus"), -Stratum ) -> freqs
    divergence <- to_mv( data ) - freqs 
    if( raw == TRUE ) { 
      return(divergence)
    } else { 
      return( apply( divergence, 1, function(x) { return(sqrt( sum( (x)^2 )) ) } ) )
    }
  }
  
  progress <- function (x, max = 100) {
    percent <- x / max * 100
    cat(sprintf('\r Heteroscedasticity: [%-50s] %d%%',
                paste(rep('=', percent / 2), collapse = ''),
                floor(percent)))
    if (x == max)
      cat('\n')
  }
  
  if( as.princomp == TRUE ) {
    vals <- get_div(x, stratum, raw=TRUE)
    return(  prcomp( vals, center = TRUE, scale. = TRUE))
  } 
  else { 
    
    results <- data.frame( Stratum = levels(x[[stratum]]),
                           Type = "Observed",
                           Value = as.numeric( by(get_div(x, stratum), x[[stratum]], mean, na.rm=TRUE) ) )
    
    for( i in 1:N) { 
      progress(i,max=N)
      tmp <- x[ sample(nrow(x)), 2:ncol(x)]
      tmp[[stratum]] <- x[[stratum]]
      
      results <- rbind( results, 
                        data.frame( Stratum = levels(x[[stratum]]),
                                    Type = "Permuted",
                                    Value = as.numeric( by(get_div(tmp, stratum), x[[stratum]], mean, na.rm=TRUE) )
                        ) )
    }                         
    
    
    if( plot == TRUE ) { 
      
      results %>%
        gplot( aes( y=Value, x=Stratum )) + 
        geom_violin() + 
        geom_jitter( height=0, 
                     width=0.05, 
                     data = df %>% 
                       filter( Type=="Permuted"), 
                     alpha=0.25, 
                     shape=16) + 
        geom_point( size=5, 
                    color="red", 
                    data = df %>% 
                      filter( Type == "Observed"))
    }
    else { 
      return( results |> mutate( Type = factor(Type, ordered=TRUE),
                                 Stratum = factor( Stratum) ) )
    }
    
    
  }
  
  
  
                             
}


