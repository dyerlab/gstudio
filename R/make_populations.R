
#' Make Populations
#' 
#' Creates a series of populaitons from a data.frame of \code{frequencies}.
#' 
#' @param freqs The data.frame with Population, Locus, Allele, and Frequency columns
#' @param N The number of individuals to make in each population
#' @param F The inbreeding level for the individuals
#' @return A data.frame of fully formed individuals
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' f1 <- data.frame( Population = "A",
#' Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
#' Allele = rep( c("01","02"), times = 3 ), 
#' Frequency = c( 0.2, 0.8, 0.3,0.7,0.4,0.6) )
#' f2 <- data.frame( Population = "B",
#'                   Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
#'                   Allele = rep( c("01","02"), times = 3 ), 
#'                   Frequency = c( 0.3,0.7, 0.2, 0.8,0.4,0.6 ) ) 
#' f3 <- data.frame( Population = "C",
#'                   Locus = rep( c("Loc1","Loc2","Loc3"), each=2 ),
#'                   Allele = rep( c("01","02"), times = 3 ), 
#'                   Frequency = c( 0.4,0.6, 0.3,0.7, 0.2, 0.8 ) )
#' rbind(f1,f2,f3) |> 
#'   mutate( Population = factor( Population, ordered=TRUE ) ) -> freqs 
#' make_populations( freqs, N = 20, F = 0.2 ) |> summary() 
make_populations <- function( freqs, N, F = 0 ) { 
  
  if( !is(freqs,"data.frame") || !("Population" %in% names(freqs)) || !("Locus" %in% names(freqs)) || !("Allele" %in% names(freqs)) || !("Frequency" %in% names(freqs) ) ) { 
    stop("You must pass a data frame of allele frequencies representing each population.  See ?frequencies() for an example structure.")
  }
  if( N < 1 ) { 
    stop("You cannot make populations that are empty...")
  }
  if( F > 1 || F < -1 ) { 
    stop("Inapproriate value for inbreeding F, it should be bound on -1 - +1")
  }
  
  pops <- unique( as.character( freqs$Population ) ) 
  pops
  ret <- data.frame()
  for( pop in pops ) { 
    
    freqs |> 
      filter( Population == pop ) |>
      select( -Population ) -> f
    
    make_population(f,N,F) |> 
      mutate( Population = pop ) |> 
      select( Population, everything() ) -> data
    ret <- rbind( ret, data )
  }
  return( ret )
}

