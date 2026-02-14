#' Converts popgraph to geojson for import into ArcGIS Online
#' 
#' This is a convienence function to convert a population graph
#'   object into GeoJSON format for inclusion into ArcGIS Online
#' @param graph A popgraph object.
#' @param file The file for saving the GeoJSON
#' @return A textual version of the popgraph
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
to_geojson <- function( graph, file ) {
  if( !inherits( graph, "popgraph"))
    stop("This function requires a population graph to work.")
  
  ret <- character(0)
  ret <- append(ret,"{")
  
  ret <- append( ret, ' "type": "FeatureCollection",')
  ret <- append( ret, ' "features": [')
  
  
  df <- to_df(graph,mode="nodes")
  if( ! ( "Latitude" %in% names(df) && "Longitude" %in% names(df) ) ) {
    stop("You must have 'Latitude' and 'Longitude' in the graph for this to work...")
  }
  
  
  # NODES
  
  properties <- setdiff( names(df),c("Longitude","Latitude"))
  
  for( i in 1:nrow(df) ){
    feature <- c('  { "type" : "Feature",')
    feature <- append( feature, '    "geometry": {')
    feature <- append( feature, '     "type" : "Point",')
    feature <- append( feature, 
                       paste('     "coordinates": [',
                             paste( c(df[i,c("Longitude","Latitude")]),collapse=", "),"]", sep=""))
    
    if( length(properties) > 0 ) {
      feature <- append( feature, "    },")
      feature <- append( feature, '    "properties": {')
    } else {
      feature <- append( feature, '    }')
    }
      
    
    # https://en.wikipedia.org/wiki/GeoJSON
    
    
    for( label in properties ) {
      
      qt <- '"'
      val <- paste('     "',label,'": "',df[i,label],'"',sep="")
      
      if( is(df[[label]], "numeric") ) {
        qt <- ''
      }
      val <- paste('     "',label,'": ',qt,df[i,label],qt,sep="")
      if(label != properties[length(properties)])
        val <- paste( val, ",", sep="")
      
      feature <- append(feature, val )
    }
    if( length(properties)>0)
      feature <- append(feature, "    }")
    
    feature <- append( feature, "  },")
    
    ret <- append( ret, feature )
  }
  
  # Edges
  dfe <- to_df(graph, mode="edges")
  edges <- character(0)
  for( i in 1:nrow(dfe) ) {
    e <- '  {"type": "Feature",\n   "geometry": {\n    "type": "LineString",'
    e <- append( e, '    "coordinates": [')
    
    # coords
    c1 <- df[ df$name == dfe[i,"From"],c("Longitude","Latitude")]
    c2 <- df[ df$name == dfe[i,"To"],c("Longitude","Latitude")]
    c <- paste("      [",c1$Longitude,", ", c1$Latitude,"], [",c2$Longitude,", ",c2$Latitude,"]", sep="")
    e <- append(e,c)
    
    e <- append( e, "    ]\n    },")
    properties <- setdiff(names(dfe),c("From","To"))
    if( length(properties)> 0 ) {
      e <- append(e, '    "properties": {')
      
      for( property in properties ) {
        qt <- '"'
        if( is(dfe[i,property], "numeric")){
          qt <- ''
        }
        p <- paste('     "',property,'":',qt,dfe[i,property],qt, sep="")
        
        if( property != properties[length(properties)])
          p <- paste(p,",",sep="")
        e <- append( e, p )
      }
      e <- append(e, "    }")
    
    }
    if( i != nrow(dfe) )
      e <- append(e,'  },')
    else
      e <- append(e,'  }')
    
    edges <- append(edges, paste(e,collapse="\n"))
  }
  ret <- append( ret, edges)
  
  
  # Close it up
  ret <- append( ret, " ]")
  ret <- append( ret, "}")
  
  ret <- paste( ret, collapse="\n")
  
  return( ret )
}


