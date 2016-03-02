  require(plotGoogleMaps)
  data(meuse)
  coordinates(meuse)<-~x+y
  proj4string(meuse) <- CRS('+init=epsg:28992')
  
  pies<-pieSP(meuse,zcol=c('zinc','lead','copper'), max.radius=120)
  pies$pie=rep(c('zinc','lead','copper'),155)
  
  m=plotGoogleMaps(pies, zcol='pie')
  
  
  
  
  
  pies$pie2=rep(1:3,155)
  spplot(pies, 'pie2')
  
  pies2<-pieSP(meuse,zcol=c('zinc','dist.m'), max.radius=70,scalelist=FALSE)
  pies2$pie=rep(c('zinc','dist.m'),155)
  
  m=plotGoogleMaps(pies2, zcol='pie')
  