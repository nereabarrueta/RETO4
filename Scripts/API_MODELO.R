#library(plumber)


#* @apiTitle RECOMENDADOR USER BASED BASADO EN 5 USUARIOS 
#* @apiDescription Inserte un numero de fila en el recomendador. El recomendador devuelve 5 productos para el usuario de la fila correspondiente. El recomendador sirve solo para algunos usuarios que han comprado mas de 32 productos (compran mucho). Por eso va a haber usuarios a los que no haga recomendaciones. 

#* @param numero_de_fila
#* @get /RecomendadorIkea
function(numero_de_fila){
  
  library(recommenderlab)
  load("ub5_topn.Rdata")
  
  #numero_de_fila <- as.numeric(numero_de_fila)
  
  #a <- as(matriz_recomendacion,"data.frame")
  
  #datosparaprediccion<-data.frame(a[numero_de_fila,])
  #names(datosparaprediccion)<-names(a[numero_de_fila,])
  #datosparaprediccion <- as(datosparaprediccion,"realRatingMatrix")
  
  y<-predict(rec_ucbf5_topn,matriz_recomendacion, getData(e, "known"), n=5)
  y <- as(y,"list")
  #print(names(y))
  prediccion <- y[names(y) == numero_de_fila]
  
  return(prediccion)
}



