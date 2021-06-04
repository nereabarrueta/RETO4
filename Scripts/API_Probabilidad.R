
#* @apiTitle HABITACION CON MAYOR PROBABILIDAD
#* @apiDescription Develve la habitacion con el mayor probabilidad y dicha probabilidad. 

#* No metas dos habitaciones que tengan el valor maximo y este coincida porque solo te devuelve uno de los dos
#* @param Cocina La probabilidad de la cocina
#* @param Banno La probabilidad del banno
#* @param Salon La probabilidad del salon
#* @param Sotano La probabilidad del sotano
#* @param Despacho La probabilidad del despacho
#* @get /sum
function(Cocina, Banno, Salon, Sotano, Despacho){
  library(dplyr)
  Habitacion <- c("Cocina", "Banno", "Salon", "Sotano", "Despacho")
  Probabilidad <- c( as.numeric(Cocina), as.numeric(Banno), as.numeric(Salon), as.numeric(Sotano), as.numeric(Despacho))
  
  df <- data.frame(Habitacion, Probabilidad)
  print(df)
  
  df[which.max(df$Probabilidad),]
}


