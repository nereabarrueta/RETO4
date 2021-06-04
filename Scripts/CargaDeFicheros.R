food<-read.csv("DatosOriginales/food.csv", header = TRUE, sep = ";")

furniture1<-read.csv("DatosOriginales/furniture_first.csv", header = TRUE, sep = ";")
furniture2<-read.csv("DatosOriginales/furniture_second.csv", header = TRUE, sep = ";")

furniture<-rbind(furniture1, furniture2)