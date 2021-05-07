# Carga de datos ----------------------------------------------------------

source("Scripts/librerias.R")
source("Scripts/data_quality.R")

# Tabla estadisticos variables continuas furniture ------------------------
str(food)
str(furniture)
tabla<- select(furniture, c(7,8))
str(tabla)
#Minimo,1Quartil,Mediana,3Quartil,Maximo
tablacuartiles<- summarise_all(tabla, funs(quantile(.,na.rm=TRUE)))
tablacuartiles<-as.matrix(tablacuartiles)
tablacuartiles<-t(tablacuartiles)
tablacuartiles<-as.data.frame(tablacuartiles)
colnames(tablacuartiles)<-c("Minimo", "1Quartil","Mediana", "3Quartil", "Maximo")

#Media
media_variables<-summarise_all(tabla, funs(mean(.,na.rm=TRUE)))
media_variables<-as.matrix(media_variables)
media_variables<-t(media_variables)
media_variable<-as.data.frame(media_variables)
colnames(media_variables)<-c("media")
Media<-media_variables[,"media"]

#Desviacion tipica
sd_variables<-summarise_all(tabla,funs(sd(.,na.rm=TRUE)))
sd_variables<-as.matrix(sd_variables)
sd_variables<-t(sd_variables)
sd_variables<-as.data.frame(sd_variables)
colnames(sd_variables)<-c("Desviacion_tipica")
Desviacion_tipica<-sd_variables[,"Desviacion_tipica"]
#Kurtosis
kurtosis_variables<-summarise_all(tabla, funs(kurtosis(.,na.rm=TRUE)))
kurtosis_variables<-as.matrix(kurtosis_variables)
kurtosis_variables<-t(kurtosis_variables)
kurtosis_variables<-as.data.frame(kurtosis_variables)
colnames(kurtosis_variables)<-c("Coeficiente_kurtosis")
Coeficiente_kurtosis<-kurtosis_variables[,"Coeficiente_kurtosis"]
#Skewness (Asimetr?a)
Skewness_variables<-summarise_all(tabla, funs(skewness(.,na.rm=TRUE)))
Skewness_variables<-as.matrix(Skewness_variables)
Skewness_variables<-t(Skewness_variables)
Skewness_variables<-as.data.frame(Skewness_variables)
colnames(Skewness_variables)<-c("Coeficiente_Asimetria")
Coeficiente_Asimetria<-Skewness_variables[,"Coeficiente_Asimetria"]


tabla_var_missings_ <- miss_var_summary(tabla)
#Excel con los estadisticos descriptivos
df_analisis_descriptivo<-cbind(tablacuartiles, Media, Desviacion_tipica, Coeficiente_kurtosis, Coeficiente_Asimetria)
names(df_analisis_descriptivo)
colnames(df_analisis_descriptivo)<-c("Minimo", "1Quartil", "Mediana", "3Quartil", "Maximo","Media","Desviacion_tipica","Coeficiente_kurtosis","Coeficiente_Asimetria")

library(xlsx)
#write.xlsx(df_analisis_descriptivo, "DatosTransformados/datadiscovery_furniture_continuas.xlsx")

# Tabla estadisticos variables continuas food ------------------------

tabla<- select(food, c(9,10))
#Minimo,1Quartil,Mediana,3Quartil,Maximo
tablacuartiles<- summarise_all(tabla, funs(quantile(.,na.rm=TRUE)))
tablacuartiles<-as.matrix(tablacuartiles)
tablacuartiles<-t(tablacuartiles)
tablacuartiles<-as.data.frame(tablacuartiles)
colnames(tablacuartiles)<-c("Minimo", "1Quartil","Mediana", "3Quartil", "Maximo")

#Media
media_variables<-summarise_all(tabla, funs(mean(.,na.rm=TRUE)))
media_variables<-as.matrix(media_variables)
media_variables<-t(media_variables)
media_variable<-as.data.frame(media_variables)
colnames(media_variables)<-c("media")
Media<-media_variables[,"media"]

#Desviacion tipica
sd_variables<-summarise_all(tabla,funs(sd(.,na.rm=TRUE)))
sd_variables<-as.matrix(sd_variables)
sd_variables<-t(sd_variables)
sd_variables<-as.data.frame(sd_variables)
colnames(sd_variables)<-c("Desviacion_tipica")
Desviacion_tipica<-sd_variables[,"Desviacion_tipica"]
#Kurtosis
kurtosis_variables<-summarise_all(tabla, funs(kurtosis(.,na.rm=TRUE)))
kurtosis_variables<-as.matrix(kurtosis_variables)
kurtosis_variables<-t(kurtosis_variables)
kurtosis_variables<-as.data.frame(kurtosis_variables)
colnames(kurtosis_variables)<-c("Coeficiente_kurtosis")
Coeficiente_kurtosis<-kurtosis_variables[,"Coeficiente_kurtosis"]
#Skewness (Asimetr?a)
Skewness_variables<-summarise_all(tabla, funs(skewness(.,na.rm=TRUE)))
Skewness_variables<-as.matrix(Skewness_variables)
Skewness_variables<-t(Skewness_variables)
Skewness_variables<-as.data.frame(Skewness_variables)
colnames(Skewness_variables)<-c("Coeficiente_Asimetria")
Coeficiente_Asimetria<-Skewness_variables[,"Coeficiente_Asimetria"]


tabla_var_missings_ <- miss_var_summary(tabla)
#Excel con los estadisticos descriptivos
df_analisis_descriptivo<-cbind(tablacuartiles, Media, Desviacion_tipica, Coeficiente_kurtosis, Coeficiente_Asimetria)
names(df_analisis_descriptivo)
colnames(df_analisis_descriptivo)<-c("Minimo", "1Quartil", "Mediana", "3Quartil", "Maximo","Media","Desviacion_tipica","Coeficiente_kurtosis","Coeficiente_Asimetria")

library(xlsx)
#write.xlsx(df_analisis_descriptivo, "DatosTransformados/datadiscovery_food_continuas.xlsx")


# Duplicados por fila: no hay ---------------------------------------------
duplicados_furniture<-furniture[duplicated(furniture[,c(1:10)]),]
duplicados_food<-food[duplicated(food[,c(1:12)]),]



# conteo de variables no numericas ----------------------------------------
str(furniture)

library(dplyr)

dfconteo_c1<-food%>%
  count(product_category_1_name)
dfconteo_c2<-food%>%
  count(product_category_2_name)
dfconteo_c3<-food%>%
  count(product_category_3_name)
dfconteo_store<-food%>%
  count(store)

#proporcionalmente en tarta
#preparacion de datos

p1<-sum(dfconteo_c1$n)
c1<-dfconteo_c1[,2]/p1
p2<-sum(dfconteo_c2$n)
c2.1<-dfconteo_c2[1,2]/p2
c2.2<-dfconteo_c2[2,2]/p2
c2.3<-dfconteo_c2[3,2]/p2
c2.4<-dfconteo_c2[4,2]/p2
p3<-sum(dfconteo_c3$n)
c3.1<-dfconteo_c3[1,2]/p3
c3.2<-dfconteo_c3[2,2]/p3
c3.3<-dfconteo_c3[3,2]/p3
c3.4<-dfconteo_c3[4,2]/p3
c3.5<-dfconteo_c3[5,2]/p3
c3.6<-dfconteo_c3[6,2]/p3
c3.7<-dfconteo_c3[7,2]/p3
c3.8<-dfconteo_c3[8,2]/p3
c3.9<-dfconteo_c3[9,2]/p3
c3.10<-dfconteo_c3[10,2]/p3
c3.11<-dfconteo_c3[11,2]/p3
c3.12<-dfconteo_c3[12,2]/p3
c3.13<-dfconteo_c3[13,2]/p3
c3.14<-dfconteo_c3[14,2]/p3
c3.15<-dfconteo_c3[15,2]/p3
c3.16<-dfconteo_c3[16,2]/p3
c3.17<-dfconteo_c3[17,2]/p3
c3.18<-dfconteo_c3[18,2]/p3
c3.19<-dfconteo_c3[19,2]/p3
c3.20<-dfconteo_c3[20,2]/p3
c3.21<-dfconteo_c3[21,2]/p3
c3.22<-dfconteo_c3[22,2]/p3
c3.23<-dfconteo_c3[23,2]/p3
c3.24<-dfconteo_c3[24,2]/p3
c3.25<-dfconteo_c3[25,2]/p3

porcentajes1<-cbind(c1)
porcentajes1<-as.data.frame(t(porcentajes1))
colnames(porcentajes1)<-("Porcentajes categoria 1")
porcentajes2<-cbind(c2.1,c2.2,c2.3,c2.4)
porcentajes2<-as.data.frame(t(porcentajes2))
colnames(porcentajes2)<-("Porcentajes categoria 2")
porcentajes3<-cbind(c3.1,c3.2,c3.3,c3.4,c3.5,c3.6,c3.7,c3.8,c3.9,c3.10,c3.11,c3.12,c3.13,c3.14,c3.15,c3.16,c3.17,c3.18,c3.19,c3.20,c3.21,c3.22,c3.23,c3.24,c3.25)
porcentajes3<-as.data.frame(t(porcentajes3))
colnames(porcentajes3)<-("Porcentajes categoria 3")
Nombre1<-c("Food")
Nombre2<-c("Bistro","Food Market", "Other products","Restaurant")
vecnombres3<-as.character(dfconteo_c3$product_category_3_name)
Nombre3<-c(vecnombres3)

df_t1<-cbind(Nombre1,porcentajes1)
df_t2<-cbind(Nombre2,porcentajes2)
df_t3<-cbind(Nombre3,porcentajes3)


ggplot(df_t2,aes(x="",y=porcentajes2, fill=Nombre2))+
  geom_bar(stat = "identity", color="white")+
  geom_text(aes(label=porcentajes1), position=position_stack(vjust=0.5),color="black",size=4)+
    coord_polar(theta = "y") + theme_pander() + scale_fill_brewer(palette="YlOrRd") +
    labs(title="Gráfico Tarta", fill= "Nombre1")
