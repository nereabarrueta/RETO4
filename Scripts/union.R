
#script para juntar food y funiture
library(odbc)
library("modeest")
library(dplyr)
library(naniar)
library(tidyverse)
library(recommenderlab)
library(tibble)
library(tidyr)


# Conexion con SQL ---------------------------------------------------------------
conexion<-dbConnect(odbc(),"conexionIKEA")

confood<-dbGetQuery(conexion,"select  fo.membership_id,fo.product_category_3_name,
  sum(fo.qty)	sumafood				            
 from food as fo
  group by fo.membership_id, fo.product_category_3_name  ;" )

dim(confood)
head(confood)
dimnames(confood)

confurniture<-dbGetQuery(conexion,"select fu.membership_id  ,
  fu.product_category_3_name,
 sum(fu.qty)  sumafurniture      
 from furniture  AS fu
  group by fu.membership_id, fu.product_category_3_name ;" )

dim(confurniture)
head(confurniture)
dimnames(confurniture)


# Pivotamos ---------------------------------------------------------------
confood<-confood%>%
  pivot_wider(names_from=product_category_3_name,values_from=sumafood)
dim(confood)
dimnames(confood) 

confood[["UNKNOWN"]]<-NULL


confurniture<-confurniture%>%
  pivot_wider(names_from=product_category_3_name,values_from=sumafurniture)

confurniture[["UNKNOWN"]]<-NULL


ikea<-merge(confood,confurniture,by="membership_id")
dim(ikea)
dimnames(ikea)[2]

#Convertir la primera columna de membershipid en nombres de filas
ikea <- data.frame(column_to_rownames(ikea, var = "membership_id"))

colnames(ikea)


# Adecuacion columnas repetidas -------------------------------------------
#Coger columnas repetidas en food y furniture
df_union<-ikea[,c(2,200,4,130,6,151,7,171,9,169,12,233,13,227,15,157,18,234,14,222,20,118)]

#Convertir los NA en 0 para el bucle
df_union[is.na(df_union)] <- 0 #22 columnas
any_na(df_union)

#Sumar a la primera columna la cantidad de la segunda
for(i in seq(1,ncol(df_union),2)){
  print(i)
  for(j in 1:nrow(df_union)){
    #print(c(j,i))
    df_union[j,i]<-df_union[j,i]+df_union[j,(i+1)]
    
  }
}

#Quitar las columnas que hemos sumado
df_union<-df_union[,-c(2,4,6,8,10,12,14,16,18,20,22)]

#Limpiar los nombres de columna (quitar el .x)
colClean <- function(x){ colnames(x) <- gsub(".x", "", colnames(x)); x } 
df_union <- colClean(df_union) 

#Dejar solo las columnas buenas para la matriz de recomendacion y convertir 0 en NA
ikea<-ikea[,-c(2,200,4,130,6,151,7,171,9,169,12,233,13,227,15,157,18,234,14,222,20,118)]
ikea<-cbind(df_union,ikea )
ikea[, 1:11][ikea[, 1:11] == 0] <- NA

# Filtro para dos matrices distintas (Compra mucho-normal) ------------------

#FILTRO MEMBERS QUE COMPRAN MUCHO/POCO
datos_totales <- ikea %>%
  mutate(total_na= apply(ikea,1,n_miss))

datos_totales <- datos_totales %>%
  mutate(total_cantidad= apply(datos_totales[,-224],1,sum, na.rm=T))

summary(datos_totales$total_cantidad)

#Este chico compra 2277 productos en total. Consideramos outlier, lo quitamos. 
outlier<-ikea[11069,]
ikea<-ikea[-11069,]
datos_totales<-datos_totales[-11069,]

summary(datos_totales$total_cantidad)

compra_mucho<-datos_totales%>%
  filter(total_cantidad>=32)

compra_normal<-datos_totales%>%
  filter(total_cantidad<32)

#Tenemos dos matrices: 
  #Clientes que compran mucho, más de 32 productos
  #Clientes que compran normal, menos de 32 productos


# Filtrado compra mucho ----------------------------------------------------

#solo vamos a filtrar por columnas por cantidad de NA
na_col_mucho <- compra_mucho %>%
  summarise(total_na= apply(compra_mucho[,-c(224,225)],2,n_miss)) #dejar fuera columnas total_na y total_cantidad

summary(na_col_mucho)
miss_var_summary<-miss_var_summary(compra_mucho)

#Filtramos por el primer cuartil (2557 missings) porque hay un porcentaje muy alto de NA por columna
compra_mucho<-compra_mucho[,colSums(is.na(compra_mucho))<=2557,] 
#Pasamos de tener 2848 filas y 223 columnas (+total_na+total_cantidad)
#a tener 2848 filas y 57 columnas (+total_na+total_cantidad)

#Quitar columnas total_cantidad y total_na
compra_mucho<-compra_mucho[,-c(58,59)]

miss_case_summary(compra_mucho)

#Volvemos a filtrar por filas
datos_totales_2 <- compra_mucho %>%
  mutate(total_na= apply(compra_mucho,1,n_miss))


summary(datos_totales_2$total_na)
#Queremos dejar filas que tengan menos NA que la mediana (45) es decir que hayan comprado 57-45=12 productos
compra_mucho<-compra_mucho[rowSums(is.na(compra_mucho))<=45,] 

miss_var_summary(compra_mucho)
#Acabamos con una matriz de  1435 filas y 57 columnas 
matriz_recomendacion_mucho<-compra_mucho

# Establecer mecanismo de evaluacion compra mucho --------------------------------------

set.seed(456)
matriz_recomendacion <- as.matrix(matriz_recomendacion_mucho)
matriz_recomendacion <- as(matriz_recomendacion,"realRatingMatrix")
matriz_recomendacion
as(matriz_recomendacion, "matrix")

getRatingMatrix(matriz_recomendacion)

#Lista usuario a usuario
as(matriz_recomendacion, "list")

miss_case_summary(matriz_recomendacion_mucho)


#Evaluar varios recomendadores a la vez topn: 
set.seed(456)

e <- evaluationScheme(matriz_recomendacion, method="split", train=0.75,
                      k=10, given=4, goodRating = 1)
e
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "IBCF_5" = list(name = "IBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

#Evaluar topn
eval_topn <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones topn
plot(eval_topn)
plot(eval_topn,"prec/rec")
getConfusionMatrix(eval_topn[["UBCF_5"]])

#Ell mejor modelo es UBCF
algos <- list( "UBCF_5" = list(name = "UBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
              "UBCF_7" = list(name = "UBCF", param = list(nn = 7)), 
              "UBCF_10" = list(name = "UBCF", param = list(nn = 10)))

eval_topn <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas


#Graficar evaluaciones topn
plot(eval_topn)
plot(eval_topn,"prec/rec")
getConfusionMatrix(eval_topn[["UBCF_5"]])


#Evaluar varios recomendadores a la vez ratings: 
set.seed(456)

e <- evaluationScheme(matriz_recomendacion, method="split", train=0.75,
                      k=10, given=4, goodRating = 1)
e
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "IBCF_5" = list(name = "IBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))


#Evaluar ratings
eval_ratings<- evaluate(e, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones topn
plot(eval_ratings)
plot(eval_ratings,"prec/rec")
getConfusionMatrix(eval_ratings[["UBCF_5"]])











#Predecir el modelo de UB 
#Con ratings para predecir valoracion de UB
prediccion_ub <- predict(eval_topn[["UBCF_5"]], getData(e, "known"), type="topNList")
prediccion_matriz_ub<-as(prediccion_ub,"matrix")

#Calcular el accuracy de UB
calcPredictionAccuracy(prediccion_ub, getData(e, "unknown"))


#Calcular el Coverage de UB
pred_cov_ub<- predict(User_based, getData(mec_split, "known"), type="ratings")
pred_cov_ub
pred_cov_matriz_ub<-as(pred_cov_ub, "matrix")

suma_no_na_ub<-function(x){
  return(sum(!is.na(x)))
}

dim(pred_cov_matriz_ub)[1]*dim(pred_cov_matriz_ub)[2]-suma_no_na_ub(pred_cov_matriz_ub)




#Evaluar ratings: 
eval_ratings <- evaluate(e, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones ratings
plot(eval_ratings)
plot(eval_ratings,"prec/rec")
getConfusionMatrix(eval_ratings[["UBCF_10"]])




#Recomendador topN (popular)
recomendador_top <- recommenderlab::Recommender(matriz_recomendacion, 
                                                method = "POPULAR")
names(getModel(recomendador_top)) # opciones derivadas del recomendador "popular"
getModel(recomendador_top)$topN # la lista puede albergar 57 recomendaciones para 1 usuario








# Filtrado compra normal ----------------------------------------------------




#COMPRA NO COMPRA
  #FILTRO: FILAS NA Y COLUMNAS NA


#CANTIDAD
  #FILTRO: FILAS CANTIDAD DE COMPRA Y COLUMNAS CANTIDAD DE COMPRA




food<-datosmm[,c(1:25)] #solo las columnas de categoria de food
furniture<-datosmm[,c(26:234)] #solo las columnas de categoria de furniture