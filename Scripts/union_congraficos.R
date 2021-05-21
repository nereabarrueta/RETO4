
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
conexion<-dbConnect(odbc(),"connection_ikea")

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


# Estadisticos ------------------------------------------------------------

#productos comprados por cada cliente (todos)
range(datos_totales$total_cantidad)
hist(datos_totales$total_cantidad, main = "Cantidad de productos comprados por clientes",
     breaks = 50, xlim = c(0, 520), col = "yellow")

#productos no comprados por cada cliente
range(datos_totales$total_na)
hist(datos_totales$total_na, main = "Cantidad de productos no comprados por clientes",
     breaks = 50, xlim = c(150, 225), col = "yellow")

#producto a producto comprado
total_productos<-colSums(datos_totales[, -c(224, 225)], na.rm = TRUE)
total_productos<-as.data.frame(total_productos)
range(total_productos)

hist(total_productos$total_productos, main = "Cantidad de productos comprados",
     breaks = 50, xlim = c(0, 12800), col = "yellow")

#productos a producto no comprado
total_na_productos <- datos_totales %>%
  summarise(total_na= apply(datos_totales[, -c(224, 225)],2,n_miss))
range(total_na_productos$total_na)

hist(total_na_productos$total_na, main = "Cantidad total de productos no comprados",
     breaks = 50, xlim =c(4600, 11400), col = "yellow")

#clientes que compran mucho
total_compramucho<-rowSums(compra_mucho[, -c(224,225)], na.rm = TRUE)
total_compramucho<-as.data.frame(total_compramucho)
str(total_compramucho)
range(total_compramucho)

media_compramucho<-rowMeans(compra_mucho[, -c(224,225)], na.rm = TRUE)
media_compramucho<-as.data.frame(media_compramucho)
str(media_compramucho)
range(media_compramucho)

hist(total_compramucho$total_compramucho, main = "Total de compras de clientes que compran mucho", 
     breaks = 25, xlim = c(0, 520), col = "red")
hist(media_compramucho$media_compramucho, main = "Media de compras de clientes que compran mucho", 
     breaks = 75, xlim = c(0, 170), col = "red")

#clientes que compran normal
total_compranormal<-rowSums(compra_normal[, -c(224,225)], na.rm = TRUE)
total_compranormal<-as.data.frame(total_compranormal)
str(total_compranormal)
range(total_compranormal)

media_compranormal<-rowMeans(compra_normal[, -c(224,225)], na.rm = TRUE)
media_compranormal<-as.data.frame(media_compranormal)
str(media_compranormal)
range(media_compranormal)

hist(total_compranormal$total_compranormal, main = "Total de compras de clientes que compran normal", 
     breaks = 35, xlim = c(0, 35), col = "blue")

hist(media_compranormal$media_compranormal, main = "Media de compras de clientes que compran normal", 
     breaks = 35, xlim = c(0, 17), col = "blue")

#productos comprados por columna

miss_var_summary(compra_mucho)

total_compramucho_col<-colSums(compra_mucho[, -c(224, 225)], na.rm = TRUE)
total_compramucho_col<-as.data.frame(total_compramucho_col)
range(total_compramucho_col)

media_compramucho_col<-colMeans(compra_mucho[, -c(208, 214, 218, 219, 221, 222, 224, 225)], na.rm = TRUE)
media_compramucho_col<-as.data.frame(media_compramucho_col)
range(media_compramucho_col)

hist(total_compramucho_col$total_compramucho_col, main = "Total compras por producto, clientes que compran mucho", 
     breaks = 35, xlim = c(0, 9100), col = "orange")

hist(media_compramucho_col$media_compramucho_col, main = "Media compras por producto, clientes que compran mucho",
     breaks = 40, xlim = c(0, 30), col = "orange")

#numero de NAs por cliente(productos no comprados)
total_na_compramucho<-compra_mucho[, 224]
total_na_compramucho<-as.data.frame(total_na_compramucho)
range(total_na_compramucho)


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
matriz_recomendacion_mucho<-compra_mucho[,-c(58,59)]

miss_case_summary(matriz_recomendacion_mucho)
#Hay un cliente que no ha comprado nada en esos productos justo
matriz_recomendacion_mucho<-matriz_recomendacion_mucho[-753,]

#Acabamos con una matriz de  2847 filas y 57 columnas 


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


#Evaluar varios recomendadores a la vez: 
e <- evaluationScheme(matriz_recomendacion, method="split", train=0.75,
                      k=3, given=1, goodRating = 1)
e
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "UBCF_10nn" = list(name = "UBCF", param = list(nn = 10)), #nn cuantos vecinos contribuyen al calculo de las predicciones
              "UBCF_20nn" = list(name = "UBCF", param = list(nn = 20)))

eval <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones
plot(eval)
plot(eval,"prec/rec")
getConfusionMatrix(eval[["UBCF_10nn"]])



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