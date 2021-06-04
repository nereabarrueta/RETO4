rm(list = ls())
library(odbc)
library("modeest")
library(dplyr)
library(naniar)
library(tidyverse)
library(recommenderlab)
library(tibble)
library(tidyr)

# Conexion con SQL ---------------------------------------------------------------
conexion<-dbConnect(odbc(),"conexion_ikea")

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


# Adecuacion columnas repetidas -------------------------------------------
#Coger columnas repetidas en food y furniture
df_union<-ikea[,c(3,201,5,131,7,152,8,172,10,170,13,234,14,228,16,158,19,235,15,223,21,119)]

#Convertir los NA en 0 para el bucle
df_union[is.na(df_union)] <- 0 #22 columnas
any_na(df_union)

#Sumar a la primera columna la cantidad de la segunda
for(i in seq(1,ncol(df_union),2)){
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
ikea<-ikea[,-c(3,201,5,131,7,152,8,172,10,170,13,234,14,228,16,158,19,235,15,223,21,119)]
ikea<-cbind(ikea[,1],df_union,ikea[,2:213])
ikea[, 2:223][ikea[, 2:223] == 0] <- NA
names(ikea)[1]<-"membership_id"


# Crear matriz binarizada -------------------------------------------------
ikea_bin <- ikea
for (i in names(ikea[, -1])){
  ikea_bin[,i] <- ifelse(is.na(ikea[,-1][,i]),NA,1)
}


# Dividir el df en food y furniture para filtrar por separado --------------
colnames(confood)
ncol(confood) #26 (membership incluida)
colnames(confurniture)
ncol(confurniture) #210 (membership incluida)

ikea_food<-ikea_bin[, c(1:26)]
ikea_furniture<-ikea_bin[, c(1, 27:224)]




# Filtros food -----------------------------------------------

#suma de NAs por fila (cliente)
ikea_food<- ikea_food %>%
  mutate(total_na= apply(ikea_food[, -1],1,n_miss))

ikea_food<- ikea_food %>%
  mutate(total_comprado = rowSums(ikea_food[, -c(1,27)], na.rm = TRUE))

summary(ikea_food$total_na)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.00   20.00   22.00   21.44   23.00   24.00

summary(ikea_food$total_comprado)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   2.000   3.000   3.563   5.000  23.000 

#filtramos por la media
ikea_food<-filter(ikea_food, total_na < 21.44)
#pasamos de tener 11341 filas a tener 5045 filas


#suma de NAs por columna (productos)

missings_food<-miss_var_summary(ikea_food[,-1])

col_food<- ikea_food %>%
  summarise(total_na= apply(ikea_food[,-c(1, 27, 28)],2,n_miss))

col_food<- col_food %>%
  mutate(total_comprado=colSums(ikea_food[, -c(1,27,28)], na.rm = TRUE))


summary(col_food$total_na)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1272    3909    4613    3951    4734    5022 

summary(col_food$total_comprado)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#23     311     432    1094    1136    3773 

#filtramos por el 1er cuartil
ikea_food<-ikea_food[,colSums(is.na(ikea_food))<=3909] 

#quitamos las columnas de total_na y total_comprado
ikea_food<-ikea_food[, -c(9, 10)]

#pasamos de tener 25 variables a tener 8 variables (membership_id incluida)

missings_food2<-miss_var_summary(ikea_food)

# Filtros furniture -------------------------------------------------------

#suma de NAs por fila (cliente)
ikea_furniture<- ikea_furniture %>%
  mutate(total_na= apply(ikea_furniture[,-1],1,n_miss))

ikea_furniture<- ikea_furniture %>%
  mutate(total_comprado = rowSums(ikea_furniture[, -c(1, 200)], na.rm = TRUE))

summary(ikea_furniture$total_na)


#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#61.0   189.0   193.0   191.4   195.0   198.0 
 
summary(ikea_furniture$total_comprado)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   3.000   5.000   6.575   9.000 137.000


#filtramos por por 1er cuartil
ikea_furniture<-filter(ikea_furniture, total_na <= 189.0)
#pasamoa de tener 11341 filas a tener 3011 filas

#suma de NAs por columna (productos)
missings_furniture<-miss_var_summary(ikea_furniture[, -1])

col_furniture<- ikea_furniture %>%
  summarise(total_na= apply(ikea_furniture[,-c(1,200, 201)],2,n_miss))


summary(col_furniture$total_na)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1843    2702    2894    2797    2970    3011 

#fitramos por columnas por el 1er cuartil
ikea_furniture<-ikea_furniture[, colSums(is.na(ikea_furniture))<=2702]

#quitamos columnas de total_na y total_comprado
ikea_furniture<-ikea_furniture[, -c(52, 53)]

#pasamos de tener 198 variables a tener 51 variables (membership_id incluida)


# Juntar otra vez los dos df filtrados ------------------------------------

ikea_bin_filtrado<-merge(ikea_food,ikea_furniture,by="membership_id")

#Convertir la primera columna de membershipid en nombres de filas
ikea_bin_filtrado <- data.frame(column_to_rownames(ikea_bin_filtrado, var = "membership_id"))

colnames(ikea_bin_filtrado)

dim(ikea_bin_filtrado)
#al juntar food y furniture filtrados, nos quedamos con un df con las siguientes dimensiones
#1661   57

missings_ikea<-miss_case_summary(ikea_bin_filtrado)
ikea_bin_filtrado["f9c1023bebf7947f4d0a0a39dc9cd7224e8bdb16be11824b18c313306da779e2",]

# Crear matriz de recomendacion -------------------------------------------


set.seed(456)
matriz_binarizada <- as.matrix(ikea_bin_filtrado)
matriz_binarizada <- as(matriz_binarizada,"realRatingMatrix")
matriz_binarizada
as(matriz_binarizada, "matrix")

getRatingMatrix(matriz_binarizada)



#Lista usuario a usuario
as(matriz_binarizada, "list")

# Evaluar y predecir con TopN ---------------------------------------------
set.seed(456)

e <- evaluationScheme(matriz_binarizada, method="split", train=0.75,
                      k=10, given=2, goodRating = 1)
e
algos <- list("random" = list(name = "RANDOM", param = NULL),#nn cuantos vecinos contribuyen al calculo de las predicciones
              "IBCF_5" = list(name = "IBCF"), #en el itembased no contribuyen vecinos por eso no hay nn
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

algo_coverage<-list("UBCF_100" = list(name="UBCF",param = list(nn=5)))

#Evaluar topn
eval_topn <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas
eval_coverage_topn<-evaluate(e,algo_coverage,type = "topNList",n = c(1,3,5,10,15,20))

#Graficar evaluaciones topn
plot(eval_topn)
plot(eval_coverage_topn)
getConfusionMatrix(eval_topn[["UBCF_5"]])

#El mejor modelo es UBCF
algos <- list( "UBCF_5" = list(name = "UBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
               "UBCF_7" = list(name = "UBCF", param = list(nn = 7)), 
               "UBCF_10" = list(name = "UBCF", param = list(nn = 10)))

eval_topn <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones topn UBCF
plot(eval_topn)
plot(eval_topn,"prec/rec")
getConfusionMatrix(eval_topn[["UBCF_5"]])

#para esta matriz el recomendador que mejor funciona es el de User based 5

#predicciones TopN UserBased_5
rec_bin<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=5))
pre_bin<-predict(rec_bin, matriz_binarizada,getData(e,"known"), n=5)

#predicciones coverage ub_5 100recomendaciones
rec_bin_100<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=5))
pre_bin_100<- predict(rec_bin_100, matriz_binarizada,getData(e,"known"), n=100)


# Calculo Accuracy, Coverage, Novelty y diversidad (matriz binarizada)------------------------------------

#ACCURACY
#Calcular el accuracy de UBCF 5 TOPN
plot(eval_topn[["UBCF_5"]],"prec/rec")

#COVERAGE
#Calcular el coverage de UBCF 5 topN
pred_bin_100_matriz<-as(pre_bin_100,"matrix")
df_pred_bin_100<-as.data.frame(pred_bin_100_matriz)
dim(df_pred_bin_100)

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pred_bin_100)
df_pred_bin_100<-df_pred_bin_100[-1625,] #no se le recomienda nada

#(total de items - maximo de NAs)/total de items
user_coverage<-((57-47)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pred_bin_100) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((1661-1661)/1661)*100
item_coverage

#COVERAGE TOTAL UBCF_5 TOPN
n_miss(df_pred_bin_100) #49846
dimensiones<- 1661*57
coverage<- ((dimensiones - 49846)/dimensiones)*100
coverage


#NOVELTY
#Calcular el novelty de UBCF_5 TOPN
rec_bin_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_bin_POPULAR <- predict(rec_bin_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_bin_POPULAR<-as(pre_bin_POPULAR,"matrix")

rec_bin_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_bin_POPULAR,2,f1))
quantile(apply(matriz_pre_bin_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_bin_POPULAR,2,f1),0.8)
vuelta_matriz_binpopular<-t(matriz_pre_bin_POPULAR)

dim(vuelta_matriz_binpopular)

itemsSeleccionados_bin<-vuelta_matriz_binpopular[apply(matriz_pre_bin_POPULAR,2,f1)>=47.2,] 
dim(itemsSeleccionados_bin)
rownames(itemsSeleccionados_bin)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_bin),rec_bin_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 0%


#DIVERSIDAD 
#Diversidad TOPN
dim(pred_bin_100_matriz) #1661   57
df_pred_bin_100

fucat2cat3<-dbGetQuery(conexion,"select fu.product_category_2_name,fu.product_category_3_name
from furniture as fu " )

#variedad en las predicciones en relacion a categoria3
int1<-data.frame(intersect(fucat2cat3$product_category_3_name,colnames(df_pred_bin_100)))
colnames(int1)<-c("product_category_3_name")
int2<-merge(fucat2cat3,int1,by="product_category_3_name")
length(table(int2$product_category_2_name))

diversidad_topn<- (10/57)*100
diversidad_topn

# escogemos un usuario al azar (25) ------------------------------------------------------------
as(pre_bin, "list")[25]

#[1] "Hot.snacks.Bistro"            "Boxes.and.baskets"           
#[3] "Organisers.and.washing.up"    "Accessories.for.babies"      
#[5] "Dinnerware.and.serving"       "Curtains.and.window.panels"  
#[7] "Artificial.flowers"           "Kitchen.interior.organisers."
#[9] "Kitchen.gadgets"              "Plant.pots"   


