#script para juntar food y funiture
library(odbc)
library("modeest")
library(dplyr)
library(naniar)
library(tidyverse)
library(recommenderlab)
library(tibble)
library(tidyr)
library(xlsx)


# Conexion con SQL ---------------------------------------------------------------
conexion<-dbConnect(odbc(),"conexionikea")

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
  #print(i)
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
miss_case_summary(compra_mucho) 
#el que menos productos compra compra 12 productos, es decir no compra 45
#el que mas productos compra compra 35 productos, es decir no compra 22

#Acabamos con una matriz de  1435 filas y 57 columnas 
matriz_recomendacion_mucho<-compra_mucho
pct_miss(compra_mucho) #72.35 %

# Filtrado compra normal ----------------------------------------------------

#FILTRADO POR COLUMNAS
#solo vamos a filtrar por columnas por cantidad de NA
na_col_normal <- compra_normal %>%
  summarise(total_na= apply(compra_normal[,-c(224,225)],2,n_miss)) #dejar fuera columnas total_na y total_cantidad

summary(na_col_normal)
#total_na   
# Min.   :3679  
# 1st Qu.:8147  
# Median :8380  
# Mean   :8202  
# 3rd Qu.:8461  
# Max.   :8492 

miss_var_summary2<-miss_var_summary(compra_normal)
miss_var_summary2

#Filtramos por el primer cuartil (8147 missings) porque hay un porcentaje muy alto de NA por columna
compra_normal<-compra_normal[,colSums(is.na(compra_normal))<=8147] 

#Pasamos de tener 8492 filas y 225 columnas (+total_na+total_cantidad)
#a tener 8492 filas y 58 columnas (+total_na+total_cantidad)

miss_case_summary(compra_normal[,-c(57,58)])

#FILTRADO FILAS
#quitamos membership con 100% de missings despues de filtrar por cols
na_row_normal <- compra_normal %>%
  summarise(total_na= apply(compra_normal[,-c(57,58)],1,n_miss))

summary(na_row_normal)

compra_normal<-compra_normal[rowSums(is.na(compra_normal))<56,] #para filas con 100%
#8492 filas y 58 columnas se convierten en 8481 filas y 58 columnas

#seguimos filtrando
na_row_normal <- compra_normal %>%
  summarise(total_na= apply(compra_normal[,-c(57,58)],1,n_miss))

summary(na_row_normal)

#filtro mas estricto por mediana (50% de los datos completos)
compra_normal<-compra_normal[rowSums(is.na(compra_normal))<50,]

#partiamos de 8481 de filas y 58 cols
#y nos quedamos con 3078 filas 58 cols

#comprobamos los nuevos valores
na_row_normal <- compra_normal %>%
  summarise(total_na= apply(compra_normal[,-c(57,58)],1,n_miss))
summary(na_row_normal)

# total_na    
# Min.   :38.00  
# 1st Qu.:46.00  
# Median :48.00  
# Mean   :47.32  
# 3rd Qu.:49.00  
# Max.   :49.00  


#Quitar columnas total_cantidad y total_na
matriz_rec_normal<-compra_normal[,-c(57,58)]

# Convertir los NAs en 0 --------------------------------------------------

#compra_mucho
compra_mucho_svd<-compra_mucho
for (i in names(compra_mucho)){
  compra_mucho_svd[,i] <- ifelse(is.na(compra_mucho[,i]),0,compra_mucho_svd[,i])
}

#compra_normal
compra_normal_svd<-matriz_rec_normal
for (i in names(matriz_rec_normal)){
  compra_normal_svd[,i] <- ifelse(is.na(matriz_rec_normal[,i]),0,compra_normal_svd[,i])
}

#PCA actua sobre los items (columnas)
tiposItems_mucho<-prcomp(compra_mucho_svd,rank=2)
tiposItems_mucho

tiposItems_normal<-prcomp(compra_normal_svd,rank=2)
tiposItems_normal

#PCA actua sbore los usuarios (filas)
tiposUsuarios_mucho<-prcomp(t(compra_mucho_svd),rank=2)
tiposUsuarios_mucho

tiposUsuarios_normal<-prcomp(t(compra_normal_svd),rank=2)
tiposUsuarios_normal

#aplicar SVD a los dos datasets

#compra_mucho
svd_compramucho <- svd(compra_mucho_svd) 
svd_compramucho$u # proyeccion de usuarios 
diag(svd_compramucho$d) 
t(svd_compramucho$v) #proyeccion de items

#compra_normal
svd_compranormal <- svd(compra_normal_svd) 
svd_compranormal$u # proyeccion de usuarios 
diag(svd_compranormal$d) 
t(svd_compranormal$v) #proyeccion de items



#compra_mucho elbow method

set.seed(7)
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:50,  function(k){
  model <- kmeans(x = compra_mucho_svd, centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss

elbow_df <- data.frame(
  k = 1:50,
  tot_withinss = tot_withinss)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:50)

#compra_normal elbow method
tot_withinss <- map_dbl(1:50,  function(k){
  model <- kmeans(x = compra_normal_svd, centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss

elbow_df <- data.frame(
  k = 1:50,
  tot_withinss = tot_withinss)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:50)

#elegimos k=3 para los dos datasets


#las primeras 3 columnas k=3 
svd3_mucho<-svd_compramucho$u[,1:3]%*%diag(svd_compramucho$d)[1:3,]%*%t(svd_compramucho$v)[,1:3]
svd3_mucho

all(compra_mucho == svd_compramucho$u%*%diag(svd_compramucho$d)%*%t(svd_compramucho$v))


svd3_normal<-svd_compranormal$u[,1:3]%*%diag(svd_compranormal$d)[1:3,]%*%t(svd_compranormal$v)[,1:3]
svd3_normal

all(compra_normal == svd_compranormal$u%*%diag(svd_compranormal$d)%*%t(svd_compranormal$v))


#guardamos las componentes para comprobar luego
u_mucho<-svd_compramucho$u
v_mucho<-svd_compramucho$v
d_mucho<-svd_compramucho$d

u_normal<-svd_compranormal$u
v_normal<-svd_compranormal$v
d_normal<-svd_compranormal$d




# Establecer mecanismo de evaluacion compra mucho --------------------------------------

set.seed(456)
compra_mucho_rec <- as.matrix(compra_mucho)
compra_mucho_rec <- as(compra_mucho_rec,"realRatingMatrix")
compra_mucho_rec
as(compra_mucho_rec, "matrix")

getRatingMatrix(compra_mucho_rec)

#Lista usuario a usuario
as(compra_mucho_rec, "list")

miss_case_summary(compra_mucho)


# Comparar compra_mucho ---------------------------------------------------

#TopN: user based 5
#Ratings user based 7

#TOPN
set.seed(456)
e <- evaluationScheme(compra_mucho_rec, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)

algos <- list("SVD" = list(name = "SVD", param = list(k = 12)),
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

eval_topn<- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))

#graficar para evaluaciones de topn
plot(eval_topn)
plot(eval_topn, "prec/rec")

#Predecir el modelo de SVD 
#Con topN para predecir valoracion de SVD
rec_svd_topn_mucho<- Recommender(getData(e,"train"), method= "SVD")
pred_svd_topn_mucho<-predict(rec_svd_topn_mucho, compra_mucho_rec,getData(e ,"known"), n=5, type= "topNList") #5 recomendaciones


#RATINGS
set.seed(456)

e <- evaluationScheme(compra_mucho_rec, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)

algos <- list("SVD" = list(name = "SVD", param = list(k = 12)),
              "UBCF_7" = list(name = "UBCF", param = list(nn = 7)))

eval_ratings<- evaluate(e, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones ratings
plot(eval_ratings)
avg(eval_ratings)

#Predecir el modelo de SVD 
#Con ratings para predecir valoracion de SVD0
rec_svd_ratings_mucho<- Recommender(getData(e,"train"), method= "SVD")
pred_svd_ratings_mucho<-predict(rec_svd_ratings_mucho, compra_mucho_rec,getData(e ,"known"), n=5, type= "ratings") #5 recomendaciones


# Calculo Accuracy, Coverage y Novelty (compra mucho)------------------------------------

#ACCURACY
#Calcular el accuracy de SVD TOPN
plot(eval_topn[["SVD"]],"prec/rec")

#Calcular el accuracy de SVD RATINGS
avg(eval_ratings$SVD)

#COVERAGE
#Calcular el coverage de SVD topN
pred_svd_topn_mucho_matriz<-as(pred_svd_topn_mucho,"matrix")
df_pred_svd_topn_mucho<-as.data.frame(pred_svd_topn_mucho_matriz)
dim(df_pred_svd_topn_mucho)

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pred_svd_topn_mucho)
#(total de items - maximo de NAs)/total de items
user_coverage<-((57-35)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pred_svd_topn_mucho) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((1435-1054)/1435)*100
item_coverage

#COVERAGE TOTAL TOPN
n_miss(df_pred_svd_topn_mucho) #22614
dimensiones<- 1435*57
coverage<- ((dimensiones - 22614)/dimensiones)*100
coverage


#Calcular el coverage de SVD ratings
pred_svd_ratings_mucho_matriz<-as(pred_svd_ratings_mucho,"matrix")
df_pred_svd_ratings_mucho<-as.data.frame(pred_svd_ratings_mucho_matriz)
dim(df_pred_svd_ratings_mucho)

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pred_svd_topn_mucho)
#(total de items - maximo de NAs)/total de items
user_coverage<-((57-35)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pred_svd_topn_mucho) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((1435-1054)/1435)*100
item_coverage

#COVERAGE TOTAL TOPN
n_miss(df_pred_svd_topn_mucho) #22614
dimensiones<- 1435*57
coverage<- ((dimensiones - 22614)/dimensiones)*100
coverage

#NOVELTY
#Calcular el novelty de SVD TOPN
rec_svd_topn_mucho_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_svd_topn_mucho_POPULAR <- predict(rec_svd_topn_mucho_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_svd_topn_mucho_POPULAR<-as(pre_svd_topn_mucho_POPULAR,"matrix")

rec_svd_topn_mucho_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_svd_topn_mucho_POPULAR,2,f1))
quantile(apply(matriz_pre_svd_topn_mucho_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_svd_topn_mucho_POPULAR,2,f1),0.8)
vuelta_matriz_svdtopnmuchopopular<-t(matriz_pre_svd_topn_mucho_POPULAR)

dim(vuelta_matriz_svdtopnmuchopopular)

itemsSeleccionados_svdtopnmucho<-vuelta_matriz_svdtopnmuchopopular[apply(matriz_pre_svd_topn_mucho_POPULAR,2,f1)>=70.4,] 
dim(itemsSeleccionados_svdtopnmucho)
rownames(itemsSeleccionados_svdtopnmucho)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_svdtopnmucho),rec_svd_topn_mucho_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 100% 


#Calcular el novelty de SVD ratings
rec_svd_ratings_mucho_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_svd_ratings_mucho_POPULAR <- predict(rec_svd_ratings_mucho_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_svd_ratings_mucho_POPULAR<-as(pre_svd_ratings_mucho_POPULAR,"matrix")

rec_svd_ratings_mucho_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_svd_ratings_mucho_POPULAR,2,f1))
quantile(apply(matriz_pre_svd_ratings_mucho_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_svd_ratings_mucho_POPULAR,2,f1),0.8)
vuelta_matriz_svdratingsmuchopopular<-t(matriz_pre_svd_ratings_mucho_POPULAR)

dim(vuelta_matriz_svdratingsmuchopopular)

itemsSeleccionados_svdratingsmucho<-vuelta_matriz_svdratingsmuchopopular[apply(matriz_pre_svd_ratings_mucho_POPULAR,2,f1)>=70.4,] 
dim(itemsSeleccionados_svdratingsmucho)
rownames(itemsSeleccionados_svdratingsmucho)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_svdratingsmucho),rec_svd_ratings_mucho_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 100% 



# Establecer mecanismo de evaluacion para compra normal ------------------

set.seed(456)
matriz_rec_norm <- as.matrix(matriz_rec_normal)
matriz_rec_norm <- as(matriz_rec_norm,"realRatingMatrix")
matriz_rec_norm #3078 x 56 rating matrix of class ‘realRatingMatrix’ with 26709 ratings

as(matriz_rec_norm, "matrix") #modo matrix
getRatingMatrix(matriz_rec_norm) #modo ratingmatrix;NA representado como puntos
as(matriz_rec_norm, "list") #modo lista

# Comparar compra normal --------------------------------------------------

#TOPN

set.seed(456)
ev_normal <- evaluationScheme(matriz_rec_norm, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)

algos <- list("SVD" = list(name = "SVD", param = list(k = 12)),
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

eval_topn_normal<- evaluate(ev_normal, algos, type = "topNList",n = c(1,3,5,10,15,20))

#graficar para evaluaciones de topn
plot(eval_topn_normal)

#Predecir el modelo de SVD 
#Con topN para predecir valoracion de SVD0
rec_svd_topn_normal<- Recommender(getData(ev_normal,"train"), method= "SVD")
pred_svd_topn_normal<-predict(rec_svd_topn_normal, matriz_rec_norm,getData(ev_normal ,"known"), n=5, type= "topNList") 


#RATINGS
set.seed(456)
ev_normal <- evaluationScheme(matriz_rec_norm, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)

algos <- list("SVD" = list(name = "SVD", param = list(k = 12)),
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

eval_ratings_normal<- evaluate(ev_normal, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones ratings
plot(eval_ratings_normal)
avg(eval_ratings_normal)
#SVD es mejor modelo

getConfusionMatrix(eval_ratings_normal[["SVD"]])

#Predecir el modelo de SVD 
#Con ratings para predecir valoracion de SVD0
rec_svd_ratings_normal<- Recommender(getData(ev_normal,"train"), method= "SVD")
pred_svd_ratings_normal<-predict(rec_svd_ratings_normal, matriz_rec_norm,getData(ev_normal ,"known"), n=5, type= "ratings") #5 recomendaciones



# Calculo Accuracy, Coverage y Novelty (compra normal)------------------------------------

#ACCURACY
#Calcular el accuracy de SVD TOPN
plot(eval_topn_normal[["SVD"]],"prec/rec")

#Calcular el accuracy de SVD RATINGS
avg(eval_ratings_normal[["SVD"]])

#COVERAGE
#Calcular el coverage de SVD topN
pred_svd_topn_normal_matriz<-as(pred_svd_topn_normal,"matrix")
df_pred_svd_topn_normal<-as.data.frame(pred_svd_topn_normal_matriz)
dim(df_pred_svd_topn_normal)

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pred_svd_topn_normal)
#(total de items - maximo de NAs)/total de items
user_coverage<-((56-51)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pred_svd_topn_normal) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((3078-3078)/3078)*100
item_coverage

#COVERAGE TOTAL TOPN
n_miss(df_pred_svd_topn_normal) #156978
dimensiones<- 3078*56
coverage<- ((dimensiones - 156978)/dimensiones)*100
coverage


#COVERAGE
#Calcular el coverage de SVD ratings
pred_svd_ratings_normal_matriz<-as(pred_svd_ratings_normal,"matrix")
df_pred_svd_ratings_normal<-as.data.frame(pred_svd_ratings_normal_matriz)
dim(df_pred_svd_ratings_normal)

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pred_svd_ratings_normal)
#(total de items - maximo de NAs)/total de items
user_coverage<-((56-18)/56)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pred_svd_ratings_normal) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((3078-2041)/3078)*100
item_coverage

#COVERAGE TOTAL RATINGS
n_miss(df_pred_svd_ratings_normal) #26709
dimensiones<- 3078*56
coverage<- ((dimensiones - 26709)/dimensiones)*100
coverage


#NOVELTY
#Calcular el novelty de SVD TOPN
rec_svd_topn_normal_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_svd_topn_normal_POPULAR <- predict(rec_svd_topn_normal_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_svd_topn_normal_POPULAR<-as(pre_svd_topn_normal_POPULAR,"matrix")

rec_svd_topn_normal_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_svd_topn_normal_POPULAR,2,f1))
quantile(apply(matriz_pre_svd_topn_normal_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_svd_topn_normal_POPULAR,2,f1),0.8)
vuelta_matriz_svdtopnnormalpopular<-t(matriz_pre_svd_topn_normal_POPULAR)

dim(vuelta_matriz_svdtopnnormalpopular)

itemsSeleccionados_svdtopnnormal<-vuelta_matriz_svdtopnnormalpopular[apply(matriz_pre_svd_topn_normal_POPULAR,2,f1)>=70.4,] 
dim(itemsSeleccionados_svdtopnnormal)
rownames(itemsSeleccionados_svdtopnnormal)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_svdtopnnormal),rec_svd_topn_normal_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 100% 


#Calcular el novelty de SVD ratings
rec_svd_ratings_normal_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_svd_ratings_normal_POPULAR <- predict(rec_svd_ratings_normal_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_svd_ratings_normal_POPULAR<-as(pre_svd_ratings_normal_POPULAR,"matrix")

rec_svd_ratings_normal_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_svd_ratings_normal_POPULAR,2,f1))
quantile(apply(matriz_pre_svd_ratings_normal_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_svd_ratings_mucho_POPULAR,2,f1),0.8)
vuelta_matriz_svdratingsnormalpopular<-t(matriz_pre_svd_ratings_normal_POPULAR)

dim(vuelta_matriz_svdratingsnormalpopular)

itemsSeleccionados_svdratingsnormal<-vuelta_matriz_svdratingsnormalpopular[apply(matriz_pre_svd_ratings_normal_POPULAR,2,f1)>=70.4,] 
dim(itemsSeleccionados_svdratingsnormal)
rownames(itemsSeleccionados_svdratingsnormal)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_svdratingsnormal),rec_svd_ratings_normal_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 100% 



# Probar las recomendaciones de los modelos con usuarios al azar ----------

#COMPRA MUCHO TOPN
as(pred_svd_topn_mucho, "list")[2]

#COMPRA MUCHO RATINGS
as(pred_svd_ratings_mucho, "list")[2]

#COMPRA NORMAL TOPN
as(pred_svd_topn_normal, "list")[12]

#COMPRA NORMAL RATIGNS
as(pred_svd_ratings_normal, "list")[12]
