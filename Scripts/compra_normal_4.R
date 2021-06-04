  #MATRIZ COMPRA NORMAL RECOMENDAODR

# Quitamos columnas extras generadas en el filtrado -----------------------
#Quitar columnas total_cantidad y total_na
matriz_rec_normal<-compra_normal[,-c(57,58)]

# Establecer mecanismo de evaluacion compra normal --------------------------------------
set.seed(456)
matriz_rec_norm <- as.matrix(matriz_rec_normal)
matriz_rec_norm <- as(matriz_rec_norm,"realRatingMatrix")
matriz_rec_norm #3078 x 56 rating matrix of class realRatingMatrix with 26709 ratings

as(matriz_rec_norm, "matrix") #modo matrix
getRatingMatrix(matriz_rec_norm) #modo ratingmatrix;NA representado como puntos
as(matriz_rec_norm, "list") #modo lista

# Evaluar y predecir TOPN ------------------------------------------------------------

#Evaluar varios recomendadores a la vez topn: 
er <- evaluationScheme(matriz_rec_norm, method="split", train=0.75,
                       k=10, given=5, goodRating = 1)
er
algos <- list("random" = list(name = "RANDOM", param = NULL),#nn cuantos vecinos contribuyen al calculo de las predicciones
              "IBCF_5" = list(name = "IBCF"), #en el itembased no contribuyen vecinos por eso no hay nn
              "UBCF_5" = list(name = "UBCF", param = list(nn = 5)))

#Evaluar topn
eval_topn_norm <- evaluate(er, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones topn
plot(eval_topn_norm)
plot(eval_topn_norm,"prec/rec")
getConfusionMatrix(eval_topn_norm[["UBCF_5"]])


#El mejor modelo es UBCF, ahora comprobamos que n es mejor
algos <- list( "UBCF_5" = list(name = "UBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
               "UBCF_7" = list(name = "UBCF", param = list(nn = 7)), 
               "UBCF_10" = list(name = "UBCF", param = list(nn = 10)))

eval_topn_norm <- evaluate(er, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas


#Graficar evaluaciones topn UBCF
plot(eval_topn_norm)
plot(eval_topn_norm,"prec/rec")
getConfusionMatrix(eval_topn_norm[["UBCF_5"]])
#nos quedamos con UBCF5

#predicciones TopN UserBase_5
rec_ubcf5_topn_norm<- Recommender(getData(er,"train"), method= "UBCF", param= list(nn=5))
pre_ub5_topn_norm<- predict(rec_ubcf5_topn_norm, matriz_rec_norm,getData(er,"known"), n=5) #queremos cinco recomendaciones
as(pre_ub5_topn_norm, "matrix")
#predicciones TopN UserBase_100
rec_ubcf100_topn_norm<- Recommender(getData(er,"train"), method= "UBCF", param= list(nn=5))
pre_ub100_topn_norm<-predict(rec_ubcf100_topn_norm, matriz_rec_norm,getData(er,"known"), n=100) 

# Evaluar y predecir RATINGS ----------------------------------------------

#Evaluar varios recomendadores a la vez ratings: 
er2 <- evaluationScheme(matriz_rec_norm, method="split", train=0.75,
                        k=10, given=5, goodRating = 1)
er2


algos2 <- list("UBCF_5" = list(name = "UBCF", param = list(nn = 5))) #definitivo

#Evaluar ratings
eval_ratings_norm<- evaluate(er2, algos2, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Decidimos el mejor
#-----------------------------
#MIN MAE 1.151924 (UBCF10)#mejor
#MIN MAE 1.159388  (IBCF10) 
#-----------------------------
#MIN MAE 1.14737 (UBCF7) #mejor
#MIN MAE 1.159388 (IBCF7) 
#-----------------------------
#MIN MAE 1.134002 (UBCF5) #mejor
#MIN MAE 1.159388 (IBCF5) 

#El mejor modelo es UBCF5
#tras haber hecho una comparacion igual que en la matriz de compra_mucho, nos hemos
#quedado con el metodo UBCF5, ejecutado en lineas anteriores

#Detalles del modelo
getConfusionMatrix(eval_ratings_norm[["UBCF_5"]])

#Predicciones ratings para UserBase_5
rec_ubcf5_ratings_norm<- Recommender(getData(er2,"train"), method= "UBCF", param= list(nn=5))
pre_ub5_rating_norm<- predict(rec_ubcf5_ratings_norm, matriz_rec_norm,getData(er2,"known"), n=5, type= "ratings")#5 recomendaciones

#Predicciones ratings para UserBase_100 para calculo de coverage
rec_ubcf100_ratings_norm<- Recommender(getData(er2,"train"), method= "UBCF", param= list(nn=5))
pre_ub100_ratings_norm<-predict(rec_ubcf100_ratings_norm, matriz_rec_norm,getData(er2,"known"), n=100, type= "ratings") 

# Calculo Accuracy, Coverage, Novelty y Diversidad ------------------------------------

#ACCURACY
#Calcular el accuracy de UserBase_5 TOPN
plot(eval_topn_norm[["UBCF_5"]],"prec/rec")
avg(eval_topn_norm[["UBCF_5"]])

#Calcular el accuracy de UserBase_5 RATINGS
avg(eval_ratings_norm)

#COVERAGE
#Calcular el coverage de UserBase TOPN 
pre_ub100_topn_norm_matriz<-as(pre_ub100_topn_norm,"matrix")
df_pre_ub100_topn_norm<-as.data.frame(pre_ub100_topn_norm_matriz)
dim(df_pre_ub100_topn_norm) #3078   56


#user-coverage (usuario maximo de missings):
miss_case_summary(df_pre_ub100_topn_norm)
#(total de items - maximo de NAs)/total de items
user_coverage<-((56-49)/56)*100
user_coverage

#item-coverage (articulo maximo de missings):
miss_var_summary(df_pre_ub100_topn_norm) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((3078-2654)/3078)*100
item_coverage

#COVERAGE TOTAL TOPN NORMAL
n_miss(pre_ub100_topn_norm_matriz) #104467
dimensiones2<- 3078*56
coverage<- ((dimensiones2 - 104467)/dimensiones2)*100
coverage


#Calcular el coverage de UserBase RATINGS
pre_ub100_ratings_norm_matriz<-as(pre_ub100_ratings_norm,"matrix")
df_pre_ub100_ratings_norm<-as.data.frame(pre_ub100_ratings_norm_matriz)
dim(df_pre_ub100_ratings_norm) #3078   56


#user-coverage (usuario maximo de missings):
miss_case_summary(df_pre_ub100_ratings_norm)
#(total de items - maximo de NAs)/total de items
user_coverage<-((56-49)/56)*100
user_coverage

#item-coverage (articulo maximo de missings):
miss_var_summary(df_pre_ub100_ratings_norm) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((3078-2654)/3078)*100
item_coverage

#COVERAGE TOTAL RATINGS
n_miss(pre_ub100_ratings_norm_matriz) #104467
dimensiones2<- 3078*56
coverage<- ((dimensiones2 - 104467)/dimensiones2)*100
coverage


#NOVELTY
#Calcular el novelty de UserBase_5 TOPN y Ratings (al ser la misma matriz calculamos un novelty)

rec_ucbf5_norm_POPULAR <- Recommender(getData(er2, "train"), method="POPULAR")
pre_ubcf5_norm_POPULAR <- predict(rec_ucbf5_norm_POPULAR, getData(er2, "known"), type="topNList")
matriz_pre_ubcf5_norm_POPULAR<-as(pre_ubcf5_norm_POPULAR,"matrix")

rec_ucbf5_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_ubcf5_norm_POPULAR,2,f1))
quantile(apply(matriz_pre_ubcf5_norm_POPULAR,2,f1),0.75) 

vuelta_matriz_ubcf5normpopular<-t(matriz_pre_ubcf5_norm_POPULAR)

dim(vuelta_matriz_ubcf5normpopular)

itemsSeleccionados_ub5norm<-vuelta_matriz_ubcf5popular[apply(matriz_pre_ubcf5_norm_POPULAR,2,f1)>=2.5,] 
dim(itemsSeleccionados_ub5norm)
rownames(itemsSeleccionados_ub5norm)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_ub5norm),rec_ucbf5_norm_POPULAR@model[["topN"]]@itemLabels[1:14])

#coinciden 2 items de 14, entonces:
novelty<-(1-(2/14))*100
novelty
#NOVEDAD DEL 85.71% 

#DIVERSIDAD (una ya que son misma matriz) 

dim(pre_ub100_topn_norm_matriz) # 3078   56
df_pre_ub100_topn_norm

fucat2cat3<-dbGetQuery(conexion,"select fu.product_category_2_name,fu.product_category_3_name
from furniture as fu " )

#variedad en las predicciones en relacion a categoria3
int1<-data.frame(intersect(fucat2cat3$product_category_3_name,colnames(df_pre_ub100_topn_norm)))
colnames(int1)<-c("product_category_3_name")
int2<-merge(fucat2cat3,int1,by="product_category_3_name")
length(table(int2$product_category_2_name))

diversidad_topn_norm<- (9/56)*100
diversidad_topn_norm


# Analisis del usuario escogido al azar en compra normal -----------------------------------

#escogemos un usuario al azar (12)
as(pre_ub5_topn_norm, "list")[12]

# Elegir un usuario cualquiera para analizar lo que compra y lo que se le va a recomendar
usuario_12<-matriz_rec_normal[12,]

which(is.na(usuario_12)==F)
nombres<-c(which(is.na(usuario_12)==F))
colnames(usuario_12)[nombres] #Productos que compra usuario 12

#usuario 12 compra: "Add.on" "Cold.beverages" "Hot.snacks.Bistro"       
# "Salads" "Breakfasts.and.brunches" "Main.courses.and.buffets"
# "Hot.beverages"

#RECOMIENDA UBCF 5 TOPN
as(pre_ub5_topn_norm, "list")[12]

# al usuario 12 se le recomienda:
# [1] "Plants"            "Candles"           "Batteries."        "Towels.."         
# [5] "Boxes.and.baskets"

#RECOMIENDA UBCF5 RATINGS
as(pre_ub5_rating_norm, "list")[12]

