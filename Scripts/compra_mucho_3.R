  #MATRIZ COMPRA MUCHO RECOMENDADOR
# Establecer mecanismo de evaluacion compra mucho --------------------------------------
set.seed(456)
matriz_recomendacion <- as.matrix(matriz_recomendacion_mucho)
matriz_recomendacion <- as(matriz_recomendacion,"realRatingMatrix")
matriz_recomendacion #1435 x 57 rating matrix of class 'realRatingMatrix' with 22614 ratings
as(matriz_recomendacion, "matrix")

getRatingMatrix(matriz_recomendacion)
as(matriz_recomendacion, "list") #Lista usuario a usuario

# Evaluar y predecir TOPN ------------------------------------------------------------

#Evaluar varios recomendadores a la vez topn: 
e <- evaluationScheme(matriz_recomendacion, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)
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
plot(eval_topn,"prec/rec")
plot(eval_coverage_topn)
getConfusionMatrix(eval_topn[["UBCF_5"]])


#El mejor modelo es UBCF, comparamos nn
algos <- list( "UBCF_5" = list(name = "UBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
               "UBCF_7" = list(name = "UBCF", param = list(nn = 7)), 
               "UBCF_10" = list(name = "UBCF", param = list(nn = 10)))

eval_topn <- evaluate(e, algos, type = "topNList",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas


#Graficar evaluaciones topn UBCF5 (hacemos definitivo abajo)
plot(eval_topn)
plot(eval_topn,"prec/rec")
getConfusionMatrix(eval_topn[["UBCF_5"]])
avg(eval_topn[["UBCF_5"]])

#predicciones TopN UserBase_5
rec_ucbf5_topn<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=5))
pre_ub5_topn<-predict(rec_ucbf5_topn, matriz_recomendacion,getData(e,"known"), n=5) #queremos cinco recomendaciones

#predicciones TopN UserBase_100
rec_ucbf100_topn<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=5))
pre_ub100_topn<-predict(rec_ucbf100_topn, matriz_recomendacion,getData(e,"known"), n=100) #queremos cinco recomendaciones


# Evaluar y predecir RATINGS ----------------------------------------------

#Evaluar varios recomendadores a la vez ratings: 
e <- evaluationScheme(matriz_recomendacion, method="split", train=0.75,
                      k=10, given=5, goodRating = 1)
e
algos <- list("random" = list(name = "RANDOM", param = NULL),
              "IBCF_7" = list(name = "IBCF"), #nn cuantos vecinos contribuyen al calculo de las predicciones
              "UBCF_7" = list(name = "UBCF", param = list(nn = 7)))


#Evaluar ratings
eval_ratings<- evaluate(e, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas
eval_coverage_ratings<-evaluate(e,algos,type = "ratings",n = c(1,3,5,10,15,20))

#Graficar evaluaciones ratings
plot(eval_ratings)

#El mejor modelo es UBCF (comprobamos n)
algos <- list( "UBCF_5" = list(name = "UBCF", param = list(nn = 5)), #nn cuantos vecinos contribuyen al calculo de las predicciones
               "UBCF_7" = list(name = "UBCF", param = list(nn = 7)), 
               "UBCF_10" = list(name = "UBCF", param = list(nn = 10)))

eval_ratings <- evaluate(e, algos, type = "ratings",n = c(1,3,5,10,15,20))#n es el numero de recomendaciones generadas

#Graficar evaluaciones ratings UBCF7 (hacemos el definitivo abajo)
plot(eval_ratings)
plot(eval_coverage_ratings)
getConfusionMatrix(eval_ratings[["UBCF_7"]])

#predicciones ratings UserBase_7
rec_ubcf7_ratings<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=7))
pre_ub7_ratings<-predict(rec_ubcf7_ratings, matriz_recomendacion,getData(e,"known"), n=5, type= "ratings") #queremos cinco recomendaciones

#predicciones ratings UserBase_100
rec_ubcf100_ratings<- Recommender(getData(e,"train"), method= "UBCF", param= list(nn=7))
pre_ub100_ratings<-predict(rec_ubcf100_ratings, matriz_recomendacion,getData(e,"known"), n=100, type= "ratings")

# Calculo Accuracy, Coverage y Novelty (compra mucho)------------------------------------

#ACCURACY
#Calcular el accuracy de UserBase_5 TOPN
plot(eval_topn[["UBCF_5"]],"prec/rec")

#Calcular el accuracy de UserBase_7 RATINGS
avg(eval_ratings[["UBCF_7"]])

#COVERAGE
#Calcular el coverage de UserBase TOPN (100 predicciones)
pre_ub100_topn_matriz<-as(pre_ub100_topn,"matrix")
df_pre_ub100_topn<-as.data.frame(pre_ub100_topn_matriz)
dim(df_pre_ub100_topn)#1435   57

#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pre_ub100_topn)
#(total de items - maximo de NAs)/total de items
user_coverage<-((57-48)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pre_ub100_topn) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((1435-1065)/1435)*100
item_coverage

#COVERAGE TOTAL TOPN
n_miss(pre_ub100_topn_matriz) #38704
dimensiones<- 1435*57
coverage<- ((dimensiones - 38704)/dimensiones)*100
coverage


#Calcular el coverage de UserBase RATINGS (100 predicciones)
pre_ub100_ratings_matriz<-as(pre_ub100_ratings,"matrix")
df_pre_ub100_ratings<-as.data.frame(pre_ub100_ratings_matriz)
dim(df_pre_ub100_ratings) #1435   57


#user-coverage (calculamos para el usuario con mas missings):
miss_case_summary(df_pre_ub100_ratings)
#(total de items - maximo de NAs)/total de items
user_coverage<-((57-42)/57)*100
user_coverage

#item-coverage (calculamos para el item con mas missings):
miss_var_summary(df_pre_ub100_ratings) 
#(total de usuarios - maximo de NAs )/total de usuarios
item_coverage<-((1435-1054)/1435)*100
item_coverage

#COVERAGE TOTAL RATINGS
n_miss(pre_ub100_ratings_matriz) #32964
dimensiones<- 1435*57
coverage<- ((dimensiones - 32964)/dimensiones)*100
coverage

#NOVELTY
#Calcular el novelty de UserBase_5 TOPN
rec_ucbf5_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_ubcf5_POPULAR <- predict(rec_ucbf5_POPULAR, getData(e, "known"), type="topNList")
matriz_pre_ubcf5_POPULAR<-as(pre_ubcf5_POPULAR,"matrix")

rec_ucbf5_POPULAR@model[["topN"]]@itemLabels[1:14]

#funcion para detectar valores que no son NA
f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_ubcf5_POPULAR,2,f1))
quantile(apply(matriz_pre_ubcf5_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

#entonces usamos el percentil 80 
quantile(apply(matriz_pre_ubcf5_POPULAR,2,f1),0.8)
vuelta_matriz_ubcf5popular<-t(matriz_pre_ubcf5_POPULAR)

dim(vuelta_matriz_ubcf5popular)

itemsSeleccionados_ub5<-vuelta_matriz_ubcf5popular[apply(matriz_pre_ubcf5_POPULAR,2,f1)>=67.4 ,] 
dim(itemsSeleccionados_ub5)
rownames(itemsSeleccionados_ub5)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_ub5),rec_ucbf5_POPULAR@model[["topN"]]@itemLabels[1:12])
#NOVEDAD DEL 100% 


#Calcular el novelty de UserBase_7 RATINGS
rec_ucbf7_POPULAR <- Recommender(getData(e, "train"), method="POPULAR")
pre_ubcf7_POPULAR <- predict(rec_ucbf7_POPULAR, getData(e, "known"), type="ratings")
matriz_pre_ubcf7_POPULAR<-as(pre_ubcf7_POPULAR,"matrix")

rec_ucbf7_POPULAR@model[["topN"]]@itemLabels[1:14]

f1<-function(x){
  return(sum(!is.na(x)))
}

#vamos a mirar cuales los productos mas frecuentemente sugeridos 
summary(apply(matriz_pre_ubcf7_POPULAR,2,f1))
quantile(apply(matriz_pre_ubcf7_POPULAR,2,f1),0.75) #los mayores del 3 cuartil son todos los items

vuelta_matriz_ubcf7popular<-t(matriz_pre_ubcf7_POPULAR)

dim(vuelta_matriz_ubcf7popular)

itemsSeleccionados_ub7<-vuelta_matriz_ubcf7popular[apply(matriz_pre_ubcf7_POPULAR,2,f1)>=340 ,] 
dim(itemsSeleccionados_ub7)
rownames(itemsSeleccionados_ub7)

#HACEMOS EL CRUCE
intersect(rownames(itemsSeleccionados_ub7),rec_ucbf7_POPULAR@model[["topN"]]@itemLabels[1:12])

#coinciden 5 items de 15, entonces:
novelty<-(1-(5/15))*100
novelty
#NOVEDAD DEL 66.66667%  

#DIVERSIDAD 
  #Diversidad TOPN
dim(pre_ub100_topn_matriz) #1435   57
df_pre_ub100_topn

fucat2cat3<-dbGetQuery(conexion,"select fu.product_category_2_name,fu.product_category_3_name
from furniture as fu " )

#variedad en las predicciones en relacion a categoria3
int1<-data.frame(intersect(fucat2cat3$product_category_3_name,colnames(df_pre_ub100_topn)))
colnames(int1)<-c("product_category_3_name")
int2<-merge(fucat2cat3,int1,by="product_category_3_name")
length(table(int2$product_category_2_name))

diversidad_topn<- (10/57)*100
diversidad_topn

  #Diversidad RATINGS
dim(pre_ub100_ratings_matriz) #1435   57
df_pre_ub100_ratings

int1<-data.frame(intersect(fucat2cat3$product_category_3_name,colnames(df_pre_ub100_ratings)))
colnames(int1)<-c("product_category_3_name")
int2<-merge(fucat2cat3,int1,by="product_category_3_name")
length(table(int2$product_category_2_name))

diversidad_ratings<- (10/57)*100
diversidad_ratings


# Analizar otro usuario al azar en compra mucho---------------------------------------------------
#con ratings, ub10
as(pre_ub5_topn, "list")[2]

# Analizamos lo que compra y lo que se le va a recomendar
usuario_2<-matriz_recomendacion_mucho[2,]

which(is.na(usuario_2)==F)
nombres<-c(which(is.na(usuario_2)==F))
colnames(usuario_2)[nombres] #Productos que compra usuario 2

#Con topN userbased(5) se le recomiendan:# Interior organisers, Kitchen organisers, Storage boxes, Pans, Mugs and cups
as(pre_ub5_topn, "list")[2]

#Con ratings userbased(7) se le recomiendan:# Interior organisers, Kitchen organisers, Mugs and cups, Storage boxes, Napkins
as(pre_ub7_ratings, "list")[2]
