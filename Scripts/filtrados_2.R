
# MEMBERSHIP ID COMO NOMBRE DE FILAS --------------------------------------
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
  for(j in 1:nrow(df_union)){
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
  #Se identifican dos formas de compras dentro del conjunto de datos
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
#Clientes que compran mucho, mas de 32 productos
#Clientes que compran normal, menos de 32 productos


# Filtrado matriz compra mucho ----------------------------------------------------

  #FILTRADO POR COLUMNAS
#solo vamos a filtrar por columnas por cantidad de NA
na_col_mucho <- compra_mucho %>%
  summarise(total_na= apply(compra_mucho[,-c(224,225)],2,n_miss)) #dejar fuera columnas total_na y total_cantidad

summary(na_col_mucho)
miss_var_summary<-miss_var_summary(compra_mucho)

#Filtramos por el primer cuartil (2557 missings) porque hay un porcentaje muy alto de NA por columna
compra_mucho<-compra_mucho[,colSums(is.na(compra_mucho))<=2557] 
#Pasamos de tener 2848 filas y 223 columnas (+total_na+total_cantidad)
#a tener 2848 filas y 57 columnas (+total_na+total_cantidad)

#Quitar columnas total_cantidad y total_na
compra_mucho<-compra_mucho[,-c(58,59)]

miss_case_summary(compra_mucho)

  #FILTRADO POR FILAS
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


# Filtrado matriz compra normal ----------------------------------------------------

#FILTRADO POR COLUMNAS
#solo vamos a filtrar por columnas por cantidad de NA
na_col_normal <- compra_normal %>%
  summarise(total_na= apply(compra_normal[,-c(224,225)],2,n_miss)) #dejar fuera columnas total_na y total_cantidad

summary(na_col_normal)
miss_var_summary(na_col_normal)

#Filtramos por el primer cuartil (8147 missings) porque hay un porcentaje muy alto de NA por columna
compra_normal<-compra_normal[,colSums(is.na(compra_normal))<=8147] 

#Pasamos de tener 8492 filas y 225 columnas (+total_na+total_cantidad)
#a tener 8492 filas y 56 columnas (+total_na+total_cantidad)

miss_case_summary(compra_normal[,-c(57,58)])

#FILTRADO POR FILAS
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
