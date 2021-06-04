
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


# Estadisticos generales------------------------------------------------------------

#1. Productos comprados por cada cliente (todos)
range(datos_totales$total_cantidad)
hist(datos_totales$total_cantidad, main = "Cantidad de productos comprados por clientes",
     breaks = 50, xlim = c(0, 520), col = "purple", xlab="Total cantidad", ylab="Numero de clientes")

#hacemos otro histograma solo con los clientes que menos que compran

#2. Productos no comprados por cada cliente
range(datos_totales$total_na)
hist(datos_totales$total_na, main = "Cantidad de productos no comprados por clientes",
     breaks = 50, xlim = c(150, 225), col = "purple", xlab="Total cantidad no comprada", ylab="Numero de clientes")

#3. Cantidad de veces que se han comprado los productos
total_productos<-colSums(datos_totales[, -c(224, 225)], na.rm = TRUE)
total_productos<-as.data.frame(total_productos)
range(total_productos)

max(total_productos$total_productos)
which.max(total_productos$total_productos)
rownames(total_productos)[23] #Hot beverages

hist(total_productos$total_productos, main = "Cantidad de productos comprados",
     breaks = 50, xlim = c(0, 12800), col = "purple", xlab="Numero de veces comprado", ylab="Cantidad de productos")
text(x=12500,y=4.5, labels="Hot beverages", cex=0.7)


#4. Cantidad de veces que NO se han comprado los productos
total_na_productos <- datos_totales %>%
  summarise(total_na= apply(datos_totales[, -c(224, 225)],2,n_miss))
range(total_na_productos$total_na)

hist(total_na_productos$total_na, main = "Cantidad total de productos no comprados",
     breaks = 50, xlim =c(4600, 11400), col = "purple", xlab="Numero de veces no comprado", ylab="Numero de productos")


# Estadisticos compra mucho -----------------------------------------------

#5. y 6. Clientes que compran mucho cantidad
total_compramucho<-rowSums(compra_mucho[, -c(224,225)], na.rm = TRUE)
total_compramucho<-as.data.frame(total_compramucho)
str(total_compramucho)
range(total_compramucho)

hist(total_compramucho$total_compramucho, main = "Total de compras de clientes que compran mucho", 
     breaks = 25, xlim = c(0, 520), col = "red", xlab="Cantidad total comprada", ylab="Numero de clientes")


#7. y 8. Cantidad y media vendida por producto
miss_var_summary(compra_mucho)

total_compramucho_col<-colSums(compra_mucho[, -c(224, 225)], na.rm = TRUE)
total_compramucho_col<-as.data.frame(total_compramucho_col)
range(total_compramucho_col)

media_compramucho_col<-colMeans(compra_mucho[, -c(208, 214, 218, 219, 221, 222, 224, 225)], na.rm = TRUE) #quitar columnas con 100 de NA
media_compramucho_col<-as.data.frame(media_compramucho_col)
range(media_compramucho_col)


which.max(total_compramucho_col$total_compramucho_col)
rownames(total_compramucho_col)[104] #Interior organisers

hist(total_compramucho_col$total_compramucho_col, main = "Cantidad vendida por producto, clientes que compran mucho", 
     breaks = 35, xlim = c(0, 9100), col = "red", xlab="Cantidad vendida por producto", ylab="Numero de productos")
text(x=9000,y=4.5, labels="Interior organisers", cex=0.7)


hist(media_compramucho_col$media_compramucho_col, main = "Media compras por producto, clientes que compran mucho",
     breaks = 40, xlim = c(0, 30), ylim = c(0, 70), col = "red", xlab="Media vendida por producto", ylab="Numero de productos")
text(x=26.8368,y=4.5, labels="Interior organisers", cex=0.7)


# Estadisticos compra normal ----------------------------------------------

# 9. y 10. Clientes que compran normal cantidad
total_compranormal<-rowSums(compra_normal[, -c(224,225)], na.rm = TRUE)
total_compranormal<-as.data.frame(total_compranormal)
str(total_compranormal)
range(total_compranormal)

hist(total_compranormal$total_compranormal, main = "Total de compras de clientes que compran normal", 
     breaks = 35, xlim = c(0, 35), col = "blue", xlab="Cantidad total comprada", ylab="Numero de clientes")


