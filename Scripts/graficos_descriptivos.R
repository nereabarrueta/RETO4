# Carga de datos y librerias----------------------------------------------------------
source("Scripts/librerias.R")

  #eliminamos la notacion cientifica  
options(scipen=999)

furniture_first <- read.csv("DatosOriginales/furniture_first.csv",sep=";")
furniture_second <- read.csv("DatosOriginales/furniture_second.csv",sep=";")
food <- read.csv("DatosOriginales/food.csv",sep=";")
furniture<-rbind(furniture_first,furniture_second)

# Familiarizarse con los datos --------------------------------------------
str(food)
str(furniture)

# Limpiar texto furniture -------------------------------------------------

#Quitar blancos innecesarios delante y detras de product category
furniture$product_category_1_name<-str_trim(furniture$product_category_1_name)
furniture$product_category_2_name<-str_trim(furniture$product_category_2_name)
furniture$product_category_3_name<-str_trim(furniture$product_category_3_name)


# Tipo de datos (furniture) -----------------------------------------------
furniture$sales<- as.numeric(furniture$sales)
furniture$qty<-as.integer(furniture$qty)
furniture$product_category_1_name<-as.factor(furniture$product_category_1_name)
furniture$product_category_2_name<-as.factor(furniture$product_category_2_name)
furniture$product_category_3_name<-as.factor(furniture$product_category_3_name)



levels(furniture$product_category_1_name)
nlevels(furniture$product_category_1_name) #22 items
levels(furniture$product_category_2_name) 
nlevels(furniture$product_category_2_name) #72 items
levels(furniture$product_category_3_name)
nlevels(furniture$product_category_3_name) #210 items



# Tipo de datos (food)-----------------------------------------------------------

food$store<-as.factor(food$store)
food$till_id<-as.factor(food$till_id)

food$qty<-as.numeric(food$qty)
food$sales<- as.numeric(food$sales)

food$product_category_1_name<-as.factor(food$product_category_1_name)
food$product_category_2_name<-as.factor(food$product_category_2_name)
food$product_category_3_name<-as.factor(food$product_category_3_name)


levels(food$till_id)
levels(food$product_category_1_name)
nlevels(food$product_category_1_name) #1 item
levels(food$product_category_2_name)  
nlevels(food$product_category_2_name) #4 items
levels(food$product_category_3_name)
nlevels(food$product_category_3_name) #25 items

#comprobacion de nuevos tipos
str(food)
str(furniture)

# Membership vacio imputar por el valor 0------------------------------------------

#Food
for(i in 1:4){
  x<-as.vector(unlist(food[,i]))
  food[,i]<-ifelse(x=="",0,x)
}

#Furniture - tiene todas las instancias con codigo hash
for(i in 1:4){
  x<-as.vector(unlist(furniture[,i]))
  furniture[,i]<-ifelse(x=="",0,x)
}

# Graficos descriptivos ---------------------------------------------------


# Histogramas de variables numericas --------------------------------------
grafico_hist<- function(col_name, t, x, c1, c2){
ggplot(food, aes(x=col_name)) + geom_histogram(boundary= 0, fill= c1, color = c2, alpha=0.5) +
    scale_x_continuous(limits=c(0,20), breaks=seq(0,20,2))+
    labs(title=t, x= x, y= "Frecuencia") + theme_classic() 
}

ventas<- grafico_hist(food$sales, "Ventas Comida", "Ventas", "darkorchid1", "darkorchid4")
cantidad<- grafico_hist(food$qty, "Cantidad Vendida Comida", "Cantidad", "gold", "gold3")
grid.arrange(ventas, cantidad, ncol= 2)

  #pasamos a plotly
ventasly<- ggplotly(ventas)
cantidadly<- ggplotly(cantidad)
subplot(ventasly,cantidadly, nrows = 1) #titulos


# Lineas evolucion cantidad vendida durante el mes en ambas tiendas -------
  #preparar datos
food_qty<- food %>%
 mutate(trans_day= day(transaction_timestamp))

norte<- food_qty %>%
  filter(store== "Norte")

sur<- food_qty %>%
  filter(store== "Sur")

  #construimos grafico
lineas<-ggplot(food_qty, aes(x= trans_day, y =qty, group = 1)) +
  stat_summary(data = norte, fun = mean,size=1, geom = "line", aes(color = "Norte")) +
  stat_summary(data = sur, fun = mean,size=1, geom = "line", aes(color = "Sur")) +
  stat_summary(data = food_qty, fun = mean,size=1, geom = "line", aes(color = "Media")) +
  scale_x_continuous(limits = c(1,30), breaks = seq(1,30,3)) +
  scale_color_manual(values = c("darkmagenta", "blue", "yellow")) +
  labs(title = "Cantidad media vendida en comida durante junio", x= "Dia del mes", 
       y= "Cantidad Media", color = "Tienda")+ theme_calc()
lineas

  #pasamos a plotly
lineasly<- ggplotly(lineas)
lineasly

# Matriz de correlacion ---------------------------------------------------
  #FURNITURE
#Se crea un data-frame con las columnas numericas
col_numerica<-unlist(lapply(furniture,is.numeric))
df_numerica<-furniture[,unlist(lapply(furniture,is.numeric))]

#Se procede a dibujar la grafica 
corrplot(cor(df_numerica), type="upper", method="color", tl.cex=0.9)

#Se muestra claramente que entre "qty" y "sales" existe una relacion directa 
cor(furniture$qty, furniture$sales) #Un valor de +0.48 implica una relacion lineal ascendente 
#a cada magnitud de aumento de una de las variables corresponde a aumentar  el valor de la otra por 0.48.
#Se dibuja esa relacion:
plot(x= furniture$qty, y=furniture$sales , main= "Relacion entre cantidad y ventas en muebles" , xlab="cantidad", ylab="ventas")


  #FOOD
#Se crea un data-frame con las columnas numericas
col_numerica2<-unlist(lapply(food,is.numeric))
df_numerica2<-food[,unlist(lapply(food,is.numeric))]
#Se procede a dibujar la grafica 
corrplot(cor(df_numerica2), type="upper", method="circle", tl.cex=0.8)

#Se muestra claramente que entre "qty" y "sales" existe una relacion directa 
cor(food$qty, food$sales) #Un valor de +0.77 implica una relacion lineal ascendente 
#a cada magnitud de aumento de una de las variables corresponde a aumentar  el valor de la otra por 0.77.
#Se dibuja esa relacion
plot(x= food$qty, y=food$sales , main= "Relacion entre cantidad y ventas en comida " , xlab="cantidad", ylab="ventas")

#Se puede ver un claro OUTLIER '!?!?!?!


# Tarta por categoria del archivo food ------------------------------------
  #Preparando datos:

#Conteo de variables no numericas
dfconteo_c1<-food%>%
  dplyr::count(product_category_1_name)
dfconteo_c2<-food%>%
  dplyr::count(product_category_2_name)
dfconteo_c3<-food%>%
  dplyr::count(product_category_3_name)
dfconteo_store<-food%>%
  dplyr::count(store)


t<-function(df,t, n){ 
  ggplot(df,aes(x="",y=porcentajes, fill=Nombre))+
    geom_bar(stat = "identity", color="white")+
    geom_text(aes(label= round(porcentajes,2)*100), position=position_stack(vjust=0.5),color="black",size=4)+
    coord_polar(theta = "y") + theme_void() + 
    labs(title=t , fill= n) +  theme(legend.text=element_text(size=10))
  
}


  #aplicamos la funcion
names(dfconteo_c1) <- c("Nombre","porcentajes")
dfconteo_c1$porcentajes <- dfconteo_c1$porcentajes/sum(dfconteo_c1$porcentajes)

names(dfconteo_c2) <- c("Nombre","porcentajes")
dfconteo_c2$porcentajes <- dfconteo_c2$porcentajes/sum(dfconteo_c2$porcentajes)

names(dfconteo_c3) <- c("Nombre","porcentajes")
dfconteo_c3$porcentajes <- dfconteo_c3$porcentajes/sum(dfconteo_c3$porcentajes)


tarta1<- t(dfconteo_c1, "Niveles de la segunda categoria de food", "Niveles")
tarta2<- t(dfconteo_c2, "Niveles de la primera categoria de food", "Niveles")
tarta3<- t(dfconteo_c3, "Niveles de la tercera categoria de food", "Niveles")


  #unificamos los dos primeros apartando el tercero debido a su tamano
grid.arrange(tarta1, tarta2, ncol=2)
tarta3


# Nube de palabras furniture ----------------------------------------------

#extrayendo palabras
lista <- str_split(furniture$product_category_3_name , " ")
length(unlist(lista))  
recuento <- data.frame(table(as.factor(str_to_lower(unlist(lista)))))  
colnames(recuento)  
dim(recuento)  #[1] 270   2
head(arrange(recuento, desc(Freq)))


#Se decide eliminar palabras "vacias" de significado
corpus <- read_excel("DatosTransformados/corpus.xlsx")
colnames(recuento)
recuento$Var1<- as.character(recuento$Var1)
colnames(corpus)<-"Var1"
corpus$Var1<- as.character(corpus$Var1)
recuento2<- anti_join(recuento, corpus, by="Var1")

head(arrange(recuento2, desc(Freq))) #Las palabras mas frecuentes
arrange(recuento2, desc(Freq))  #todas las palabras ordenadas segun su frecuencia en orden descendiente

#Creamos el "wordcloud"
nube<-wordcloud2(recuento2,rotateRatio=0.1,minSize =15 ,shape = "pentagon",color= 'random-light' ,backgroundColor = "black",size=1.1)
nube

#descargamos el wordcloud
saveWidget(nube,"tmp.html",selfcontained = F)
webshot("tmp.html","fig_1.png", delay =5, vwidth = 480, vheight=480)

# Boxplot de cada semana Ventas y Cantidad (Food)-----------------------------------------------------------------

#Preparar datos
  #semana 1
w1<- filter(food, day(food$transaction_timestamp)>=1 & day(food$transaction_timestamp)<=9)
w1<- w1 %>%
  mutate(week= "Primera semana")
w1$week<-as.factor(w1$week)

  #semana 2
w2<- filter(food, day(food$transaction_timestamp)>=10 & day(food$transaction_timestamp)<=16)
w2<- w2 %>%
  mutate(week= "Segunda semana")
w2$week<-as.factor(w2$week)

  #semana 3
w3<- filter(food, day(food$transaction_timestamp)>=17 & day(food$transaction_timestamp)<=23)
w3<- w3 %>%
  mutate(week= "Tercera semana")
w3$week<-as.factor(w3$week)

  #semana 4
w4<- filter(food, day(food$transaction_timestamp)>=24 & day(food$transaction_timestamp)<=30)
w4<- w4 %>%
  mutate(week= "Cuarta semana")
w4$week<-as.factor(w4$week)

  #unificar cuatro df
food_week<- rbind(w1,w2,w3,w4)

#generar boxplots
grafico_bp<- function(col_name, t,x, y, c1, c2){
  ggplot(food_week, aes(x=as.factor(week), y=col_name)) + geom_boxplot(fill = c1, color = c2, alpha = 0.5)+
    labs(title=t, x=x, y=y) + theme_minimal() }

bp_ventas<- grafico_bp(food_week$sales, "Ventas Por Semana", "Semana", "Ventas Comida", "darkorchid1", "darkorchid4")
bp_cantidad<- grafico_bp(food_week$qty, "Cantidad Vendida Por Semana", "Semana", "Cantidad Comida","gold", "gold3" )
grid.arrange(bp_ventas, bp_cantidad, ncol= 2)


#Realizamos zoom sobre los boxplots
grafico_bp_zoom<- function(col_name, t,x, y, c1, c2){
  ggplot(food_week, aes(x=as.factor(week), y=col_name)) + geom_boxplot(fill = c1, color = c2, alpha = 0.5)+
    labs(title=t, x=x, y=y) + theme_minimal() + coord_cartesian(ylim=c(0,80))}

bp_ventas_zoom<- grafico_bp_zoom(food_week$sales, "Ventas Por Semana (Zoom)", "Semana", "Ventas Comida", "darkorchid1", "darkorchid4")
bp_cantidad_zoom<- grafico_bp_zoom(food_week$qty, "Cantidad Vendida Por Semana (Zoom)", "Semana", "Cantidad Comida","gold", "gold3" )

grid.arrange(bp_ventas_zoom, bp_cantidad_zoom, ncol= 2)

#unificamos los cuatro boxplot para observar comparativa
grid.arrange(bp_ventas, bp_cantidad, bp_ventas_zoom, bp_cantidad_zoom, ncol=2, nrow=2)

  #pasamos a plotly
bp_ventasly<- ggplotly(bp_ventas)
bp_cantidadly<- ggplotly(bp_cantidad)
bp_ventas_zoom<- ggplotly(bp_ventas_zoom)
bp_cantidad_zoom<- ggplotly(bp_cantidad_zoom)
subplot(bp_ventasly,bp_cantidadly, bp_ventas_zoom, bp_cantidad_zoom, nrows = 2) #titulos



# Barras apiladas Norte/Sur Comida Vendida --------------------------------

ggplot(data=food, aes(x=till_id, y=sales, fill=store)) + 
  geom_bar(stat="identity") + labs(title = "Ventas en tiendas Norte y Sur", x= "Tipo de Caja", y= "Ventas en Comida",
                                   fill= "Tienda Ikea") +
  scale_fill_manual(values=c("blue", "gold2")) + theme_gdocs()


# Histograma members socios y NO socios -----------------------------------

food_socios<- food[food$membership_id !=0,]
food_no_socios<- food[food$membership_id ==0,]

grafico_socios<- function(df, col_name, t, x, c1, c2){
  ggplot(df, aes(x=col_name)) + geom_histogram(boundary= 0, fill= c1, color = c2, alpha=0.5) +
    labs(title=t, x= x, y="Frecuencia") + theme_hc() + facet_grid(. ~ store)+
    scale_x_continuous( limit= c(0,100))+ coord_cartesian(xlim=c(0,40))
}

hist_nos<- grafico_socios(food_no_socios, food_no_socios$sales, "Ventas a No Socios", "Ventas en Comida", "turquoise1", "turquoise3")
hist_s<- grafico_socios(food_socios, food_socios$sales, "Ventas a Socios", "Ventas en Comida", "violetred1", "violetred3")

grid.arrange(hist_nos, hist_s, ncol= 2)

  #pasamos a plotly
hist_nosly<- ggplotly(hist_nos)
hist_sly<- ggplotly(hist_s)
subplot(hist_nosly,hist_sly, nrows = 1) #titulos


#Grafico de lineas por semana Norte/sur de ventas furniture ----------------------------------------------------------------------

#Preparar datos
#semana 1
w1.1<- filter(furniture, day(furniture$transaction_timestamp)>=1 & day(furniture$transaction_timestamp)<=9)
w1.1<- w1.1 %>%
  mutate(week= "Primera semana")
w1.1$week<-as.factor(w1.1$week)

#semana 2
w2.2<- filter(furniture, day(furniture$transaction_timestamp)>=10 & day(furniture$transaction_timestamp)<=16)
w2.2<- w2.2 %>%
  mutate(week= "Segunda semana")
w2.2$week<-as.factor(w2.2$week)

#semana 3
w3.3<- filter(furniture, day(furniture$transaction_timestamp)>=17 & day(furniture$transaction_timestamp)<=23)
w3.3<- w3.3 %>%
  mutate(week= "Tercera semana")
w3.3$week<-as.factor(w3.3$week)

#semana 4
w4.4<- filter(furniture, day(furniture$transaction_timestamp)>=24 & day(furniture$transaction_timestamp)<=30)
w4.4<- w4.4 %>%
  mutate(week= "Cuarta semana")
w4.4$week<-as.factor(w4.4$week)

#unificar cuatro df
furniture_week<- rbind(w1.1,w2.2,w3.3,w4.4)

#discernir norte/sur
norte_f<- furniture_week %>%
  filter(store== "Norte")

sur_f<- furniture_week %>%
  filter(store== "Sur")

#generar grafico 
lineas2<- ggplot(furniture_week, aes(x=as.factor(week), y= sales, group= 1)) + 
  stat_summary(data=  norte_f,  fun= sum ,size=1, geom ="line", aes(color= "Norte"))+
  stat_summary(data= sur_f , fun=sum ,size=1, geom= "line",aes(color= "Sur"))+ 
  labs(title="Ventas por semana en muebles" , x= "Semana", y="Cantidad vendida ", color= "Tienda")+
  scale_color_manual(values = c( "blue", "gold2" ))+
  theme_classic()+
  theme(panel.grid.major = element_line(color="grey", linetype="dotted"))

  #pasamos a plotly
lineas2ly<- ggplotly(lineas2)
lineas2ly
