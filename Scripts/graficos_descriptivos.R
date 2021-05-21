# Carga de datos y librerias----------------------------------------------------------
source("Scripts/librerias.R")
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


# Limpiar texto food -------------------------------------------------

#Quitar blancos innecesarios delante y detras de product category
food$product_category_1_name<-str_trim(food$product_category_1_name)
food$product_category_2_name<-str_trim(food$product_category_2_name)
food$product_category_3_name<-str_trim(food$product_category_3_name)



#Quitar puntuacion al final de product category
food<-as.data.frame(lapply(food, function(y) gsub("[[:punct:]]+$", "", y)))

#No hay tildes que molesten


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
#food$transaction_timestamp<-as.Date(food$transaction_timestamp)
food$till_id<-as.factor(food$till_id)
food$sales<- as.numeric(food$sales)
food$qty<-as.numeric(food$qty)
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

#Furniture

for(i in 1:4){
  x<-as.vector(unlist(furniture[,i]))
  furniture[,i]<-ifelse(x=="",0,x)
}

# Graficos descriptivos ---------------------------------------------------

  #Histogramas de variables numericas
grafico_hist<- function(col_name, t, x, c1, c2){
ggplot(food, aes(x=col_name)) + geom_histogram(boundary= 0, fill= c1, color = c2, alpha=0.5) +
    scale_x_continuous(limits=c(0,20), breaks=seq(0,20,2))+
    labs(title=t, x= x, y= "Frecuencia") + theme_classic() 
}

ventas<- grafico_hist(food$sales, "Ventas Comida", "Ventas", "darkorchid1", "darkorchid4")
cantidad<- grafico_hist(food$qty, "Cantidad Vendida Comida", "Cantidad", "gold", "gold3")
grid.arrange(ventas, cantidad, ncol= 2)

  #Lineas evolucion cantidad vendida durante el mes en ambas tiendas
  #preparar datos
food_qty<- food %>%
 mutate(trans_day= day(transaction_timestamp))

norte<- food_qty %>%
  filter(store== "Norte")

sur<- food_qty %>%
  filter(store== "Sur")

  #construimos gráfico
ggplot(food_qty, aes(x= trans_day, y =qty, group = 1)) +
  stat_summary(data = norte, fun = mean,size=1, geom = "line", aes(color = "Norte")) +
  stat_summary(data = sur, fun = mean,size=1, geom = "line", aes(color = "Sur")) +
  stat_summary(data = food_qty, fun = mean,size=1, geom = "line", aes(color = "Media")) +
  scale_x_continuous(limits = c(1,30), breaks = seq(1,30,3)) +
  scale_color_manual(values = c("darkmagenta", "blue", "yellow")) +
  labs(title = "Cantidad media vendida en comida durante junio", x= "Dia del mes", 
       y= "Cantidad Media", color = "Tienda")+ theme_calc()


# Matriz de correlaci?n ---------------------------------------------------
#FURNITURE
#Se crea un data-frame con las columnas num?ricas
col_numerica<-unlist(lapply(furniture,is.numeric))
df_numerica<-furniture[,unlist(lapply(furniture,is.numeric))]
#Se procede a dibujar la gr?fica 
corrplot(cor(df_numerica), type="upper", method="color", tl.cex=0.9)

#Se muestra claramente que entre "qty" y "sales" existe una relaci?n directa 
cor(furniture$qty, furniture$sales) #Un valor de +0.48 implica una relaci?n lineal ascendente 
#a cada magnitud de aumento de una de las variables corresponde a aumentar  el valor de la otra por 0.48.
#Se dibuja esa relaci?n:
plot(x= furniture$qty, y=furniture$sales , main= "Relacion entre cantidad y ventas en muebles" , xlab="cantidad", ylab="ventas")


#FOOD
#Se crea un data-frame con las columnas num?ricas
col_numerica2<-unlist(lapply(food,is.numeric))
df_numerica2<-food[,unlist(lapply(food,is.numeric))]
#Se procede a dibujar la gr?fica 
corrplot(cor(df_numerica2), type="upper", method="circle", tl.cex=0.9)

#Se muestra claramente que entre "qty" y "sales" existe una relaci?n directa 
cor(food$qty, food$sales) #Un valor de +0.77 implica una relaci?n lineal ascendente 
#a cada magnitud de aumento de una de las variables corresponde a aumentar  el valor de la otra por 0.77.
#Se dibuja esa relaci?n
plot(x= food$qty, y=food$sales , main= "Relacion entre cantidad y ventas en comida " , xlab="cantidad", ylab="ventas")

#Se puede ver un claro OUTLIER '!?!?!?!


# Tarta por categoria del archivo food ------------------------------------

#Preparando datos:

# conteo de variables no numericas ----------------------------------------
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
    geom_text(aes(label= round(porcentajes,2) ), position=position_stack(vjust=0.5),color="black",size=4)+
    coord_polar(theta = "y") + theme_pander() +
    labs(title=t , fill= n) 

}

names(dfconteo_c1) <- c("Nombre","porcentajes")
dfconteo_c1$porcentajes <- dfconteo_c1$porcentajes/sum(dfconteo_c1$porcentajes)

names(dfconteo_c2) <- c("Nombre","porcentajes")
dfconteo_c2$porcentajes <- dfconteo_c2$porcentajes/sum(dfconteo_c2$porcentajes)

names(dfconteo_c3) <- c("Nombre","porcentajes")
dfconteo_c3$porcentajes <- dfconteo_c3$porcentajes/sum(dfconteo_c3$porcentajes)


t(dfconteo_c1, "Niveles de la segunda categoria de food", "Niveles")
t(dfconteo_c2, "Niveles de la primera categoria de food", "Niveles")
t(dfconteo_c3, "Niveles de la tercera categoria de food", "Niveles")



# Nube de palabras furniture ----------------------------------------------

#extrayendo palabras
lista <- str_split(furniture$product_category_3_name , " ")
length(unlist(lista))  
recuento <- data.frame(table(as.factor(str_to_lower(unlist(lista)))))  
colnames(recuento)  
dim(recuento)  #[1] 270   2
head(arrange(recuento, desc(Freq)))

#Se decide eliminar palabras "vacias" de significado

corpus <- read_excel("./DatosTransformados/corpus.xlsx")
colnames(recuento)
recuento$Var1<- as.character(recuento$Var1)
colnames(corpus)<-"Var1"
corpus$Var1<- as.character(corpus$Var1)
recuento2<- anti_join(recuento, corpus, by="Var1")

head(arrange(recuento2, desc(Freq))) #Las palabras m?s frecuentes
arrange(recuento2, desc(Freq))  #todas las palabras ordenadas seg?n su frecuencia en orden descendiente

#Creamos el "wordcloud"

wordcloud2(recuento2,rotateRatio=0.1,minSize =15 ,shape = "pentagon",color= 'random-light' ,backgroundColor = "black",size=1.1)


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


  #Barras apiladas Norte/Sur Comida Vendida
ggplot(data=food, aes(x=till_id, y=sales, fill=store)) + 
  geom_bar(stat="identity") + labs(title = "Ventas en tiendas Norte y Sur", x= "Tipo de Caja", y= "Ventas en Comida",
                                   fill= "Tienda Ikea") +
  scale_fill_manual(values=c("blue", "yellow")) + theme_gdocs()

  #Histograma members socios y NO socios
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


# Grafico de lineas con semanas y dos lineas que sean Norte/sur de ventas furniture ----------------------------------------------------------------------

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


#

norte_f<- furniture_week %>%
  filter(store== "Norte")

sur_f<- furniture_week %>%
  filter(store== "Sur")

#generar gr?fico 

ggplot(furniture_week, aes(x=as.factor(week), y= sales, group= 1)) + 
  stat_summary(data=  norte_f,  fun= sum ,size=1.5, geom ="line", aes(color= "Norte"))+
  stat_summary(data= sur_f , fun=sum ,size=1.5, geom= "line",aes(color= "Sur"))+ 
  labs(title="Ventas por semana en muebles" , x= "Semana", y="Cantidad vendida ", color= "Tienda")+
  scale_color_manual(values = c( "darkorchid1", "darkorchid4" ))+
  theme_classic()+
  theme(panel.grid.major = element_line(color="grey", linetype="dotted"))






# Intento shiny 1 ---------------------------------------------------------
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(DT)
library(shinyWidgets)
library(shinythemes)
library(leaflet)

df<-furniture

df<-furniture[1:1000,] #para hacer mas pequeño el dataset a ver si asi carga pero no :(
df$transaction_day <- substr(df$transaction_timestamp, 1, 10)
df$transaction_hour <- substr(df$transaction_timestamp, 12,16)

df$transaction_day<-as.Date(df$transaction_day)
str(df)

#eliminar columna timestamp
df<-select(df, - transaction_timestamp)
str(df)

ui <- fluidPage(
  titlePanel("Top 10 categorías vendidas"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(inputId = 'date_range', label = 'Selecciona el periodo temporal', min=min(df$transaction_day), max=max(df$transaction_day), start="2019-06-01", end="2019-06-29"), 
      pickerInput("tienda_select", label= "Selecciona la tienda Ikea: ", choices = as.character(unique(df$store)), selected = "Norte")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Categoria', plotlyOutput('barra'))
      )
    )
  )
)


server<-function(input, output){
  tabla<-reactive({
    tabla<-filter(df, 
                  transaction_day >= input$date_range[1] & 
                  transaction_day <= input$date_range[2] )%>%
                  group_by(product_category_1_name)%>%
                  summarise(cantidad=sum(qty))
  })
  

    output$barra <- renderPlot({
      ggplot(top_n(tabla(),10), aes(product_category_1_name, cantidad))+
        theme_bw()+
        theme(axis.text.x = element_text(face= 'bold', size=12),
              axis.text.y = element_text(face= 'bold', size=12),
              axis.title.x = element_blank())


})
}


shinyApp(ui = ui, server = server)


