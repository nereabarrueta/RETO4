#PRUEBA 3
source("Scripts/librerias.R")
options(scipen=999)
library(readxl)
library(dplyr)
food<-read.csv("DatosOriginales/food.csv", header = TRUE, sep = ";")
food$store<- as.character(food$store)
food$product_category_3_name<- as.character(food$product_category_3_name)

furniture1<-read.csv("DatosOriginales/furniture_first.csv", header = TRUE, sep = ";")
furniture2<-read.csv("DatosOriginales/furniture_second.csv", header = TRUE, sep = ";")

furniture<-rbind(furniture1, furniture2)
furniture$store<- as.character(furniture$store)

furniture$product_category_2_name<- as.character(furniture$product_category_2_name)

furniture<- furniture %>%
  mutate(day= day(transaction_timestamp))

food<- food %>%
  mutate(day= day(transaction_timestamp))
food<-food[,-3] #quitar columna transaction timestamp
furniture<- furniture[,-3] #quitar columna transaction timestamp

#unificamos food y furniture
ikea<- merge(furniture, food, by= "membership_id")

#GRAFICOS CONJUNTOS ------------------------------------------------------

#Grafico de lineas ventas dia filtrado por tienda
#preparamos datos
ikea<- ikea %>%
  mutate(suma_ventas= apply(ikea[,c(8,18)],1,sum))

ikea_norte<- ikea %>%
  filter(store.x == "Norte" &
           store.y == "Norte")

ikea_sur<- ikea %>%
  filter(store.x == "Sur" &
           store.y == "Sur")

ggplot(ikea, aes(x= day.x , y =suma_ventas, group = 1)) +
  stat_summary(data = ikea_norte, fun = sum,size=1, geom = "line", aes(color = "Norte")) +
  stat_summary(data = ikea_sur, fun = sum,size=1, geom = "line", aes(color = "Sur")) +
  stat_summary(data = ikea, fun = sum,size=1, geom = "line", aes(color = "Suma")) +
  scale_x_continuous(limits = c(1,30), breaks = seq(1,30,1)) +
  scale_color_manual(values = c("blue", "darkmagenta", "yellow")) +
  labs(title = "Ventas totales en IKEA durante junio", x= "Dia del mes", 
       y= "Ventas totales", color = "Tienda")+ theme_minimal()

###

recuento2<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento2.csv")
recuentof<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento_food.csv")
library(shiny)
library(DT)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(EnvStats)
library(plotly)
library(wordcloud2)
library(writexl)
#install.packages("shinydashboard")
library("shinydashboard")
library(leaflet)
library("shiny")
library("shinyWidgets")
library("shiny")
library("shinyWidgets")
library(tibble)
library(odbc)
library(tidyverse)
library(naniar)
library(modeest)
library(colourpicker)





recuento2<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento2.csv" , sep= ";")


#GIRLS VS BOYS GARFICO DE TARTA

# gender<-read_excel("C:/Users/HP/Desktop/RETO4/VISU/Shiny/shinygender.xlsx")
# colnames(gender)<-c("Genero", "Puestos")
# plot_ly(gender, labels= ~Genero , values= ~Puestos , type = "pie")



# UI ----------------------------------------------------------------------

ui <- dashboardPage(skin="green",
                      
                      header = dashboardHeader(
                        title = "RETO 4: Colaboración con Ikea",
                        titleWidth = 350
                      ), 
                      
                      sidebar = dashboardSidebar(
                        width = 150,
                        sidebarMenu(
                          menuItem(text = "Contexto",  tabName = "tema2",icon = icon('glyphicon glyphicon-eur',lib="glyphicon")),
                          menuItem(text = "Food", tabName = "tema1", icon = icon('glyphicon glyphicon-cutlery',lib="glyphicon")), 
                          menuItem(text = "Furniture",  tabName = "tema3",icon = icon('bed'))
                        )
                        
                      ),

                        body = dashboardBody(
                          tabItems(
                           #CONTEXTO
                           tabItem(tabName="tema2" , 
                                   tabBox( width=15,height=2,
                                           tabPanel("Contexto",
                                                    fluidRow(
                                                      
                                                      column(6,
                                                             box(
                                                                title=  "¿Dondé se situan las tiendas de IKEA?",width= 15,solidHeader=TRUE, "En España se encuentran 30 establecimientos disponibles ")
                                                             
                                                      ),
                                                      
                                                        column(3,
                                                               pickerInput('sitio_select', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)

                                                        ),
                                                        column(3,
                                                               sliderInput('day_range', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 )
                                                               )
                                                      
                                                      ),
                                                      fluidRow( 
                                                        column(6,
                                                               leafletOutput( "mapa", height=315 )
                                                               ),
                                                       
                                                        column(6,
                                                               box(plotOutput("lineas_ventas"), width=200)
                                                               )
                                                      )
                                                       
                                                    
                                           ),
                                           tabPanel("Otros", 
                                                    
                                                     fluidRow(
                                                     column(6,
                                                            box(
                                                              title= "La igualdad de género es una realidad en Ikea", solidHeader=TRUE, ' "Queremos crear un mejor día a día para la mayoría de las personas. Por eso trabajamos duro para tener un impacto positivo en igualdad.    - Ikea " ', width=350 ,  height=115)
                                                              
                                                            ),
                                                     
                                                      column(6, 
                                                             box(
                                                               title = "Campaña 2021", solidHeader = TRUE, "El lema de la campaña: Fortune favours the frugal", width=350 , height=115)
                                                             )
                                                             
                                                             
                                                     ),
                                                   
                                                    
                                                    
                                                    fluidRow(
                                                      column(6, 
                                                             box(plotlyOutput("tarta_gender"), width=200, height=280)
                                                    ),
                                                    
                                                     column(6, 
                                                            HTML('<iframe width="450" height="315" src="https://www.youtube.com/embed/JPb4n8GdxKM" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                                                            ),
                                                    
                                                    ),
                                                    
                                                    
                                                    fluidRow(
                                                     column(12, 
                                                             infoBoxOutput("Número_de_empleados", width= 10) 
                                                      ),
                                                      column(12,
                                                             infoBoxOutput("Diversidad", width=6 )
                                                      )
                                                      
                                                    
                                                      
                                           
                                      )
                                   )
                                )
                           ), 
                           tabItem(tabName = "tema1", 
                                   tabBox(width = 15, height = 2,
                                          
                                          tabPanel("Evolución Ventas", 
                                                   fluidRow(
                                                     column(6, 
                                                            pickerInput("categoria3_select1" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                     ),
                                                     
                                                     
                                                     column( 2, 
                                                             pickerInput('sitio_select1', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                     ),
                                                     column( 4, 
                                                             sliderInput('day_range1', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 ) 
                                                             
                                                     )
                                                   
                                                   ),
                                                   
                                                   
                                                   
                                                   column(6,
                                                          fluidRow(
                                                            box(  title= "Veamos la distribución del precio y la cantidad", width=15, height =90,  solidHeader = T, background = "green"," Es interactivo") #,
                                                            
                                                            
                                                          ),
                                                          
                                                          fluidRow( 
                                                            box (plotlyOutput(outputId ='dispersion'), width = 15, solidHeader = TRUE)
                                                          )  
                                                          
                                                          
                                                   ),
                                                   
                                                   column(6,
                                                          box( title= "CANTIDAD POR PRODUCTO(S) ESGOGIDO(S)", width=15, solidHeader = TRUE, background = "black", "Prueba", 
                                                    plotlyOutput("boxplot_category" , width= 360 , height=420)
                                                            
                                                          )
                                                   )
                                                          
                                                       
                                                   
                                          ), 
                                          
                                          tabPanel("Distribución",
                                                   
                                                   fluidRow(
                                                     column(3,
                                                            box( title= "Veamos la distribución del precio", width=12, height =90,  solidHeader = T, background = "orange"," Es interactivo")
                                                     ),
                                                     
                                                     
                                                     column(5, 
                                                            pickerInput("categoria3_select2" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                            
                                                     ),
                                                     
                                                     column(3, 
                                                            pickerInput('sitio_select2', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                            
                                                     ),
                                                     
                                                     column(3 , 
                                                            sliderInput('day_range2', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1) 
                                                            
                                                     ),
                                                     
                                                     column(2 , 
                                                            numericInput("size", "Point size", 1, 1)
                                                     ),
                                                     
                                                     column(2, 
                                                            checkboxInput("fit", "Add line of best fit", FALSE)
                                                     ), 
                                                     
                                                     column(2, 
                                                            colourInput("color", "Point color", value = "blue")
                                                            
                                                     )
                                                     
                                                     
                                                     
                                                   ),
                                                   
                                                   
                                                   
                                                   fluidRow(
                                                     column(6,
                                                            box( 
                                                              title= "Datos:Cuantificando aquellos productos que escogas" , width= 12, solidHeader=TRUE, img(scr ="https://tbcdn.talentbrew.com/company/22908/v1_0/img/IKEA-opengraph.jpg"))
                                                           
                                                      
                                                     ), 
                                                     column(6,
                                                            box("Datos: TOP ventas" ,plotlyOutput('barras_top'),width=12, solidHeader= TRUE)
                                                       )
                                                   )
                                                   
                                                     
                                              
                                                     
                                                     
                                                   ),
                                                   
                                                   
                                        
                                          
                                          ###3
                                          tabPanel("Word Cloud",
                                                   
                                                   fluidRow(
                                                     column(5, 
                                                            box( "¿Cuáles son las palabras más empleadas?", width=12, solidHeader = TRUE,  "Veamos los productos más deseados/conocidos en IKEA")
                                                            
                                                     ),
                                                     
                                                     column(3,
                                                            sliderInput("freq","Minimum Frequency:",min = 1,  max = 50, value = 15)
                                                     ),
                                                     
                                                     column(2, 
                                                            colourInput("col", "Background color", value = "white")  
                                                     ),
                                                     
                                                     column(2,
                                                            downloadButton("downloadcloud","Download Cloud")
                                                     
                                                     )
                                                   ),
                                                   
                                                   fluidRow( 
                                                     column(10,
                                                            box(br(),"Explicación del wordcloud", br(), br(), br(), wordcloud2Output("cloud"), width = 12,heigth=600)
                                                     ), 
                                                     
                                                     column(2, 
                                                            fluidRow(h1()), 
                                                            fluidRow(h1()),
                                                            fluidRow(h1()),
                                                            fluidRow(  
                                                              box( "Como usuario tienes la posibilidad de...", width=12, solidHeader = TRUE,  "¡Animate y trastea!")
                                                              
                                                              
                                                            )
                                                     )
                                                     
                                                   ) 
                                                   
                                                   
                                          ),
                                          
                                          tabPanel( "Tablas" , 
                                                     fluidRow( 
                                                       column(5,
                                                              pickerInput("categoria3_select4" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                              
                                                       ),
                                                       
                                                       column(3,
                                                              pickerInput('sitio_select4', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                       ),
                                                       
                                                       column( 4, 
                                                               sliderInput('day_range4', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 ) 
                                                               
                                                       )
                                                     ),
                                                     
                                                     fluidRow( 
                                                       
                                                       column(8,
                                                              box(DTOutput('tabla_food'), width=12, solidHeader = TRUE)
                                                              
                                                       ),
                                                       
                                                       column(4,
                                                              fluidRow(
                                                                valueBox(value = "1468",subtitle = "Máxima cantidad vendida: 'Add on' ",icon = icon("food"), width=60) 
                                              
                                                              ),
                                                              fluidRow(
                                                                valueBox(value = "342",subtitle = "Segundo más vendido: 'Cold Beverages' ",icon = icon("food"), width=60, color="lime") 
                                                              ),
                                                              
                                                              fluidRow(
                                                                downloadButton("download_data","Download data" )
                                                                )
                                                              
                                                       )
                                                       
                                                     )
                                          )
                                          
                                   )
                              ), 
                           tabItem(tabName="tema3", 
                                   
                                   tabBox(width=15,height=2, 
                                          tabPanel("Idea General",
                                                   fluidRow(
                                                     
                                                     column(3, 
                                                            box( 
                                                              title="Evolución de las ventas medias de furniture", width=12, solidHeader=TRUE, "Veamos como ha variado durante con el trascurso del tiempo.¿Cuando han despegado las ventas?")
                                                     ),
                                                     
                                                     column(4, 
                                                            sliderInput('day_rangel', label='Selecciona el periodo temporal:', min= min(furniture$day), max= max(furniture$day), value = c(1,29), step=1)
                                                     ),
                                                     column(2, 
                                                            pickerInput('sitio_selectl', label = 'Selecciona la ubicacion:', choices=unique(furniture$store), selected = unique(furniture$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                            
                                                     ),
                                                     column(3, 
                                                            pickerInput('categoria2_selectl', label= 'Selecciona categoria_2 :', choices= unique(furniture$product_category_2_name),selected =c("Green decoration","Dining and serving", "Kitchen accessories", "Play"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                            
                                                     ),
                                                     
                                                   ),
                                                     
                                                     fluidRow(
                                                       
                                                       column(6, 
                                                              box( plotlyOutput('dispersion2', width= 300 )
                                                              )
                                                       ),
                                                       
                                                       column(6,
                                                              box( title= "CANTIDAD POR PRODUCTO(S) ESGOGIDO(S)", width=15, solidHeader = TRUE, background = "teal", "Prueba", 
                                                                   plotlyOutput("boxplot_category2" , width= 360 , height=420)
                                                       
                                                     )
                                                     
                                                       )
                                                     
                                                     
                                                   )
                                          ), 
                                          tabPanel("Top", 
                                                   fluidRow( 
                                                   
                                                   
                                                   column(4, 
                                                          sliderInput('day_ranget', label='Selecciona el periodo temporal:', min= min(furniture$day), max= max(furniture$day), value = c(1,29), step=1)
                                                   ),
                                                   column(2, 
                                                          pickerInput('sitio_selectt', label = 'Selecciona la ubicacion:', choices=unique(furniture$store), selected = unique(furniture$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                          
                                                   ),
                                                   column(3, 
                                                          pickerInput('categoria2_selectt', label= 'Selecciona categoria_2 :', choices= unique(furniture$product_category_2_name),selected =c("Green decoration","Dining and serving", "Kitchen accessories", "Play"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                          
                                                   )
                                                   
                                                   ), 
                                                   
                                                   fluidRow(
                                                   
                                                   column(12, box( "Datos: TOP " , plotlyOutput('Topf'), width=12, solidHeader= TRUE))
                                                   )
                                                  
                                                   
                                                   
                                                   
                                                   ), 
                                          
                                          
                                          tabPanel("Word Cloud", 
                                                    fluidRow(
                                                      column(6, 
                                                             box("¿Cuáles son las palabras más empleadas?", width=12, solidHeader = TRUE,  "Veamos los productos más deseados/conocidos en IKEA")
                                                             
                                                      ),
                                                      
                                                      column(2,
                                                             sliderInput("freqfur","Minimum Frequency:",min = 1,  max = 50, value = 15)
                                                      ),
                                                      
                                                      column(2, 
                                                             colourInput("colfur", "Background color", value = "white")  
                                                      ),
                                                      column(2,
                                                             downloadButton("downloadcloud2","Download Cloud")
                                                             
                                                      )
                                                       ),
                                                   
                                                   fluidRow(12, box( "WC", wordcloud2Output("wcf"), width=400)
                                                   
                                                     
                                                   )
                                            ),
                                          
                                          tabPanel("Tabla_furniture",
                                                   fluidRow(
                                                     column(6,box(DTOutput('tabla_furniture'))
                                                            ), 
                                                     column(3, pickerInput("categoria3_select4t" ,"Selecciona la categoría: ", choices= unique(furniture$product_category_2_name), selected =c("Quilts and pillows","Home decoration","Kitchen tools"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                                      
                                                     ),
                                                     
                                                     column(1,
                                                            pickerInput('sitio_select4t', label = 'Selecciona la ubicacion:', choices=unique(furniture$store), selected = unique(furniture$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                     ),
                                                     
                                                     column(2, 
                                                             sliderInput('day_range4t', label='Selecciona el periodo temporal:', min= min(furniture$day), max= max(furniture$day), value = c(1,29), step=1 ) 
                                                             
                                                     )
                                                   ),
                                                     
                                                     fluidRow(
                                                       downloadButton("download_data_fur","Download data" )
                                                     )
                                                            
                                                            
                                                            )
                                                            
                                                     
                                                     )
                                                   
                                          )
                                          
                                        )     
                                   )
                                   
                             )
                                   
                       
                    
  
                                          
                 
                  
                           
                      
                      






# SERVER ------------------------------------------------------------------

server <- function(input, output){  
  
  tabla00<-reactive({
    filter(food,
            product_category_3_name %in% input$categoria3_select)
  })

  
  output$mapa<- renderLeaflet({ 
    df_m<- read_excel("C:/Users/HP/Desktop/RETO4/VISU/Shiny/RETO_MAPA.xlsx")  
     leaflet(df_m)%>%
       addTiles() %>%
       addCircleMarkers(lng=~Longitud, lat=~Latidud, clusterOptions = markerClusterOptions())
     
   })
  
  output$tarta_gender<-renderPlotly({
    gender<-read_excel("C:/Users/HP/Desktop/RETO4/VISU/Shiny/shinygender.xlsx") 
    plot_ly(gender, labels= ~Genero , values= ~Puestos , type = "pie")
  }) 
  
  output$Número_de_empleados<-renderInfoBox({
    infoBox(title = "Número de empleados",
            value= "8945", 
            subtitle = "Dato de 2020", width=70 
            
            )
    
  })
    
    
  output$Diversidad<-renderInfoBox({
    infoBox(title = "Amplia diversidad ",
            value= "60", 
            subtitle = "Ikea suma 60 nacionalidades en su equipo", width=70 
             
    )
 
    
  })
  
  tablaikea<-reactive({
    filter(ikea,
            store.x %in% input$sitio_select &
            day.x >= input$day_range[1] &
            day.x <= input$day_range[2]

           )
    })
  
  tabla<-reactive({
    filter(food,
           day >= input$day_range[1] & 
             day <= input$day_range[2] &
             store %in% input$sitio_select &
             product_category_3_name %in% input$categoria3_select)
  })
  
  tabla.1<-reactive({
    filter(food,
           day >= input$day_range1[1] & 
             day <= input$day_range1[2] &
             store %in% input$sitio_select1 &
             product_category_3_name %in% input$categoria3_select1)
  })

  tabla4<-reactive({
    filter(food,
           day >= input$day_range4[1] & 
             day <= input$day_range4[2] &
             store %in% input$sitio_select4 &
             product_category_3_name %in% input$categoria3_select4)
  })
  
  output$lineas_ventas <- renderPlot({
    ggplot(tablaikea(),aes(x= day.x , color = store.x))+
      stat_count(geom='line')+ 
      scale_x_continuous(limits=c(input$day_range[1], input$day_range[2]), breaks=seq(input$day_range[1], input$day_range[2], 1))+
      labs(title = "Ventas totales en IKEA durante junio", x= "Dia del mes", y= "Ventas totales", color = "Tienda")+ 
      theme_minimal()
    
  })
    
  
  # output$point<-renderPlot({
  #   b<- ggplot(tabla00(), aes(sales, qty)) +
  #     geom_point(size = input$size, col=input$color) +
  #     scale_x_log10()
  #   if (input$fit) {
  #     b <- b + geom_smooth(method = "lm")
  #   }
  #   b
  # })
  

  
  output$dispersion<-renderPlotly(
    plot_ly(tabla.1(), x=~qty, y=~sales , type= "scatter", mode= "markers", color=~store)
  )
  
  
  output$cloud <- renderWordcloud2({
    recuento<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento_food.csv" , sep=";")
    wordcloud2(recuento, minSize = input$freq , backgroundColor = input$col )
  })
  
  output$downloadcloud<- downloadHandler(
    filename= function(){ 
      paste("wordcloudfood", "png", sep=".")
    },
    content = function(file){
      png(file)
      recuento<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento_food.csv", sep=";")
      wordcloud2(recuento, minSize = input$freq , backgroundColor = input$col )
      dev.off()
    }
  )
  
  output$boxplot_category<-renderPlotly({
    plot_ly(tabla.1(), x=~product_category_3_name, y= ~qty , type= "box", color=~product_category_3_name)
  })
  
  output$tabla_food<- renderDT({
    select(tabla4(),c("store","product_category_3_name","qty", "sales"))
  })
  
  output$download_data <- downloadHandler(
    filename = function(){
     paste("tablafood", "csv", sep=".") 
    },
    content = function(file){
      
      a <- select(tabla4(),c("store","product_category_3_name","qty", "sales"))
      write.csv(a(), file, row.names = FALSE)
    })

  tabla2<-reactive({
    filter(food,
           day >= input$day_range2[1] & 
             day <= input$day_range2[2] &
             store %in% input$sitio_select2 &
             product_category_3_name %in% input$categoria3_select2)%>%
      group_by(product_category_3_name)%>%
      summarise(cantidad_prod=sum(qty))
  })
  tabla3<-reactive({
    top_n(tabla2(),10)
  })
  
  output$barras_top<-renderPlotly({
    plot_ly(tabla3(), x=~product_category_3_name, y= ~cantidad_prod , type= "bar", color=~product_category_3_name)%>%
    layout(title = "Top 10 productos más vendidos en food",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""))

  })
  
  tabla0_<-reactive({
    filter(furniture,
           day >= input$day_range[1] & 
             day <= input$day_range[2] &
             store %in% input$sitio_select &
             product_category_2_name %in% input$categoria2_select)%>%
      group_by(day, store)%>%
      summarise(ventas_media=mean(sales))
  })
  
  tabla.2<-reactive({
    filter(furniture,
           day >= input$day_rangel[1] & 
             day <= input$day_rangel[2] &
             store %in% input$sitio_selectl &
             product_category_2_name %in% input$categoria2_selectl)
  })
  
  
  output$dispersion2<-renderPlotly(
    plot_ly(tabla.2(), x=~qty, y=~sales , type= "scatter", mode= "markers", color=~store)
  )
  
  
  output$boxplot_category2<-renderPlotly({
      plot_ly(tabla.2(), x=~product_category_2_name, y= ~qty , type= "box", color=~product_category_2_name)
    })
  

tablaT<-reactive({
  filter(furniture,
         day >= input$day_ranget[1] &
           day <= input$day_ranget[2] &
           store %in% input$sitio_selectt &
           product_category_2_name %in% input$categoria2_selectt)%>%
    group_by(product_category_2_name)%>%
    summarise(cantidad_prod=sum(qty))
})


tabla3.2<-reactive({
  top_n(tablaT(),10)
})

output$Topf<-renderPlotly({
  plot_ly(tabla3.2(), x=~product_category_2_name, y= ~cantidad_prod , type= "bar", color=~product_category_2_name)%>%
    layout(title = "Top 10 productos más vendidos en furniture",
           xaxis = list(title = "", tickangle = -45),
           yaxis = list(title = ""))
  
})

output$downloadcloud2<- downloadHandler(
  filename= "wordcloudfurniture.png",
  content = function(file){
    png(file)
    recuento2<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento2csv", sep=";")
    wordcloud2(recuento2, minSize = input$freqfur , backgroundColor = input$colfur )
    dev.off()
  }
)

  output$wcf <- renderWordcloud2({
    recuento2<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento2.csv", sep=";")
    wordcloud2(recuento2, minSize = input$freqfur , backgroundColor = input$colfur )
  })
  
  tabla4.2<-reactive({
    filter(furniture,
           day >= input$day_range4t[1] & 
             day <= input$day_range4t[2] &
             store %in% input$sitio_select4t &
             product_category_2_name %in% input$categoria3_select4t)
  })
  
  output$tabla_furniture<-renderDT({
      select(tabla4.2(),c("store","product_category_2_name","qty", "sales"))
    })
  
  output$download_data_fur<- downloadHandler(
    filename = function(){
      paste("tablafurniture", "csv", sep=".")
    },
    content = function(file){
      b<-select(tabla4.2(),c("store","product_category_3_name","qty", "sales"))
      write.csv(b(), file, row.names = FALSE)
    })
  
}


# ShinyApp -----------------------------------------------------------------------

shinyApp(ui = ui, server = server)
                                   




