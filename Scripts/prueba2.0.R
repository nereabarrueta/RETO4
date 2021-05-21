source("Scripts/librerias.R")
options(scipen=999)
furniture_first <- read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosOriginales/furniture_first.csv",sep=";")
furniture_second <- read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosOriginales/furniture_second.csv",sep=";")
food <- read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosOriginales/food.csv",sep=";")
furniture<-rbind(furniture_first,furniture_second)

food$transaction_day <- substr(food$transaction_timestamp, 1, 10)
furniture$transaction_day <- substr(food$transaction_timestamp, 1, 10)
furniture$transaction_hour <- substr(furniture$transaction_timestamp, 12,16)
food$transaction_hour <- substr(food$transaction_timestamp, 1, 10)

recuento2<-read.csv("C:/Users/HP/Desktop/RETO4/WORK/DatosTransformados/recuento2.csv")

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



#CASI BIEN



ui <- dashboardPage ( skin="yellow",
                      
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
                          tabItem(tabName = "tema1", 
                                  tabBox(width = 15, height = 2 ,
                                         
                                         tabPanel("Evolución Ventas", 
                                         fluidRow(
                                           column(6,pickerInput("categoria3_select" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                           ),
                                           
                                           
                                           column( 3,pickerInput('sitio_select', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                           ),
                                           column( 3, sliderInput('day_range', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 ) 
                                                   
                                           )
                                           
                                           
                                           
                                           
                                         ),
                                         
                                         
                                         
                                         column(6,
                                                fluidRow(
                                                  box(  title= "Veamos la distribución del precio y la cantidad", width=15, height =90,  solidHeader = T, background = "orange"," Es interactivo"),
                                                  
                                                  
                                                ),
                                                
                                                fluidRow( 
                                                  box (plotOutput(outputId = 'dispersion'), width = 15, solidHeader = TRUE)
                                                )  
                                                
                                                
                                         ),
                                         column(6, 
                                                fluidRow( 
                                                  box( title= "Ventas por producto", width=12, plotOutput(outputId = "lineas" , height = 460))
                                                  
                                                )
                                                
                                         )    
                                         
                                  ), 
                                  
                                  tabPanel("Distribución",
                                           
                                           fluidRow(
                                             column(3,
                                                    box( title= "Veamos la distribución del precio", width=12, height =90,  solidHeader = T, background = "orange"," Es interactivo")
                                             ),
                                           
                                           
                                           column(4, 
                                                  pickerInput("categoria3_select" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                 
                                                   ),
                                           
                                           column(2, 
                                                  pickerInput('sitio_select', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                           
                                                  ),
                                           
                                           column(3 , 
                                                  sliderInput('day_range', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 ) 
                                                   
                                           )
                                                  
                                          ),
                                          
                                        
                                                  
                                              fluidRow(
                                                column(6,
                                                box( 
                                                  title= "Datos:Cuantificando aquellos productos que escogas" , plotlyOutput("boxplot_category"), width= 12, solidHeader=TRUE)
                                                
                                                ),
                                                
                                                column(6,
                                                       box("Datos: TOP ventas"),plotlyOutput('barras_top'),
                                                       
                                                       
                                                        width=12, solidHeader= TRUE)
                                                      
                                                
                                                    )
                                                ),
                                  
                                          
                                              
                                            
                                           ###3
                                           tabPanel("Word CLoud",
                                                    
                                                    fluidRow(
                                                      column(5, 
                                                             box( "¿Cuáles son las palabras más empleadas?", width=12, solidHeader = TRUE,  "Veamos los productos más deseados/conocidos en IKEA")
                                                      
                                                      ),
                                                      
                                                      column(3,
                                                             sliderInput("freq","Minimum Frequency:",min = 1,  max = 50, value = 15)
                                                      ),
                                                                  
                                                      column(4, 
                                                             sliderInput("max","Maximum Number of Words:",min = 1,  max = 300,  value = 100) 
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
                                                             
                                                    tabPanel ( "Tablas" , 
                                                               fluidRow( 
                                                                 column(5,
                                                                        pickerInput("categoria3_select" ,"Selecciona la categoría: ", choices= unique(food$product_category_3_name), selected =c("Hot snacks Bistro","Sauces, jam & condiments", "Add-on Bistro", "Sweets, chocolate & snacks"), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                                        
                                                                        ),
                                                                 
                                                                 column(3,
                                                                      pickerInput('sitio_select', label = 'Selecciona la ubicacion:', choices=unique(food$store), selected = unique(food$store), options= list('actions-box'=TRUE, 'deselectAllText'=FALSE),  multiple = TRUE)
                                                                        ),
                                                                 
                                                                column( 4, 
                                                                        sliderInput('day_range', label='Selecciona el periodo temporal:', min= min(food$day), max= max(food$day), value = c(1,29), step=1 ) 
                                                                                
                                                                          )
                                                                  ),
                                                                 
                                                               fluidRow( 
                                                                 
                                                                 column(8,
                                                                        box(DTOutput('tabla_category'), width=12, solidHeader = TRUE)
                                                                        
                                                                        ),
                                                                 
                                                                 column(4,
                                                                 fluidRow(valueBox (value = "8",subtitle = "Maximum impact energy (kilotons of TNT)",icon = icon("star")) 
                                                                        ),
                                                                 fluidRow(downloadButton("download_data","Download data" ))
                                                                   
                                                                          )

                                                                        )
                                                                     )
                                                               
                                                                  ),
                                                    
                                                          #CONTEXTO
                                                            tabItem(tabName="tema2" , 
                                                                    tabBox()
                                                                    )
                        
                                                                ) #
                        
                                                              ) #TabPanel clousure
                      
                      
                                                         )#body clousure
                                        )#DASHBOARD PAGE CLOUSURE
                      
                      
                      


server <- function(input, output){  
  tabla0<-reactive({
    filter(food,
           day >= input$day_range[1] & 
             day <= input$day_range[2] &
             store %in% input$sitio_select &
             product_category_3_name %in% input$categoria3_select)%>%
      group_by(day, store)%>%
      summarise(ventas=sum(sales))
  })
  
  tabla<-reactive({
    filter(food,
           day >= input$day_range[1] & 
             day <= input$day_range[2] &
             store %in% input$sitio_select &
             product_category_3_name %in% input$categoria3_select)
  })
  
  
  tabla2<-reactive({
    filter(food,
           day >= input$day_range[1] & 
             day <= input$day_range[2] &
             store %in% input$sitio_select &
             product_category_3_name %in% input$categoria3_select)%>%
      group_by(product_category_3_name)%>%
      summarise(cantidad_prod=sum(qty))
  })
  
  tabla3<-reactive({top_n(tabla2(), 10)
  })
  
  
  output$table<-renderDT({
    tabla() 
  })
  
  output$download_data <- downloadHandler(
    filename = "food.csv",
    content = function(file) {
      write.csv(tabla(), file, row.names = FALSE)
    })
  
  output$cloud <- renderWordcloud2({
    # Create a word cloud object
    wordcloud2(recuento2, minSize = input$freq)
  })
  
  output$boxplot_category<-renderPlotly(
    plot_ly(tabla(), x=~product_category_3_name, y= ~qty , type= "box", color= ~product_category_3_name)
  )
  
  output$barras_top<- renderPlotly(
    ggplot(tabla3(), aes(x=product_category_3_name, y=cantidad_prod))+
      geom_bar(stat="identity", color="royalblue", fill="blue", alpha=0.7)+ 
      theme(axis.text.x=element_text(face="bold", angle=45))+
      labs(title="Top 10 productos mas vendidos en food")
  )
  
  output$tabla_top<- renderDT({
    tabla3()
  })
  
  output$tabla_category<- renderDT({
    tabla()
    
  })
  
  output$dispersion<-renderPlotly(
    plot_ly(tabla(), x= ~qty, y= ~sales , type= "scatter", mode= "markers", color= ~store)
  )
  
  output$lineas<-renderPlotly(
    ggplot(tabla0(), aes(x=day, y=ventas, color=store), group=1) +
      geom_line() +
      scale_x_continuous(limits =c(input$day_range[1],input$day_range[2]),
                         breaks = seq(input$day_range[1],input$day_range[2], 1) )
  )
  
}

shinyApp(ui = ui, server = server)

