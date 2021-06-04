
# DESCRIPTIVOS EN SQL -----------------------------------------------------
conexion<-dbConnect(odbc(),"conexion_ikea")

#MEDIAS Y CANTIDAD EN CADA DIA DEL MES
media_mob<- dbGetQuery(conexion, "select day(transaction_timestamp) as dia, avg(sales) as ventas_medias, avg(qty) as cantidad_media
                       from furniture
                       group by day(transaction_timestamp)
                       order by dia;")

media_food<- dbGetQuery(conexion, "select day(transaction_timestamp) as dia, avg(sales) as ventas_medias, avg(qty) as cantidad_media
                       from food
                       group by day(transaction_timestamp)
                       order by dia;")

#¿CUANTOS COMPRADORES HAY EN MOBILIARIO Y EN COMIDA?
comp_mob<- dbGetQuery(conexion,"select count(distinct(membership_id)) as membership_num from furniture;")
comp_food<- dbGetQuery(conexion,"select count(distinct(membership_id)) as num_membership from food;")
numero_comprados<- cbind(comp_mob, comp_food)
remove(comp_food,comp_mob)

#VENTAS MEDIAS EN CADA TIENDA
medias_fur<- dbGetQuery(conexion, "select store, avg(sales) as ventas_mobiliario
from furniture
group by store;")

medias_food<- dbGetQuery(conexion, "select store, avg(sales) as ventas_comida
from food
group by store;")

ventas_medias<- merge(medias_fur, medias_food, by= "store")
remove(medias_food,medias_fur)

#VENTAS MEDIAS JERARQUIA CATEGORIA 3
mob_pro3<- dbGetQuery(conexion, "select product_category_3_name as Categoria_3, avg(sales) as VentasMedia_Mob
from furniture 
group by product_category_3_name
order by avg(sales) DESC;")

food_pro3<- dbGetQuery(conexion, "select  product_category_3_name as Categoria_3, avg(sales) as VentasMedia_Comida
from food 
group by product_category_3_name
order by avg(sales) DESC;")

  #¿CUALES SON LAS CATEGORIAS QUE SUPERAN LA MEDIA?
mob_pro32<-dbGetQuery(conexion, "select product_category_3_name as Categoria_3, sales as Ventas_Mob
from furniture 
where sales > (select avg(sales) from furniture)
group by product_category_3_name;")

food_pro32<-dbGetQuery(conexion, "select product_category_3_name as Categoria_3, sales as Ventas_Comida
from food 
where sales > (select avg(sales) from food)
group by product_category_3_name;")

#MAXIMO DE VENTAS EN MOBILIARIO
max_mob<- dbGetQuery(conexion, "select max(sales) as maximo_ventas_mobiliario
from furniture 
order by maximo_ventas_mobiliario DESC;") 

max_food<- dbGetQuery(conexion, "select max(sales) as maximo_ventas_comida
from food 
order by maximo_ventas_comida DESC;") 

maximos<- cbind(max_mob, max_food)
remove(max_mob, max_food)


#¿QUIEN HA COMPRADO MENOS? Y EN QUE TIENDA?
min_fur<- dbGetQuery(conexion, "select store, membership_id, MIN(sales) AS minimo_venta_mob
from furniture;")

min_food<- dbGetQuery(conexion, "select store, membership_id, MIN(sales) AS minimo_venta_comida
from food;")

remove(menos_comprador)

#Y EL MAX?
max2_mob<- dbGetQuery(conexion, "select store, membership_id, MAX(sales) AS maximo_venta_mob
from furniture;")

max2_food<- dbGetQuery(conexion, "select store, membership_id, MAX(sales) AS maximo_venta_comida
from food;")




