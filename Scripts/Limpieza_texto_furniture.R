# Limpiar texto -----------------------------------------------------------

#Quitar blancos innecesarios delante y detras de product category
furniture$product_category_1_name<-str_trim(furniture$product_category_1_name)
furniture$product_category_2_name<-str_trim(furniture$product_category_2_name)
furniture$product_category_3_name<-str_trim(furniture$product_category_3_name)

str(furniture)

#Quitar puntuacion al final de product category
furniture<-as.data.frame(lapply(furniture, function(y) gsub("[[:punct:]]+$", "", y)))

#No hay tildes que molesten

#Quitar A final de la categoria children en product category
furniture$product_category_1_name<-gsub("ChildrenÂ", "Children",furniture$product_category_1_name)
furniture$product_category_2_name<-gsub("ChildrenÂ", "Children",furniture$product_category_2_name)
furniture$product_category_3_name<-gsub("ChildrenÂ", "Children",furniture$product_category_3_name)

#volvemos a cambiar el formato a factor (se cambia al quitar puntuacion)
furniture$product_category_1_name<-as.factor(furniture$product_category_1_name)
furniture$product_category_2_name<-as.factor(furniture$product_category_2_name)
furniture$product_category_3_name<-as.factor(furniture$product_category_3_name)

levels(furniture$product_category_1_name)
levels(furniture$product_category_2_name)
levels(furniture$product_category_3_name)


