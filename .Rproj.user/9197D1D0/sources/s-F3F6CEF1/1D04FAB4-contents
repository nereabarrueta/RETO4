

# Limpiar texto -------------------------------------------------

#Quitar blancos innecesarios delante y detras de product category
food$product_category_1_name<-str_trim(food$product_category_1_name)
food$product_category_2_name<-str_trim(food$product_category_2_name)
food$product_category_3_name<-str_trim(food$product_category_3_name)

#Quitar puntuacion al final de product category
food<-as.data.frame(lapply(food, function(y) gsub("[[:punct:]]+$", "", y)))

#No hay tildes que molesten

#Quitar caracteres raros
food$item_description<-gsub("Ã", "A", food$item_description)
food$item_description<-gsub("–", " ", food$item_description)
food$item_description<-gsub("„", " ", food$item_description)
food$item_description<-gsub("…", " ", food$item_description)
food$item_description<-gsub("/", " ", food$item_description)
food$item_description<-gsub("-/", " ", food$item_description)
food$item_description<-gsub(",", " ", food$item_description)
levels(as.factor(food$item_description))
#volvemos a cambiar el formato a factor (se cambia al quitar puntuacion)
food$product_category_1_name<-as.factor(food$product_category_1_name)
food$product_category_2_name<-as.factor(food$product_category_2_name)
food$product_category_3_name<-as.factor(food$product_category_3_name)

levels(food$product_category_1_name)
levels(food$product_category_2_name)
levels(food$product_category_3_name)
