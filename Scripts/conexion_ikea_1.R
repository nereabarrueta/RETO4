#Librerias cargadas a mano, source da error
library(odbc)
library(modeest)
library(dplyr)
library(naniar)
library(tidyverse)
library(recommenderlab)
library(tibble)
library(tidyr)

# Conexion con SQL ---------------------------------------------------------------
conexion<-dbConnect(odbc(),"conexion_ikea")

confood<-dbGetQuery(conexion,"select  fo.membership_id,fo.product_category_3_name,
  sum(fo.qty)	sumafood				            
 from food as fo
  group by fo.membership_id, fo.product_category_3_name  ;" )

dim(confood)
head(confood)
dimnames(confood)

confurniture<-dbGetQuery(conexion,"select fu.membership_id  ,
  fu.product_category_3_name,
 sum(fu.qty)  sumafurniture      
 from furniture  AS fu
  group by fu.membership_id, fu.product_category_3_name ;" )

dim(confurniture)

# Pivotamos ---------------------------------------------------------------
confood<-confood%>%
  pivot_wider(names_from=product_category_3_name,values_from=sumafood)
dim(confood)
dimnames(confood) 

confood[["UNKNOWN"]]<-NULL

confurniture<-confurniture%>%
  pivot_wider(names_from=product_category_3_name,values_from=sumafurniture)

confurniture[["UNKNOWN"]]<-NULL

  #hacemos el merge de ambas tablas
ikea<-merge(confood,confurniture,by="membership_id")
dim(ikea)
