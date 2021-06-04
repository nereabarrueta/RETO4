# RETO 4 (EQUIPO VERDE CLARO)

# Librerias ---------------------------------------------------------------

source("Scripts/librerias.R")

# Carga del Dataset -------------------------------------------------------

source("Scripts/CargaDeFicheros.R")

# Limpieza de datos --------------------------------------------------------
#data quality
source("Scripts/data_quality.R")

#data discovery
source("Scripts/estadisticos_descriptivos.R")

# Graficos descriptivos

source("Scripts/graficos_descriptivos.R")


# Modelo(recomendador) ----------------------------------------------------

#conexionIKEA
source("Scripts/conexion_ikea.R")

#SQL
source("Scripts/descriptivos_SQL.R")

#filtrado (compra mucho y normal)
source("Scripts/filtrados.R")

#graficos despues de filtrado
source("Scripts/graficoS_filtrado.R")

#recomendador compra mucho y normal
source("Scripts/compra_mucho.R")
source("Scripts/compra_normal.R")

#svd
source("Scripts/SVD.R") #NO ESTA

#binarizada
source("Scripts/matriz_binarizada.R") #NO ESTA


# Gráficos ----------------------------------------------------------------

# Grafico en shiny

source("Scripts/") #NO ESTA


# API ---------------------------------------------------------------------

source("Scripts/API_MODELO.R")
source("Scripts/API_probabilidad.R")



