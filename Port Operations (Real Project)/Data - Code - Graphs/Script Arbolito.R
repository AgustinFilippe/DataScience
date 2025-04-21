#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rattle)

#Agrupar por 'Commodity Group' y sumar las toneladas
resumen_toneladas <- data %>%
  group_by(CommodityGroup) %>%
  summarise(Total_Tons = sum(Tons))
# Calcular la media de las toneladas
media_toneladas <- mean(resumen_toneladas$Total_Tons)
# Crear una nueva columna con 'Commodity_Otros' si las toneladas son mayores a la media
resumen_toneladas <- resumen_toneladas %>%
  mutate(Commodity_Final = ifelse(Total_Tons > media_toneladas, "Commodity_Otros", CommodityGroup))
# Ver el resumen de grupos modificados
print(resumen_toneladas)
# Unir esta nueva clasificación al dataset original
data <- data %>%
  left_join(resumen_toneladas %>% select(CommodityGroup, Commodity_Final), by = "CommodityGroup")
head(data)


#arbol de decision
arbol <- rpart(
  formula= Operation ~ Port + Commodity_Final.y, data=data, method="class"
  )
#graficar
fancyRpartPlot(arbol)
rpart.plot(
  arbol,
  type = 2,               # Muestra las variables en los nodos
  extra = 106,            # Muestra conteos y porcentajes
  under = TRUE,           # Texto debajo de los nodos
  varlen = 0,             # No recortar los nombres de las variables
  faclen = 0,             # No recortar los nombres de las categorías
  cex = 0.8               # Ajusta el tamaño del texto (1.5 es más grande que el valor por defecto)
)

