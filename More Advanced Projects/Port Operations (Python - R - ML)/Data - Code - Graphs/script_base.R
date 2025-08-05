
# Abriendo el dataset
data <- read.csv("data_semi_cleaned.csv")
print(colnames(data))

# Eliminacion de la columna 'X'
data$X <- NULL 
#install.packages("dplyr")
library(dplyr)
#Re - definicion de las columnas
data <- rename(data, CommodityGroup = Commodity.Group) 
data <- rename(data, Operation = Op.)
data <- rename(data, ArrivedAtPort = Arrived.at.Port)
data <- rename(data, SailedFromPort = Sailed.from.Port)
data <- rename(data, TimeAtPort = Time.at.Port)
data <- rename(data, Coordinator = Coord.)
data <- rename(data, Country = Destination...Origin)
data <- rename(data, TimeAtBerth = Time.at.Berth)



# Conversion de data
data$CommodityGroup <- as.factor(data$CommodityGroup)
data$Commodity <- as.factor(data$Commodity)
data$Operation <- as.factor(data$Operation)
data$IMO <- as.numeric(data$IMO)
# data$Vessel <- as.factor(data$Vessel) no hace falta convertir los barcos al format ?
data$Port <- as.factor(data$Port)
data$ArrivedAtPort <- as.Date(data$ArrivedAtPort, format = "%Y-%m-%d")
data$SailedFromPort <- as.Date(data$SailedFromPort, format = "%Y-%m-%d")
data$Berth <- as.factor(data$Berth)
data$Coordinator <- as.factor(data$Coordinator)
data$Country <- as.factor(data$Country)

# Resolver los índices de los registros negativos en TimeAtPort
mal_tiempo <- which(data$TimeAtPort < 0)
data$TimeAtPort[mal_tiempo] <- abs(data$TimeAtPort[mal_tiempo])

# Cambiar de lugar los registros mal ubicados
aux <- data$ArrivedAtPort[mal_tiempo]
data$ArrivedAtPort[mal_tiempo] <- data$SailedFromPort[mal_tiempo]
data$SailedFromPort[mal_tiempo] <- aux

# Resolver los índices de los registros negativos en TimeAtBerth
mal_tiempo <- which(data$TimeAtBerth < 0)
data$TimeAtBerth[mal_tiempo] <- abs(data$TimeAtBerth[mal_tiempo])


# Filtrar filas que contienen NA
# Esto filtra el dataframe data, seleccionando solo las filas que contienen al menos un NA
#y los asigno a la varible 'filas_con_nan'
filas_con_nan <- data[!complete.cases(data), ]
print(filas_con_nan)
# Reemplazar NAN en la columna 'IMO' por 0
data$IMO[is.na(data$IMO)] <- 0


#Borro la variable que no necesito mas
rm(mal_tiempo, aux, filas_con_nan)
print(colnames(data))
