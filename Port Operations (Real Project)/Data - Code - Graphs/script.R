

rm(list = ls())

source("script_base.R")

#install.packages("ggplot2")
#install.packages("GGaly")
#install.packages("class")


#-----------------------------Grafico de Dispersion: Tons vs TimeAtPort--------------------------------------------------
library(ggplot2)

ggplot(data, aes(x = TimeAtPort, y = Tons, color = Operation)) +
  geom_point(alpha = 0.4) +
  labs(title = "Grafico de Dispersion: Tons vs TimeAtPort",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  theme_minimal()

#-----------------------------Modelo--------------------------------------------------
# odelo de regresión lineal
model <- lm(Tons ~ TimeAtPort, data = data)

# ddispersión con linea de regresion
ggplot(data, aes(x = TimeAtPort, y = Tons, color = Operation)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Gráfico de Dispersión con Línea de Regresión",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#-----------------------------grafico de pares--------------------------------------------------

library(GGally)
library(ggplot2)
# grafico de pares
ggpairs(
  data, 
  aes(color = Operation), 
  columns = c("TimeAtPort", "Tons")
) + 
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

#-------------------------k NN------------------------------------------------------
#para clasificar por knn
library(class)

# divido los datos en conjunto de entrenamiento y prueba
set.seed(69)  # para reproducibilidad seteo la semilla
train_index <- sample(1:nrow(data), 0.7 * nrow(data))  # 70% para entrenamiento
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# normalizazion de las características
train_data_scaled <- scale(train_data[, c("Tons", "TimeAtPort")])
test_data_scaled <- scale(test_data[, c("Tons", "TimeAtPort")],
                          center = attr(train_data_scaled, "scaled:center"),
                          scale = attr(train_data_scaled, "scaled:scale"))
# seteo el número de vecinos
k <- 3
# clasificación kNN
predictions <- knn(train = train_data_scaled, 
                   test = test_data_scaled, 
                   cl = train_data$Operation, 
                   k = k)
# matriz de confusión
confusion_matrix <- table(test_data$Operation, predictions)
# calcular precisión
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Precisión:", round(accuracy, 4)))

print(confusion_matrix)

# crear la matriz de confusión
confusion_matrix <- table(test_data$Operation, predictions)

# convertir la matriz de confusión a un dataframe para porder visualizarlo en ggplot
confusion_df <- as.data.frame(confusion_matrix)
colnames(confusion_df) <- c("Actual", "Predicted", "Count")

# Calcular precisión
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Precisión:", round(accuracy, 4)))

# Calcular error de pérdida
loss_error <- 1 - accuracy
print(paste("Error de pérdida:", round(loss_error, 4)))

ggplot(data = confusion_df, aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Count), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Count), vjust = 1, color = "white", size = 6) +
  labs(
    title = "Matriz de Confusión",
    x = "Clase Actual",
    y = "Clase Predicha"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#----------Grafico ---------------

test_data$Predictions <- predictions
# grafico de las clases reales
ggplot(test_data, aes(x = TimeAtPort, y = Tons, color = Operation)) +
  geom_point(alpha = 0.6) +
  labs(title = "TimeAtPort vs Tons, Real",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
# grafico de las predicciones
ggplot(test_data, aes(x = TimeAtPort, y = Tons, color = Predictions)) +
  geom_point(alpha = 0.6) +
  labs(title = "Clases Predichas por el modelo k-NN",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
# columna para predicciones correctas y incorrectas
test_data$Correct <- ifelse(test_data$Operation == test_data$Predictions, "Correcto", "Incorrecto")

ggplot(test_data, aes(x = TimeAtPort, y = Tons, color = Correct)) +
  geom_point(alpha = 0.6) +
  labs(title = "Predicciones Correctas vs Incorrectas en el Modelo k-NN",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  scale_color_manual(values = c("Correcto" = "green", "Incorrecto" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

#------perdida


#-----------------------k Means Manu-------------------------------------------------------
data_selected <- data[, c("Tons", "TimeAtPort")]
data_normalized <- scale(data_selected)

set.seed(420)
# cantidad de grupos
k <- 3
kmeans_result <- kmeans(data_normalized, centers = k, nstart = 25)

# agregar los resultados al dataframe original
data$Cluster <- as.factor(kmeans_result$cluster)
# gráfico de dispersión con Tons en el eje y y TimeAtPort en el eje x
ggplot(data, aes(x = TimeAtPort, y = Tons, color = Cluster)) +
  geom_point(alpha = 0.6) +
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = TimeAtPort, y = Tons), 
             color = "red", size = 5, shape = 3) +
  labs(title = "Resultados del Agrupamiento K-means",
       x = "Tiempo en el Puerto (TimeAtPort)",
       y = "Tons") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
#------Logico--------
rm(list = ls())

source("script_base.R")

# para arboles
library(rpart)
library(rpart.plot)

summary(data)

# creacion del modelo arbol
modelo_arbol <- rpart(Operation ~ Tons + TimeAtPort, 
                      data = data_clean, 
                      method = "class")
summary(modelo_arbol)

rpart.plot(modelo_arbol, main = "Árbol de Decisión para Predecir la Operación")

predicciones_arbol <- predict(modelo_arbol, data_clean, type = "class")

tabla_predicciones <- table(predicciones_arbol, data_clean$Operation)
print(tabla_predicciones)

# arbol
confusion_matrix_arbol <- table(predicciones_arbol, data_clean$Operation)
print(confusion_matrix_arbol)

# calcular precisión
accuracy_arbol <- sum(diag(confusion_matrix_arbol)) / sum(confusion_matrix_arbol)
print(paste("Precisión del modelo de árbol de decisión:", round(accuracy_arbol * 100, 2), "%"))

modelo_arbol_ajustado <- rpart(Operation ~ Tons + TimeAtPort, 
                               data = data_clean, 
                               method = "class", 
                               control = rpart.control(maxdepth = 2, minsplit = 10))

rpart.plot(modelo_arbol_ajustado, main = "Arbol de Decision Ajustado")

library(caret)
library(ggplot2)

confusion_matrix_arbol <- table(predicciones_arbol, data_clean$Operation)


confusion_df <- as.data.frame(confusion_matrix_arbol)
colnames(confusion_df) <- c("Predicted", "Actual", "Count")
#precision
accuracy_arbol <- sum(diag(confusion_matrix_arbol)) / sum(confusion_matrix_arbol)
accuracy_label <- paste("Precisión:", round(accuracy_arbol * 100, 2), "%")

ggplot(data = confusion_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  geom_text(aes(label = Count), color = "black", size = 6) + 
  labs(title = "Matriz de Confusión",
       x = "Clase Real",
       y = "Clase Predicha") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16),  
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) + 
  annotate("text", x = 2, y = 2.2, label = accuracy_label, size = 6, color = "black", fontface = "bold") # Aumentar el tamaño de la precisión

