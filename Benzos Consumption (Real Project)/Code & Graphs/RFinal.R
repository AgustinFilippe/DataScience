#----------------------------------------------------------------------------------------------------------------------------------------------
# Cargar las librerías necesarias
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("glmnet", quietly = TRUE)) install.packages("glmnet")
if (!requireNamespace("caret", quietly = TRUE)) install.packages("caret")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("xgboost", quietly = TRUE)) install.packages("xgboost")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("PRROC", quietly = TRUE)) install.packages("PRROC")
library(ggplot2)
library(dplyr)
library(reshape2)
library(glmnet)
library(caret)
library(tidyr)
library(xgboost)
library(pROC)
library(PRROC)

datos <- read.csv("datos_droga_limpio_completo.csv", stringsAsFactors = FALSE)

# Como se duplico la columna id, hay que borrarla,
if("id" %in% names(datos)) {
  datos <- datos[, !names(datos) %in% "id"]
}



#----------------------------------------------------------------------------------------------------------------------------------------------



# Convertir 'benzodiacepinas' en un factor ordenado ya que es nuestra objetivo, tiene que representar una jerarquia segun el nivel de consumo

# Precondicion: Porque es necesario antes de hasta moderlar
datos$benzodiacepinas <- factor(datos$benzodiacepinas, 
                                levels = c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"), 
                                ordered = TRUE)


# Definir una funcion personalizada para el tema de ggplot2, esto es porque se veain todos chicos con letras enormes entonces habia que hacer una y no joder mas
# A esto lo podemos ver como una postcondicion si mal no estoy

# Postcondicion: La función 'mi_tema' esta definida para poder devolver graficos
mi_tema <- function() {
  theme_minimal(base_size = 18) + # Aumentar el tamaño base
    theme(
      plot.title = element_text(hjust = 0.5, size = 14), # Aumentar y centrar el titulo
      axis.title = element_text(size = 12), # Aumentar los titulos de los ejes
      axis.text = element_text(size = 12), # Aumentar el texto de los ejes
      legend.title = element_text(size = 12), # Aumentar el titulo de la leyenda
      legend.text = element_text(size = 1) # Aumentar el texto de la leyenda
    )
}



#----------------------------------------------------------------------------------------------------------------------------------------------



# Grafico de distribucion de clases del consumo de Benzodiacepinas

grafico1 <- ggplot(datos, aes(x = benzodiacepinas)) +
  geom_bar(fill = "#1f77b4", color = "black") +
  labs(
    title = "Distribución de Clases de Benzodiacepinas",
    x = "Nivel de Consumo",
    y = "Frecuencia"
  ) +
  mi_tema()

print(grafico1)



#----------------------------------------------------------------------------------------------------------------------------------------------


grafico2 <- ggplot(datos, aes(x = benzodiacepinas, fill = edad)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(
    palette = "Set1",
    name = "Rango de Edad",
    breaks = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
    labels = c("18-24 años", "25-34 años", "35-44 años", "45-54 años", "55-64 años", "65 años o más")
  ) +
  labs(
    title = "Consumo de Benzodiacepinas Según Edad",
    x = "Clases de Benzodiacepinas",
    y = "Frecuencia"
  ) +
  mi_tema() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    legend.key.width = unit(2, "cm")
  )

print(grafico2)

#----------------------------------------------------------------------------------------------------------------------------------------------



# Creamos una lista de variables sobre las características de personalidad y demográficas para el análisis bivariado

# # Precondicion: Las variables existen en el dataset y son relevantes para hacer un EDA
vars_no_drogas <- c("edad", "genero", "educacion", "pais", "etnicidad",
                    "neuroticismo", "extraversion", "apertura", "amabilidad",
                    "responsabilidad", "impulsividad", "busqueda_sensaciones")



#----------------------------------------------------------------------------------------------------------------------------------------------


for (var in vars_no_drogas) {
  p <- ggplot(datos, aes_string(x = var, fill = "benzodiacepinas")) +
    geom_bar(position = "dodge", show.legend = TRUE) +
    scale_fill_manual(
      name = "Clases de Benzodiacepinas",
      values = c("CL0" = "#66C2A5", "CL1" = "#FC8D62", "CL2" = "#8DA0CB", 
                 "CL3" = "#E78AC3", "CL4" = "#A6D854", "CL5" = "#FFD92F", 
                 "CL6" = "#E5C494"),
      breaks = c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"),
      labels = c("CL0", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6"),
      drop = FALSE,
      guide = guide_legend(title.position = "top", title.hjust = 0.5)
    ) +
    labs(
      title = paste("Benzodiacepinas vs", var),
      x = var,
      y = "Frecuencia"
    ) +
    mi_tema() +
    # Sobrescribimos el tema para asegurar que la leyenda se vea claramente
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.8, "cm")
    )
  
  print(p)
}




#----------------------------------------------------------------------------------------------------------------------------------------------



# Filtrar el dataset para excluir CL0, para realizar el analisis con la gente que si consume o consumia

# Precondicion: Para poder analizar solo los que consumenn o consumeron en algun momento
datos_filtrados <- subset(datos, benzodiacepinas != "CL0")


# Sin CL0 vs Variables Demográficas y de Personalidad

for (var in vars_no_drogas) {
  p <- ggplot(datos_filtrados, aes_string(x = var, fill = "benzodiacepinas")) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = paste("Benzos (sin CL0) vs", var),
      x = var,
      y = "Frecuencia",
      fill = "Clases de Benzodiacepinas"
    ) +
    mi_tema() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(p)
}



#----------------------------------------------------------------------------------------------------------------------------------------------



# Conversión de variables categóricas a numéricas para análisis de correlación

# Precondicion: Pasar las variables categoricas a numericas
datos_num <- datos
categorical_to_numeric <- c("neuroticismo", "extraversion", "apertura", "amabilidad", "responsabilidad",
                            "edad", "genero", "educacion", "pais", "etnicidad")

for (var in categorical_to_numeric) {
  datos_num[[var]] <- as.numeric(factor(datos_num[[var]], ordered = TRUE))
}

# Seleccionar variables de personalidad y demográficas para correlación

variables_personalidad_demograficas <- c("benzodiacepinas", "neuroticismo", "extraversion", "apertura", 
                                         "amabilidad", "responsabilidad", "edad", "genero", 
                                         "educacion", "pais", "etnicidad")
datos_num_personalidad_demo <- datos_num[, variables_personalidad_demograficas]
datos_num_personalidad_demo <- datos_num_personalidad_demo[, sapply(datos_num_personalidad_demo, is.numeric)]



#----------------------------------------------------------------------------------------------------------------------------------------------



# Calcular matriz de correlaciones utilizando el coeficiente de Spearman

cor_personalidad_demo <- cor(datos_num_personalidad_demo, use = "pairwise.complete.obs", method = "spearman")

# Convertir matriz de correlación a formato largo para graficar

cor_personalidad_demo_derretido <- melt(cor_personalidad_demo)

# Heatmap de Correlaciones entre Benzodiacepinas y Variables de Personalidad/Demográficas

heatmap_personalidad_demo <- ggplot(cor_personalidad_demo_derretido, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(abs(value) > 0.3, round(value, 2), "")), 
            color = "black", size = 4) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlación"
  ) +
  labs(
    title = "Correlaciones: Benzodiacepinas con Variables de Personalidad y Demográficas",
    x = "Variables",
    y = "Variables"
  ) +
  mi_tema() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 18), 
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = ggplot2::margin(10, 10, 10, 10) # Llamar explícitamente ggplot2::margin
  ) +
  coord_fixed(ratio = 1.2)



print(heatmap_personalidad_demo)



#----------------------------------------------------------------------------------------------------------------------------------------------



# Definir las variables relacionadas con las drogas

variables_drogas <- c("alcohol", "anfetaminas", "nitrito_amilo", "benzodiacepinas", 
                      "cannabis", "chocolate", "cocaina", "cafeina", "crack", 
                      "extasis", "heroina", "ketamina", "drogas_legales", "lsd", 
                      "metadona", "hongos", "nicotina", "semeron", "sustancias_volatiles")


# Verificar si las variables de drogas existen en el dataset

variables_drogas_presentes <- intersect(variables_drogas, colnames(datos))

categorical_variables_drogas <- variables_drogas_presentes[!sapply(datos[variables_drogas_presentes], is.numeric)]
for (var in categorical_variables_drogas) {
  datos[[var]] <- as.numeric(factor(datos[[var]], ordered = TRUE))
}


# Crear subset con las variables de drogas

datos_num_drogas <- datos[, variables_drogas_presentes]
datos_num_drogas <- datos_num_drogas[, sapply(datos_num_drogas, is.numeric)]
cor_drogas <- cor(datos_num_drogas, use = "pairwise.complete.obs", method = "spearman")


# Convertir matriz de correlación a formato largo para graficar

cor_drogas_derretido <- melt(cor_drogas)

# Crear un heatmap de correlaciones entre las variables de drogas

heatmap_drogas_limpio <- ggplot(cor_drogas_derretido, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlación"
  ) +
  labs(
    title = "Correlaciones entre Variables de Drogas",
    x = "Variables de Drogas",
    y = "Variables de Drogas"
  ) +
  theme_minimal(base_size = 18) + # Ajustar tamaño base
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 18),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.margin = ggplot2::margin(10, 10, 10, 10),
    axis.ticks = element_blank()
  ) +
  coord_fixed(ratio = 1.2)

print(heatmap_drogas_limpio)



#----------------------------------------------------------------------------------------------------------------------------------------------
" Vamos a analizar los distintos modelos para predecir la probabilidad de que alguien se drogue con benzodiacepinas"
#----------------------------------------------------------------------------------------------------------------------------------------------


set.seed(123) # el auc con seed = 42, es mucho menor al de ahora

# Hacemos una copia de los datos para modelar el XGBoost, nuestro tercer intento de modelo

# Precondicion:'datos' debe existir y ser un data.frame con los datos originales.
datos_modelo <- datos



# Verificamos que la variable objetivo exista

if(!"benzodiacepinas" %in% colnames(datos_modelo)){
  stop("La variable 'benzodiacepinas' no se encuentra en los datos.")
}

# Convertimos la variable objetivo en factor

# Precondicion: 'datos_modelo$benzodiacepinas' es un factor con niveles '1', '2', ..., '7'
datos_modelo$benzodiacepinas <- as.factor(datos_modelo$benzodiacepinas)

# Convertir Clase 1 vs. Clases 2-7 en una variable binaria

datos_modelo$benzodiacepinas_binaria <- ifelse(datos_modelo$benzodiacepinas == "1", 0, 1)

# Verificamos la distribucion de la variable binaria

print(table(datos_modelo$benzodiacepinas_binaria))

# Invariante: 'benzodiacepinas_binaria' debe tener al menos dos clases y que no cambien nunca desde ahora
if(length(unique(datos_modelo$benzodiacepinas_binaria)) < 2){
  stop("La variable 'benzodiacepinas_binaria' no tiene suficientes clases para crear una particion.")
}

# Convertimos la variable binaria a factor

datos_modelo$benzodiacepinas_binaria <- as.factor(datos_modelo$benzodiacepinas_binaria)

# Dividimos los datos en entrenamiento y prueba

# Postcondicion: Los conjuntos de entrenamiento y prueba tienen ambas clases representadas
indice_entrenamiento <- createDataPartition(datos_modelo$benzodiacepinas_binaria, p = 0.8, list = FALSE)
datos_entrenamiento <- datos_modelo[indice_entrenamiento, ]
datos_prueba <- datos_modelo[-indice_entrenamiento, ]


# Separamos las variables predictoras

variables_predictoras <- setdiff(names(datos_modelo), c("id", "benzodiacepinas", "benzodiacepinas_binaria"))

# Identificamos variables numericas y categoricas

variables_numericas <- variables_predictoras[sapply(datos_modelo[variables_predictoras], is.numeric)]
variables_categoricas <- variables_predictoras[sapply(datos_modelo[variables_predictoras], function(x) is.character(x) | is.factor(x))]

# Convertimos variables categoricas a factores

datos_entrenamiento[variables_categoricas] <- lapply(datos_entrenamiento[variables_categoricas], as.factor)
datos_prueba[variables_categoricas] <- lapply(datos_prueba[variables_categoricas], as.factor)

# Creamos variables dummy para las categoricas

codificador_dummy <- dummyVars(~ ., data = datos_entrenamiento[variables_categoricas], fullRank = TRUE)
entrenamiento_dummy <- predict(codificador_dummy, newdata = datos_entrenamiento[variables_categoricas])
entrenamiento_dummy <- as.data.frame(entrenamiento_dummy)

# Tomamos las variables numericas

entrenamiento_numerico <- datos_entrenamiento[variables_numericas]

# Combinamos numericas y dummy

entrenamiento_combinado <- cbind(entrenamiento_numerico, entrenamiento_dummy)

# Aplicamos preprocesamiento (imputacion, centrado y escalado)

pasos_preprocesamiento <- c("medianImpute", "center", "scale")
modelo_preprocesamiento <- preProcess(entrenamiento_combinado, method = pasos_preprocesamiento)

entrenamiento_procesado <- predict(modelo_preprocesamiento, entrenamiento_combinado)

# Aplicamos el mismo proceso al conjunto de prueba

prueba_dummy <- predict(codificador_dummy, newdata = datos_prueba[variables_categoricas])
prueba_dummy <- as.data.frame(prueba_dummy)
prueba_numerica <- datos_prueba[variables_numericas]
prueba_combinada <- cbind(prueba_numerica, prueba_dummy)
prueba_procesada <- predict(modelo_preprocesamiento, prueba_combinada)

# Aseguramos que las columnas de prueba coincidan con entrenamiento

columnas_faltantes <- setdiff(colnames(entrenamiento_procesado), colnames(prueba_procesada))
if(length(columnas_faltantes) > 0){
  for(col in columnas_faltantes){
    prueba_procesada[, col] <- 0
  }
}

# Reordenamos las columnas


# Postcondicion: 'prueba_procesada' tiene las mismas columnas que 'entrenamiento_procesado'
prueba_procesada <- prueba_procesada[, colnames(entrenamiento_procesado), drop = FALSE]



# Extraemos la variable objetivo

# Precondicion: y_entrenamiento_binario_xgb e y_prueba_binario_xgb son factores con valores 0 y 1
y_entrenamiento_binario_xgb <- datos_entrenamiento$benzodiacepinas_binaria
y_prueba_binario_xgb <- datos_prueba$benzodiacepinas_binaria



# Convertimos los datos a matrices

x_entrenamiento_matriz <- as.matrix(entrenamiento_procesado)
x_prueba_matriz <- as.matrix(prueba_procesada)

# Crear el DMatrix para XGBoost

dtrain <- xgb.DMatrix(data = x_entrenamiento_matriz, label = as.numeric(as.character(y_entrenamiento_binario_xgb)))
dtest <- xgb.DMatrix(data = x_prueba_matriz, label = as.numeric(as.character(y_prueba_binario_xgb)))

# Validacion Cruzada para Optimizacion de Hiperparametros
# Configuramos una grilla de hiperparametros

param_grid <- expand.grid(
  max_depth = c(4, 6, 8),
  eta = c(0.01, 0.1, 0.3),
  subsample = c(0.6, 0.8, 1),
  colsample_bytree = c(0.6, 0.8, 1)
)

# Funcion para evaluar cada combinacion de hiperparametros

eval_function <- function(params_row) {
  params <- as.list(params_row)
  xgb_model <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = params$max_depth,
      eta = params$eta,
      subsample = params$subsample,
      colsample_bytree = params$colsample_bytree
    ),
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain),
    verbose = 0
  )
  
  # Calcular AUC en el conjunto de prueba
  pred <- predict(xgb_model, newdata = dtest)
  
  # Verificamos que 'response' tenga dos niveles
  if(length(unique(as.numeric(as.character(y_prueba_binario_xgb)))) < 2){
    return(NA)
  }
  
  roc_curve <- roc(response = as.numeric(as.character(y_prueba_binario_xgb)), predictor = pred)
  return(auc(roc_curve))
}

# Aplicamos la validacion cruzada

resultados <- sapply(seq_len(nrow(param_grid)), function(i) eval_function(param_grid[i, ]))

# Eliminamos los NA de los resultados

resultados_validos <- !is.na(resultados)
param_grid_validos <- param_grid[resultados_validos, ]
resultados_validos <- resultados[resultados_validos]

# Invariante: 'resultados_validos' contiene solo resultados que no son NA, y no puede cambiar nunca
if(length(resultados_validos) == 0){
  stop("No se pudo calcular AUC debido a falta de variabilidad en la variable respuesta.")
}

# Identificamos los mejores hiperparametros

mejores_params <- param_grid_validos[which.max(resultados_validos), ]
cat("Mejores hiperparametros:\n")
print(mejores_params)

# Entrenamos el modelo optimizado

xgb_model_optimizado <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = mejores_params$max_depth,
    eta = mejores_params$eta,
    subsample = mejores_params$subsample,
    colsample_bytree = mejores_params$colsample_bytree
  ),
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain),
  verbose = 1
)

# Prediccion de probabilidades

xgb_pred <- predict(xgb_model_optimizado, newdata = dtest)

# Curva ROC y AUC

roc_curve <- roc(response = as.numeric(as.character(y_prueba_binario_xgb)), predictor = xgb_pred)
valor_auc <- auc(roc_curve)
cat("XGBoost AUC Optimizado:", valor_auc, "\n")

# Grafico de la Curva ROC

plot(roc_curve, main = paste("Curva ROC - XGBoost - AUC:", round(valor_auc, 2)), col = "blue", lwd = 2)

# --- Curva Precision-Recall ---
pr_curve <- pr.curve(scores.class0 = xgb_pred[y_prueba_binario_xgb == 1],
                     scores.class1 = xgb_pred[y_prueba_binario_xgb == 0],
                     curve = TRUE)
plot(pr_curve, main = "Curva Precision-Recall - XGBoost", col = "darkgreen", lwd = 2)

# Grafico de Ganancias Acumuladas

lift_data <- data.frame(
  Probabilidad = xgb_pred,
  Clase_Real = as.numeric(as.character(y_prueba_binario_xgb))
)
lift_data <- lift_data[order(-lift_data$Probabilidad), ]
lift_data$Posicion <- 1:nrow(lift_data)
lift_data$Ganancia <- cumsum(lift_data$Clase_Real) / sum(lift_data$Clase_Real)

ggplot(lift_data, aes(x = Posicion, y = Ganancia)) +
  geom_line(color = "purple", size = 1) +
  labs(title = "Grafico de Ganancias Acumuladas - XGBoost",
       x = "Numero de Observaciones",
       y = "Ganancia Acumulada") +
  theme_minimal()

# Convertimos probabilidades en clases con el umbral de 0.47

xgb_pred_class <- ifelse(xgb_pred > 0.47, 1, 0)

# Matriz de confusion

matriz_confusion_xgb <- confusionMatrix(
  as.factor(xgb_pred_class), 
  as.factor(as.numeric(as.character(y_prueba_binario_xgb))), 
  positive = "1"
)
print(matriz_confusion_xgb)

# Grafico de la Matriz de Confusion

cm_table_xgb <- as.data.frame(matriz_confusion_xgb$table)
colnames(cm_table_xgb) <- c("Prediccion", "Referencia", "Frecuencia")

ggplot(data = cm_table_xgb, aes(x = Referencia, y = Prediccion, fill = Frecuencia)) +
  geom_tile() +
  geom_text(aes(label = Frecuencia), color = "black", size = 5) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Matriz de Confusion - XGBoost", x = "Referencia", y = "Prediccion") +
  theme_minimal() +
  coord_fixed()

# Calcular importancia de variables

importancia <- xgb.importance(feature_names = colnames(x_entrenamiento_matriz), model = xgb_model_optimizado)

# Mostrar las 10 variables mas importantes

cat("Variables mas importantes:\n")
print(head(importancia, 10))

# Grafico de Importancia de Variables

xgb.plot.importance(importance_matrix = importancia[1:10, ], main = "Importancia de Variables - XGBoost")

