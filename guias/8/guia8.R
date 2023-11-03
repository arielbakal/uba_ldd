require(palmerpenguins)
require(patchwork)
require(tidyverse)
require(rpart)     
require(rpart.plot)   
require(parttree) 
require(class)

#Parte 1
penguins <- na.omit(penguins)

ggplot(penguins, aes(x = bill_length_mm, fill = sex)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribución del Largo del Pico por Sexo",
       x = "Largo del Pico (mm)",
       y = "Densidad") +
  theme_minimal()

ggplot(penguins, aes(x = flipper_length_mm, fill = sex)) +
  geom_density(alpha = 0.7) +
  labs(title = "Distribución del Largo de la Aleta por Sexo",
       x = "Largo de la Aleta (mm)",
       y = "Densidad") +
  theme_minimal()

ggplot(penguins, aes(x = bill_length_mm, y = flipper_length_mm, color = sex)) +
  geom_point() +
  labs(title = "Largo del Pico vs. Largo de la Aleta por Sexo",
       x = "Largo del Pico (mm)",
       y = "Largo de la Aleta (mm)") +
  theme_minimal()

#1
set.seed(123)

indices_entrenamiento <- sample(1:nrow(penguins), round(0.8 * nrow(penguins)), replace = FALSE)

datos_entrenamiento <- penguins[indices_entrenamiento, ]
datos_prueba <- penguins[-indices_entrenamiento, ]

calcular_error <- function(cutoff) {
  predicciones <- ifelse(datos_entrenamiento$bill_length_mm > cutoff, "male", "female")
  error <- mean(predicciones != datos_entrenamiento$sex)
  return(error)
}

"Utilizo los picos extremos en la distribucion del largo del pico de ambos sexos"

cutoff_values <- seq(30, 60, by = 0.1)
errors <- sapply(cutoff_values, calcular_error)
best_cutoff <- cutoff_values[which.min(errors)]

"Nuestro mejor cutoff es 48, por lo que deberiamos empezar nuestro arbol con 
largo >= 48 => macho"

ggplot(data = data.frame(cutoff = cutoff_values, error = errors), aes(x = cutoff, y = error)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  labs(title = "Error de Clasificación en Función de la Masa Crítica",
       x = "Masa Crítica (Cutoff)",
       y = "Error de Clasificación") +
  theme_minimal()

test_predictions <- ifelse(datos_prueba$bill_length_mm > best_cutoff, "male", "female")
confusion_matrix <- table(test_predictions, datos_prueba$sex)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy

"Luego el accuracy de este modelo es de 0.642, el cual tambien es el minimo accuracy esperable
para un modelo de clasificacion con estos datos, puesto que este modelo solo se basa en el 
mejor cutoff y que todo modelo de clasificacion parte de allí."

#2
calcular_error <- function(cutoff) {
  predicciones <- ifelse(datos_entrenamiento$flipper_length_mm > cutoff, "male", "female")
  error <- mean(predicciones != datos_entrenamiento$sex)
  return(error)
}

"Utilizo los picos extremos en la distribucion del largo del pico de ambos sexos"

cutoff_values <- seq(180, 230, by = 0.1)
errors <- sapply(cutoff_values, calcular_error)
best_cutoff <- cutoff_values[which.min(errors)]

"Nuestro mejor cutoff es 193"

ggplot(data = data.frame(cutoff = cutoff_values, error = errors), aes(x = cutoff, y = error)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 2) +
  labs(title = "Error de Clasificación en Función de la Masa Crítica",
       x = "Masa Crítica (Cutoff)",
       y = "Error de Clasificación") +
  theme_minimal()

test_predictions <- ifelse(datos_prueba$flipper_length_mm > best_cutoff, "male", "female")
confusion_matrix <- table(test_predictions, datos_prueba$sex)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy

"Tenemos una accuracy de 0.612, peor en comparacion al modelo con el largo del pico (0.642)"

#3
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm, color = sex)) +
  geom_point() +
  labs(title = "Peso vs. Largo de la Aleta por Sexo",
       x = "Peso (g)",
       y = "Largo de la Aleta (mm)") +
  theme_minimal()

modelo <- rpart(sex ~ body_mass_g + flipper_length_mm, data = datos_entrenamiento)

rpart.plot(modelo, main = "Árbol de Decisión para Clasificación de Sexo en Pingüinos",
           extra = 101, under = TRUE)

predicciones <- predict(modelo, newdata = datos_prueba, type = "class")

confusion_matrix <- table(predicciones, datos_prueba$sex)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
confusion_matrix
accuracy

"Aumentamos el accuracy a 0.806, puesto que estamos usando dos variables predictoras y
ajustamos un arbol de decision con multiples ramas"

confusion_matrix_plot <- as.data.frame(as.table(confusion_matrix))
names(confusion_matrix_plot) <- c("Predicción", "Observación", "Frecuencia")
ggplot(confusion_matrix_plot, aes(x = Predicción, y = Observación, fill = Frecuencia)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(aes(label = Frecuencia), vjust = 1) +
  labs(title = "Matriz de Confusión",
       x = "Predicción",
       y = "Observación") +
  theme_minimal()

#4
set.seed(123)

num_particiones <- 1000
accuracies <- numeric(num_particiones)

for (i in 1:num_particiones) {
  
  train_index <- sample(1:nrow(penguins), 0.8 * nrow(penguins))
  train_data <- penguins[train_index, ]
  test_data <- penguins[-train_index, ]
  
  # Construir el modelo de árbol de decisión
  modelo <- rpart(sex ~ body_mass_g + flipper_length_mm, data = train_data)
  
  # Hacer predicciones en el conjunto de prueba
  predicciones <- predict(modelo, newdata = test_data, type = "class")
  
  # Calcular la matriz de confusión y la precisión
  confusion_matrix <- table(predicciones, test_data$sex)
  accuracies[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
}

ggplot(data = data.frame(Accuracy = accuracies), aes(x = Accuracy)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  labs(title = "Distribución de Precisión en 1000 Particiones Train-Test",
       x = "Precisión (Accuracy)",
       y = "Frecuencia") +
  theme_minimal()

rango_accuracies = c(min(accuracies),max(accuracies))
rango_accuracies

max(accuracies) - min(accuracies)

mean(accuracies)

"Nuestra accuracy varia en el rango de (0.672,0.925), es decir varia un maximo de
0.256, lo cual es significante para nuestro analisis. Sin embargo, tiene mas
sentido usar el accuracy promedio (0.813) para alguna conclusion de este modelo"

#5
modelo_1 <- rpart(sex ~ body_mass_g + flipper_length_mm, data = datos_entrenamiento)
rpart.plot(modelo_1, main = "Árbol de Decisión para Clasificación de Sexo en Pingüinos",
           extra = 101, under = TRUE)


#6
predictors <- c("bill_length_mm", "flipper_length_mm")
target <- "sex"

error_data <- data.frame(k = numeric(), error = numeric())

for (k in seq(1, 20, by = 2)) {
  knn_model <- knn(train = datos_entrenamiento[predictors], 
                   test = datos_prueba[predictors], 
                   cl = datos_entrenamiento[[target]], 
                   k = k)
  
  error <- 1 - sum(knn_model == datos_prueba[[target]]) / nrow(datos_prueba)
  error_data <- rbind(error_data, data.frame(k = k, error = error))
}

optimal_k <- error_data[which.min(error_data$error), "k"]
optimal_k

ggplot(error_data, aes(x = k, y = error)) +
  geom_line() +
  geom_point(color = "red") +
  labs(x = "Valor de k", y = "Error de predicción", 
       title = "Error de predicción para diferentes valores de k en k-NN")

optimal_knn_model <- knn(train = datos_entrenamiento[predictors], 
                         test = datos_prueba[predictors], 
                         cl = datos_entrenamiento[[target]], 
                         k = optimal_k)

accuracy <- sum(optimal_knn_model == datos_prueba[[target]]) / nrow(datos_prueba)
accuracy

"0.821 accuracy"

modelo <- rpart(sex ~ bill_length_mm + flipper_length_mm, data = datos_entrenamiento)

rpart.plot(modelo, main = "Árbol de Decisión para Clasificación de Sexo en Pingüinos",
           extra = 101, under = TRUE)

predicciones <- predict(modelo, newdata = datos_prueba, type = "class")

confusion_matrix <- table(predicciones, datos_prueba$sex)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

"0.806 accuracy"

"Observamos que tenemos una mayor accuracy con el clasificador de knn que el de arboles.
Aunque la diferencia es minima (0.015)"

#Parte 2
predictors <- c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm", "sex", "island", "year")

"Vamos a hacer todas las combinaciones posibles de las variables predictoras, en grupos
de 1 a 7, haciendo la sumatoria del numero combinatorio (7,k) con k=1,..,7 tenemos 127
modelos distintos y vamos a testear cual es el modelo con mejor accuracy"

set.seed(123)
indices_entrenamiento <- sample(1:nrow(penguins), round(0.8 * nrow(penguins)), replace = FALSE)
datos_entrenamiento <- penguins[indices_entrenamiento, ]
datos_prueba <- penguins[-indices_entrenamiento, ]

combinaciones <- lapply(1:7, function(i) t(combn(predictors, i)))

# Lista para almacenar resultados
resultados <- list()

# Iterar sobre las combinaciones y ajustar/evaluar modelos
for (i in 1:7) {
  for (j in 1:ncol(combinaciones[[i]])) {
    # Variables predictoras para el modelo actual
    predictoras <- combinaciones[[i]][, j]
    
    # Ajustar modelo
    modelo <- rpart(paste("species ~", paste(predictoras, collapse = " + ")), 
                    data = datos_entrenamiento)
    
    # Evaluar modelo en datos de prueba
    predicciones <- predict(modelo, newdata = datos_prueba, type = "class")
    confusion_matrix <- table(predicciones, datos_prueba$sex)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    # Almacenar resultados
    resultados[[paste(predictoras, collapse = ", ")]] <- accuracy
  }
}

# Encontrar la mejor combinación de variables predictoras
mejor_combinacion <- names(resultados)[which.max(unlist(resultados))]
mejor_combinacion

"Observamos que tenemos la mejor accuracy con el modelo species ~ sex, que es de una sola variable
predictora!. Esto se podria deberse a que el sexo del punguino esté fuertemente correlacionado con la especie 
del pinguino.
Sin embargo, tuvo una pobre accuracy del 0.418.
Probemos usando el metodo k-nn"

set.seed(123)
indices_entrenamiento <- sample(1:nrow(penguins), round(0.8 * nrow(penguins)), replace = FALSE)
datos_entrenamiento <- penguins[indices_entrenamiento, ]
datos_prueba <- penguins[-indices_entrenamiento, ]

datos_entrenamiento$sex <- as.integer(factor(datos_entrenamiento$sex))
datos_entrenamiento$island <- as.integer(factor(datos_entrenamiento$island))
datos_entrenamiento$year <- as.integer(factor(datos_entrenamiento$year))

datos_prueba$sex <- as.integer(factor(datos_prueba$sex))
datos_prueba$island <- as.integer(factor(datos_prueba$island))
datos_prueba$year <- as.integer(factor(datos_prueba$year))

predictors <- c("flipper_length_mm", "body_mass_g", "bill_length_mm", "bill_depth_mm", "sex", "island", "year")

combinaciones <- lapply(1:7, function(i) t(combn(predictors, i)))

resultados <- list()
target <- "species"

for (i in 1:7){
  
  for (j in 1:ncol(combinaciones[[i]])){
    
    predictoras <- combinaciones[[i]][, j]
    
    error_data <- data.frame(k = numeric(), error = numeric())
    
    for (k in seq(1, 20, by = 2)){
      knn_model <- knn(train = datos_entrenamiento[predictoras], 
                       test = datos_prueba[predictoras], 
                       cl = datos_entrenamiento[[target]], 
                       k = k)
      
      error <- 1 - sum(knn_model == datos_prueba[[target]]) / nrow(datos_prueba)
      error_data <- rbind(error_data, data.frame(k = k, error = error))
    }
    
    optimal_k <- error_data[which.min(error_data$error), "k"]
    
    optimal_knn_model <- knn(train = datos_entrenamiento[predictoras], 
                             test = datos_prueba[predictoras], 
                             cl = datos_entrenamiento[[target]], 
                             k = optimal_k)
    
    accuracy <- sum(optimal_knn_model == datos_prueba[[target]]) / nrow(datos_prueba)
    
    resultados[[paste(predictoras, collapse = "," )]] <- accuracy
  }
}
mejor_combinacion <- names(resultados)[which.max(unlist(resultados))]
mejor_combinacion



