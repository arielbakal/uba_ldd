load("tp2.RData")

require(ggplot2)
require(class)
require(rpart)
require(rpart.plot)
#1
ggplot(data = fake_news, aes(x = type)) +
         geom_bar()

ggplot(data = fake_news, aes(x = type, y = negative)) +
  geom_col()

"Los real_news tienen mayor porcentaje de palabras negativas en el titulo"

ggplot(data = fake_news, aes(x = title_has_excl, color = type)) +
  geom_bar()

"Los fake_news tienden a tener signos de exclamacion"

ggplot(data = fake_news, aes(x = negative, color = type)) +
  geom_bar()

sum(fake_news$type == "real" & fake_news$title_words)
sum(fake_news$type == "fake")

#2
set.seed(123)
indices_entrenamiento <- sample(1:nrow(fake_news), round(0.8 * nrow(fake_news)), replace = FALSE)
datos_entrenamiento <- fake_news[indices_entrenamiento, ]
datos_prueba <- fake_news[-indices_entrenamiento, ]

fit <- rpart("type ~ title_has_excl + negative + title_words", data = fake_news)

preds <- predict(fit, newdata = datos_prueba, type = "class")

confusion_matrix <- table(preds, datos_prueba$type)

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

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

rpart.plot(fit)

"Accuracy de 0.7 con rpart"




# Define the complexity parameter (cp) values to be tuned
cp_values <- c(0.001, 0.01, 0.1, 0.2, 0.5)

# Initialize a vector to store accuracy values for different cp values
accuracy_values <- numeric(length(cp_values))

# Perform cross-validation with different cp values
for (i in 1:length(cp_values)) {
  fit <- rpart(type ~ title_has_excl + negative + title_words, 
               data = datos_entrenamiento, 
               method = "class", 
               control = rpart.control(cp = cp_values[i]))
  
  preds <- predict(fit, newdata = datos_prueba, type = "class")
  
  # Calculate accuracy
  accuracy <- sum(preds == datos_prueba$type) / nrow(datos_prueba)
  accuracy_values[i] <- accuracy
}

# Get the best cp value
best_cp <- cp_values[which.max(accuracy_values)]

# Build the final model with the best cp value
fit <- rpart(type ~ title_has_excl + negative + title_words, 
                     data = datos_entrenamiento, 
                     method = "class", 
                     control = rpart.control(cp = best_cp))

rpart.plot(fit)

"Accuracy del 0.73 con rpart y mejor cp value"


params <- list(cp = c(0.001, 0.01, 0.1, 0.2, 0.5),
               minsplit = c(2, 5, 10),
               minbucket = c(1, 5, 10),
               maxdepth = c(3, 5, 7),
               xval = 5) # Number of cross-validations

# Initialize a vector to store accuracy values for different parameter combinations
accuracy_values <- numeric(length(params$cp) * length(params$minsplit) * length(params$minbucket) * length(params$maxdepth))

# Perform cross-validation with different parameter combinations
i <- 1
for (cp in params$cp) {
  for (minsplit in params$minsplit) {
    for (minbucket in params$minbucket) {
      for (maxdepth in params$maxdepth) {
        fit <- rpart(type ~ title_has_excl + negative + title_words, 
                     data = datos_entrenamiento, 
                     method = "class", 
                     control = rpart.control(cp = cp, minsplit = minsplit, minbucket = minbucket, maxdepth = maxdepth))
        
        preds <- predict(fit, newdata = datos_prueba, type = "class")
        
        # Calculate accuracy
        accuracy <- sum(preds == datos_prueba$type) / nrow(datos_prueba)
        accuracy_values[i] <- accuracy
        i <- i + 1
      }
    }
  }
}


"maximo accuracy 0.73, no sirve probar todo tipo de parametros" 



predictors <- c("title_has_excl", "negative")
target <- "type"

error_data <- data.frame(k = numeric(), error = numeric())

for (k in seq(1, 40, by = 2)) {
  knn_model <- knn(train = datos_entrenamiento[predictors], 
                   test = datos_prueba[predictors], 
                   cl = datos_entrenamiento[[target]], 
                   k = k,
                   prob = TRUE)
  
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
                         k = optimal_k,
                         prob = TRUE)

accuracy <- sum(optimal_knn_model == datos_prueba[[target]]) / nrow(datos_prueba)
accuracy

"OBS: Tengo 0.8 de accuracy con title_has_excl y negative, y luego 0.7 con las 3 variables 
(el mismo que con rpart)"

"TODO: Probar diferentes parametros"











