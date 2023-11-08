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




predictors <- c("title_has_excl", "negative")
target <- "type"

error_data <- data.frame(k = numeric(), error = numeric())

for (k in seq(1, 40, by = 2)) {
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

"OBS: Tengo 0.8 de accuracy con title_has_excl y negative, y luego 0.7 con las 3 variables 
(el mismo que con rpart)"

"TODO: Probar diferentes parametros"











