require(palmerpenguins)
penguins <- na.omit(penguins)

#1
#a
"peso_est = A + B*flipper_length_mm + C*sex. Donde penguins$sex es de tipo factor"

#b
modelo_1 <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins)
coeficientes_1 <- coef(modelo_1)
print(coeficientes_1)

predicciones_1 <- predict(modelo_1, newdata = penguins)
error_1 <- sqrt(mean((penguins$body_mass_g - predicciones_1)^2))

#c
require(ggplot2)
ggplot(penguins, aes(x = bill_length_mm, y = body_mass_g, color = sex)) +
  geom_point(size = 1.5) +
  geom_line(aes(y = predicciones_1), color = "blue", size = 0.6, linetype = "dashed") +
  labs(x = "Longitud de la Aleta", y = "Peso del Pinguino") +
  ggtitle("Peso del Pinguino vs Longitud de la Aleta") +
  theme_minimal()

#d
"Como el coeficiente generado para el sexo, esta hecho en base a que (male,female)=(1,0) y
es positivo, cuando el pinguino es macho suma aquel coeficiente y cuando es hembra suma 0.
Diciendo que aquel coeficiente es la diferencia entre el peso de un macho y una hembra, que
es de 348g"

#2
#a
"peso_est = A + B*largo_aleta + C*especie 
Obs, Al incluir species como una variable en el modelo lineal, R automáticamente realiza 
una codificación dummy (también conocida como one-hot encoding) para esta variable. 
En este proceso, las diferentes categorías de la variable species se convierten en 
variables dummy binarias. Si hay k especies diferentes, habrá k−1 variables dummy.

Por ejemplo, si hay tres especies de pingüinos (A, B y C), habrá dos variables dummy: 
speciesB y speciesC. Estas variables tomarán valores de 1 o 0, indicando si un pingüino 
pertenece a la especie B o C, respectivamente. Si ambas variables dummy son 0, se deduce que 
el pingüino es de la especie A, ya que en un modelo con k categorías, solo se necesitan 
k−1 variables dummy para representar todas las categorías.

Entonces tenemos 2 variables dummy binarias, resultando nuestra ecuacion en:

peso_est = B0 + B1*long_aleta + B2*especie_chinstrap + B3*especie_gentoo"

#b
"La diferencia entre este modelo y el del ej 1, es que tomamos la especie como variable
predictora en lugar del sexo, la cual varia en 3 tipos en vez de 2 con el sexo."

#c
modelo_2 <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
coeficientes_2 <- coef(modelo_2)
print(coeficientes_2)

predicciones_2 <- predict(modelo_2, newdata = penguins)
error_2 <- sqrt(mean((penguins$body_mass_g - predicciones_2)^2))

#d
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_line(aes(y = predicciones_2), color = "blue", size = 1) +
  labs(x = "Longitud de la Aleta", y = "Peso del Pinguino", color = "Especie del Pinguino") +
  ggtitle("Peso del Pinguino vs Longitud de la Aleta y Especie") +
  theme_minimal()

#3
#a
"Podriamos iniciar pensando una ecuacion del siguiente estilo:

peso_est = A + B*long_aleta + C*especie + D*long_aleta*especie

Sin embargo como hablamos antes, R realiza una codificacion dummy. Entonces tenemos
k-1=3-1=2 variables dummy, en este caso Chinstrap y Gentoo.
Por lo tanto nuestra ecuacion seria con 6 variables:

peso_est = B0 + B1*long_aleta + B2*especie_chinstrap + B3*especie_gentoo 
          + B4*(long_aleta*especie_chinstrap) + B5*(long_aleta*especie_gentoo)

Al ser variables dummy binarias, cuando trabajemos con un pinguino chinstrap, tendremos:
especie_chinstrap = 1, especie_gentoo = 0
Para geento al revez y para Adelie ambas seran 0 cancelando los terminos correspondientes.
"

#b
"En este modelo seguimos prediciendo el peso en funcion de la longitud de la aleta y
la especie, pero tenemos una tercera variable predictora, la interaccion entre las mismas"

#c
modelo_3 <- lm(body_mass_g ~ flipper_length_mm * species, data = penguins)
coeficientes_3 <- coef(modelo_3)
print(coeficientes_3)

predicciones_3 <- predict(modelo_3, newdata = penguins)
error_3 <- sqrt(mean((penguins$body_mass_g - predicciones_3)^2))

#d
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() +
  geom_line(aes(y = predicciones_3), color = "blue", size = 1) +
  labs(x = "Longitud de la Aleta", y = "Peso del Pinguino", color = "Especie del Pinguino") +
  ggtitle("Peso del Pinguino vs Longitud de la Aleta y Especie (con Interacción)") +
  theme_minimal()

#e
"Tenemos,

peso_est_adelie = B0 + B1*long_aleta + B2*especie_chinstrap + B3*especie_gentoo 
                  + B4*(long_aleta*especie_chinstrap) + B5*(long_aleta*especie_gentoo)
peso_est_adelie = B0 + B1*long_aleta + B2*0 + B3*0 + B4*(long_aleta*0) + B5*(long_aleta*0)
peso_est_adelie = B0 + B1*long_aleta

puesto que los pinguinos Adelie tenemos nuestros dummy = 0. Luego,

peso_est_gentoo = B0 + B1*long_aleta + B2*0 + B3*1 + B4*(long_aleta*0) + B5*(long_aleta*1)
peso_est_gentoo = B0 + B1*long_aleta + B3 + B5*long_aleta


Finalmente,

peso_est_gentoo - peso_est_adelie
B0 + B1*long_aleta + B3 + B5*long_aleta - (B0 + B1*long_aleta)
B3 + B5*long_aleta

Tendran una diferencia de peso de (B3 + B5*long_aleta)"

#4
#a
set.seed(123)

total_filas <- nrow(penguins)

num_filas_entrenamiento <- round(0.8 * total_filas)

indices_entrenamiento <- sample(1:total_filas, num_filas_entrenamiento, replace = FALSE)

datos_entrenamiento <- penguins[indices_entrenamiento, ]
datos_prueba <- penguins[-indices_entrenamiento, ]

#b,c,d
resultados <- data.frame(Variables = character(0), ErrorAjusteEntrenamiento = numeric(0), ErrorPrediccionPrueba = numeric(0))

for (num_variables in 1:6) {
  # Seleccionar las primeras 'num_variables' variables predictoras
  predictoras <- c("flipper_length_mm", "bill_length_mm", "bill_depth_mm", "sex", "species", "year")[1:num_variables]
  
  # Crear la fórmula para el modelo
  formula <- paste("body_mass_g ~", paste(predictoras, collapse = "+"))
  
  # Ajustar el modelo en el grupo de entrenamiento
  modelo <- lm(formula, data = datos_entrenamiento)
  
  # Calcular el error de ajuste en el grupo de entrenamiento
  error_ajuste <- sqrt(mean((datos_entrenamiento$body_mass_g - predict(modelo, datos_entrenamiento))^2))
  
  # Calcular el error de predicción en el grupo de prueba
  error_prediccion <- sqrt(mean((datos_prueba$body_mass_g - predict(modelo, datos_prueba))^2))
  
  # Guardar los resultados en el dataframe
  resultados <- rbind(resultados, data.frame(Variables = paste(predictoras, collapse = ", "),
                                             ErrorAjusteEntrenamiento = error_ajuste,
                                             ErrorPrediccionPrueba = error_prediccion))
}

# Mostrar los resultados
print(resultados)

"A medida que agregamos variables predictoras a nuestro modelo, este se vuelve mas complejo.
Lo cual nos brinda mas flexibilidad para adaptarse a los datos de entrenamiento, por lo que
podriamos esperar que nuestro error disminuye.
Es asi el caso del modelo 6, el cual tiene 6 variables predictoras y es el que tuvo el menor 
error de ajuste.

Lo mismo podriamos decir del error de prediccion. Sin embargo, el modelo 5 fue el que tuvo menor
error de prediccion y este tiene 5 variables predictoras. Podriamos decir que complejizar el 
modelo no implica tener menor error de prediccion, sino que existe un equilibrio entre la
complejidad y el error.

En conclusion, basándonos en estos resultados, podríamos concluir que un modelo que incluye 
las variables flipper_length_mm, bill_length_mm, bill_depth_mm y sex (modelo 4) proporciona 
un buen equilibrio entre la capacidad de ajuste a los datos de entrenamiento y la capacidad 
de hacer predicciones precisas en nuevos datos (grupo de prueba). Agregar más variables no 
parece mejorar significativamente la capacidad predictiva del modelo y puede aumentar la 
complejidad del modelo sin un beneficio claro en términos de precisión de la predicción."
