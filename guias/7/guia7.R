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





