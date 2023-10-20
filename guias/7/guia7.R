require(palmerpenguins)
penguins <- na.omit(penguins)

#1
#a
"peso_est = A*flipper_length_mm + B*sex + C . penguins$sex es de tipo factor"

#b
modelo <- lm(body_mass_g ~ flipper_length_mm + sex, data = penguins)
coeficientes <- coef(modelo)
print(coeficientes)

peso_est_1 = coeficientes[1] + coeficientes[2]*penguins$flipper_length_mm + coeficientes[3]*ifelse(penguins$sex == "male", 1, 0) 

peso_error_1 = penguins$body_mass_g - peso_est_1

#c
require(ggplot2)

#d
"Como el coeficiente generado para el sexo, esta hecho en base a que (male,female)=(1,0) y
es positivo, cuando el pinguino es macho suma aquel coeficiente y cuando es hembra suma 0.
Diciendo que aquel coeficiente es la diferencia entre el peso de un macho y una hembra, que
es de 348g"

#2
#c
modelo <- lm(body_mass_g ~ flipper_length_mm + species, data = penguins)
matriz_modelo <- model.matrix(~ flipper_length_mm + species, data = penguins)

coeficientes <- coef(modelo)
print(coeficientes)

peso_est_2 = as.list(matriz_modelo %*% coeficientes)

peso_error_2 = penguins$body_mass_g - peso_est_2

#3
modelo <- lm(body_mass_g ~ flipper_length_mm*species, data = penguins)
matriz_modelo <- model.matrix(~ flipper_length_mm*species, data = penguins)

coeficientes <- coef(modelo)
print(coeficientes)

peso_est_3 = as.list(matriz_modelo %*% coeficientes)

peso_error_3 = penguins$body_mass_g - peso_est_3






