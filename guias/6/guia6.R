require(ggplot2)
require(dplyr)
require(palmerpenguins)

# 1.1
penguins <- na.omit(penguins)

#1.2
ggplot(penguins) +
  geom_point( mapping = aes(x = bill_length_mm, y = bill_depth_mm)) 

#1.3
coeficientes <- coef(lm(bill_length_mm ~ bill_depth_mm, data = penguins))
print(paste("largo_est =", coeficientes[1], "+", coeficientes[2], "x ancho"))

#1.4
"Tenemos 54.89 de coeficiente independiente y -0.635 de pendiente. Al tener una pendiente negativa,
podemos decir que a mayor sea el ancho del pico, menor sera nuestro largo"

#1.5
largo_est <- (coeficientes[1] + coeficientes[2] * penguins$bill_depth_mm)

mse_r2 <- function(original, prediccion) {
  mse <- (1/length(original)) * sum((original - prediccion)**2)
  original_promedio <- (1/length(original)) * sum(original)
  variabilidad_explicada <- sum((original - prediccion)**2)
  variabilidad_total <- sum((original - original_promedio)**2)
  r2 <- variabilidad_explicada / variabilidad_total
  return(c(mse, r2))
}

mse_r2(penguins$bill_length_mm, largo_est)

#1.6
largo_pico <- coeficientes[1] + coeficientes[2] * 20
largo_pico_bebe <- coeficientes[1] + coeficientes[2] * 5
"Este modelo no es adecuado para casos particulares como lo son los bebes, el modelo esta
hecho en base a pinguinos adultos"

#1.7
penguins_est <- penguins %>% 
                  group_by(species) %>%
                  mutate(largo_est = coeficientes[1] + coeficientes[2] * bill_depth_mm) 

#1.8
penguins_group_species <- penguins %>% group_by(species)

ggplot(penguins_group_species, aes(x = bill_depth_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method="lm", se = F)

ggplot(penguins_est, aes(x = bill_depth_mm, y = largo_est, color = species)) +
  geom_point() 

#1.9
penguins_adelie <- penguins %>% filter(species == "Adelie")

mean_bill_depth <- mean(penguins_adelie$bill_depth_mm) 

center_depth <- penguins_adelie$bill_depth_mm - mean_bill_depth

coeficientes <- coef(lm(bill_length_mm ~ center_depth, data = penguins_adelie))

est_length <- coeficientes[1] + coeficientes[2] * center_depth

#1.10
"mi ordenada al origen es 38.82 y luego mi pendiente 0.842. Tengo una pendiente positiva, entonces, 
a mayor ancho centrado, mayor largo"

#1.11
coeficientes <- coef(lm(bill_length_mm ~ bill_depth_mm, data = penguins_adelie))

est_length_normal <- (coeficientes[1] + coeficientes[2] * penguins_adelie$bill_depth_mm)

mse_r2(penguins_adelie$bill_length_mm, est_length)
mse_r2(penguins_adelie$bill_length_mm, est_length_normal)
"Tenemos iguales R2 (0.8511). Esto se debe a que ambos modelos explican la misma variabilidad 
en las diferencias individuales en el largo del pico en relacion con la media del ancho del pico"


#2.1
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue") +
  ggtitle("Relación entre Potencia del Motor y Eficiencia (mpg)") +
  xlab("Potencia del Motor (hp)") +
  ylab("Eficiencia (mpg)") +
  theme_minimal()

"Se observa claramente que a mayor potencia tenemos menos eficiencia"

#2.2
coeficientes <- coef(lm(mpg ~ hp, data = mtcars))

mpg_est <- coeficientes[1] + coeficientes[2] * mtcars$hp

mse_r2(mtcars$mpg, mpg_est)

"Tenemos un R2 = 0.398, y como sabemos que el R2 representa que tan bien el modelo se ajusta a los
datos reales, podemos decir que no es adecuado este modelo para describir la relacion"

#3.1
coeficientes <- coef(lm(Sepal.Length ~ Sepal.Width, data = iris))

sepal_length_est <- coeficientes[1] + coeficientes[2] * iris$Sepal.Width

mse_r2(iris$Sepal.Length, sepal_length_est)

"Tenemos R2 = 0.986. Este modelo se ajusta con solo 1.4% de error!, podemos decir que es un buen modelo."

#4.1
require(gapminder)

gapminder_1997 <- gapminder %>% filter(year == 1997)

ggplot(gapminder_1997, aes(x = lifeExp, y = gdpPercap)) +
  geom_point() +
  ggtitle("Relacion entre La Esperanza de Vida y el PBI per Capita") +
  xlab("Esperanza de Vida (lifeExp)") +
  ylab("PBI per Capita (gdpPercap)") +
  theme_minimal()

#4.2
gapminder_1997_america <- gapminder_1997 %>% filter(continent == "Americas")

coeficientes <- coef(lm(lifeExp ~ gdpPercap, data = gapminder_1997_america))

lifeExp_est <- coeficientes[1] + coeficientes[2] * gapminder_1997_america$gdpPercap

#4.3
mse_r2(gapminder_1997_america$lifeExp, lifeExp_est)

"R2 = 0.63"

#4.4
see <- function(model) {
  residuos <- residuals(model)
  
  sse <- sum(residuos^2)
  
  n <- length(residuos)
  k <- length(coef(model))
  
  see <- sqrt(sse / (n - k))
  
  return(see)
}

see(lm(lifeExp ~ gdpPercap, data = gapminder_1997_america))

"see = 3.97. Es decir, en promedio, las predicciones del modelo tienen un error típico de
aprox 3.97 unidades en la variable de respuesta"

#4.5
coeficientes <- coef(lm(log(lifeExp) ~ gdpPercap, data = gapminder_1997_america))

log_lifeExp_est <- coeficientes[1] + coeficientes[2] * gapminder_1997_america$gdpPercap

mse_r2(log(gapminder_1997_america$lifeExp), log_lifeExp_est)

see(lm(log(lifeExp) ~ gdpPercap, data = gapminder_1997_america))

"Nuestro see bajo a 0.006. Esto se debe a que, como vimos en el grafico de dispersion anterior, la
relacion es exponencial y no lineal. Al tomar el logaritmo, se puede hacer que las diferencias 
porcentuales se traduzcan en cambios constantes en los logaritmos, lo que puede facilitar la 
interpretación y mejorar la linealidad en el modelo"
