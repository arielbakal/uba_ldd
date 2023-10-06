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
penguins %>% 
  group_by(species) %>%
  mutate(largo_est = coeficientes[1] + coeficientes[2] * bill_depth_mm) %>%
  summarise(largo_est)

#1.8
penguins <- penguins %>% group_by(species)




















