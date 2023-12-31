# 1.1 
"Categoricas = Color, Nombre, Pais | Numericas = Edad, Peso, Altura"

# 1.2 
"gdpPercap es una variable numerica continua porque toma valores reales"

# 1.3
"I.gdp es numerica discreta porque toma valores enteros. 
Si, pasaria a ser categorica ordinal, ya que establecemos cierto orden jerarquico."

# 1.4
require(gapminder)

gapminder_2002 <- subset(gapminder, year == 2002)

continent_table <- table(gapminder_2002$continent)
continent_prop <- prop.table(continent_table)

# 1.5
I <- ifelse(gapminder_2002$gdpPercap > 2000, 1, 0)
continent_table2 <- table(I, gapminder_2002$continent)
print(continent_table2)

# 1.6 
colores <- c('blue', 'red', 'green', 'red', 'black', 'yellow','blue','blue')
colores <- as.factor(colores)
levels(colores)

# 1.7
"
Media, suma de los valores sobre su cantidad.
Mediana, es el numero que representa la mitad de un set de datos ordenado.
Moda, es el numero con mas frecuencia del set, pueden haber varias modas.
"

# 1.8
moda <- function(x) {
  x <- table(x)
  result <- names(which.max(x))
  return(result)
}
moda(colores)

# 1.9
gapminder_1952 <- subset(gapminder, year == 1952)
life_expectation_1952 <- gapminder_1952$lifeExp
media_lifeExp <- mean(life_expectation_1952)
mediana_lifeExp <- median(life_expectation_1952)

# 1.10
"
Desvio estandar, es la dispersion de los valores respecto del valor medio.
El numerador al cuadrado permite trabajar con valores positivos y
dar mas peso a las diferencias mas grandes.
"
desvio_estandar <- function(x) {return( sqrt(mean((x - mean(x))^2)) )}
desvio_estandar(life_expectation_1952)
sd(life_expectation_1952)
"
Segun la documentacion, sd() utiliza (n-1) como denominador. 
Es por eso que obtenemos un valor mas alto pues el denominador es menor.
"

# 1.11
rango <- function(x) {return( max(x)-min(x) )}
rango(life_expectation_1952)
desvio_estandar(life_expectation_1952)

require(ggplot2)
require(gapminder)

# 2.1
"Haria un bar plot porque quiero comparar valores de distintas categorias"

# 2.2
"Edad y Peso, histograma | Sexo, diagrama de pastel"
"Para visualizar la relacion entre peso y altura, haria un grafico de dispersion donde mis ejes serian peso y altura. 
Luego asi, podria analizar la existecia de algun patron o tendencia."

# 2.3
gapminder_2002 <- subset(gapminder, year == 2002)

ggplot(gapminder_2002, aes(x = continent)) +
  geom_bar() + 
  labs(x = "Continente", y = "Cantidad de Paises") +
  ggtitle("Cantidad de Paises por Continente")

# 2.4
p <- ggplot(gapminder, aes(x = continent, y = lifeExp)) +
       geom_boxplot() 
p

# 2.5
ggplot(gapminder, aes(x = year, y = lifeExp)) +
  geom_line(aes(group = country)) +
  geom_smooth(method = "lm", linewidth = 2, se = FALSE) +
  facet_wrap(~continent)    
  

















