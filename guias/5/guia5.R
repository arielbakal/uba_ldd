require(dplyr)

set.seed(1010)
aux = rchisq(100, df=2)

dfEstudiantes = tibble(legajo = paste0("LE_",1:100), 
                       edad = as.integer((80-18) * aux/max(aux) + 18),
                       carrera = sample(c("Ciencias Físicas",
                                          "Ciencias Matemáticas",
                                          "Ciencias de Datos",
                                          "Paleontología",
                                          "Ciencias Biológicas",
                                          "Ciencias de la Atmósfera",
                                          "Ciencias de la Computación",
                                          "Ciencias Geológicas",
                                          "Ciencias Químicas",
                                          "Ciencia y Tecnología de Alimentos",
                                          "Oceanografía"),
                                        100, replace = T))

dfNotas = rbind(tibble(legajo = sample(paste0("LE_",1:50),30),
                       nota = as.integer(runif(30,min=2,max=10)),
                       materia = "Biología"),
                tibble(legajo = sample(paste0("LE_",1:100),50),
                       nota = as.integer(runif(50,min=2,max=10)),
                       materia = "Matemática"))

# 1.1
notas_biologia <- dfNotas %>% filter(materia == "Biología")
mean(notas_biologia$nota)

# 1.2
dfEstudiantes %>%
  anti_join(dfNotas, by = "legajo") %>%
  nrow()

# 1.3
estudiantes_rendidores <- dfEstudiantes %>%
                            inner_join(dfNotas, by = "legajo") %>%
                            distinct(legajo, .keep_all = TRUE) 
mean(estudiantes_rendidores$edad)

# 1.4
require(ggplot2)

dfEstudiantes %>% 
  mutate(edadCat = cut_number(edad, 3)) %>%
  inner_join(dfNotas, by = "legajo") %>%
  group_by(materia, edadCat) %>%
  ggplot(aes(x = edadCat, y = nota, color = materia)) +
  geom_boxplot()

# 2.1
rango <- function(v) {
  return(c(min(v),max(v)))
}
rango(c(5,1))
range(c(1,5))

# 2.2
media <- function(v) {
  return(sum(v)/length(v))
}
media(c(1,2,3,4))
mean(c(1,2,3,4))

# 2.3
proporcion.na <- function(v) {
  return(sum(is.na(v))/length(v))
}

vector <- c(1,2,NA)
proporcion.na(vector)

# 2.4
convertir.na <- function(v) {
  return(replace(v, is.na(v), -1))
}

convertir.na(vector)

# 2.5
convertir.a.na <- function(v, x) {
  return(replace(v, which(v == x), NA))
}

vector <- c(1,2,4,1)
convertir.a.na(vector, 2)

# 2.6
proporcion.suma <- function(v) {
  return(round(v / sum(v, na.rm = TRUE) * 100, 1))
}

proporcion.suma(vector)

# 2.7
ambos_na <- function(v, u) {
  if (length(v) != length(u)) {
    return(print("Listas de distinta longitud"))
  }
  return( which(is.na(v) & is.na(u)) )
}
vector1 <- c(NA,1,2,NA,NA)
vector2 <- c(NA,2,4,4,NA)
vector3 <- c(1,2,4)
  
ambos_na(vector1, vector2)
ambos_na(vector1, vector3)

# 2.8
graficar.dispersion.recta <- function(d) {
  return(ggplot(d, aes(x,y)) + 
           geom_point() + 
           geom_smooth(method = "lm", linewidth = 2, se = FALSE))
}

puntos <- data.frame(x = c(1,2,3,4,5,6), y = c(1,4,5,2,7,4))

graficar.dispersion.recta(puntos)

# 2.9
mayorigual.a.n <- function(lst, n) {
  length(lst) >= n
}

mayorigual.a.n(c(1,2),1)

# 2.11

select.na <- function(d, column) {
  return(d %>% filter(is.na(column)))
}

df <- data.frame(x = c(1,NA,3,4,5,6), y = c(1,4,5,2,7,4))

select.na(df, "x")

# TODO

# 2.12
fsda <- function(df, column) {
  
  p <- df$column
  
  v <- c(mean(p), median(p), min(p), max(p))
  
  return(v)
}

fsda(df, y)

# TODO

# 2.13

# TODO

# 2.14
graficar.hex <- function(df) {
  ggplot(df, aes(x, y)) + geom_hex()
}

puntos <- data.frame(x = c(1,1,1,2,3,4,5,6), y = c(1,1,1,4,5,2,7,4))

graficar.hex(puntos)

# 3.1
i <- 1
while( 2**i < 1000 ) {
  i <- i + 1
}
i

# 3.2
suma <- 0
i <- 1
while( i * 7 < 100 ) {
  i <- i * 7
  suma <- suma + i
}
suma

# 3.3
poblacion <- 100
años <- 0
while( poblacion < 1000 ) {
  poblacion <- poblacion * 2
  años <- años + 1
}
años
  
# 3.4

# TODO

