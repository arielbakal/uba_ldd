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

select.na <- function(dataframe, column) {
  filter_dataframe <- subset(dataframe, is.na(dataframe[[column]]))
  return(filter_dataframe)
}

df <- data.frame(x = c(1,NA,3,4,5,6), y = c(1,4,5,2,7,4))

select.na(df, "x")

# 2.12
summary.stats <- function(data_frame, column_name) {
  summary_stats <- data_frame %>%
    summarise(
      Media = mean({{column_name}}, na.rm = TRUE),
      Mediana = median({{column_name}}, na.rm = TRUE),
      Mínimo = min({{column_name}}, na.rm = TRUE),
      Máximo = max({{column_name}}, na.rm = TRUE)
    )
  
  return(summary_stats)
}

calculate_summary_stats(df, x)

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
is_prime <- function(number) {
  if (number <= 1) {
    return(FALSE)
  }
  if (number <= 3) {
    return(TRUE)
  }
  if (number %% 2 == 0 || number %% 3 == 0) {
    return(FALSE)
  }
  i <- 5
  while (i * i <= number) {
    if (number %% i == 0 || number %% (i + 2) == 0) {
      return(FALSE)
    }
    i <- i + 6
  }
  return(TRUE)
}

largest_prime <- 0

for (i in 100:2) {
  if (is_prime(i)) {
    largest_prime <- i 
    break
  }
}

largest_prime

# 4.1
suma.tiradas <- function(n) {
  caras <- c(1,2,3,4,5,6)
  probabilidades <- c(1/6,1/6,1/6,1/6,1/6,1/6)
  tiradas <- sample(caras, size = n, prob = probabilidades, replace = TRUE)
  return(sum(tiradas))
}
resultados <- suma.tiradas(10)
for (i in 2:10000) {
  suma <- suma.tiradas(10)
  resultados <- append(resultados, suma)
}
resultados <- data.frame(numero = resultados)

ggplot(resultados, aes(x = numero)) +
  geom_histogram()

# 4.2
N <- 50
cumples <- sample(1:365, N, replace = TRUE)

unique(cumples)

# variable que va a contar las coincidencias
n_coincidencias <- 0

# simula 10000 grupos de N=50 personas y verifica si hubo coincidencias o no
Nrep <- 10000
for(i in 1:Nrep){
  cumples <- sample(1:365, N, replace = TRUE)
  if(length(unique(cumples)) < N){
    n_coincidencias <- N - length(unique(cumples))
  } 
}

# calcula la probabilidad estimada de coincidencias y la imprime en la consola
p_coincidencias <- n_coincidencias / Nrep
print(p_coincidencias)

pcumples <- function(N=50, Nrep=10000){
  cumples <- sample(1:365, N, replace = TRUE)
  for(i in 1:Nrep){
    cumples <- sample(1:365, N, replace = TRUE)
    if(length(unique(cumples)) < N){
      n_coincidencias <- N - length(unique(cumples))
    } 
  }
  p_coincidencias <- n_coincidencias / Nrep
  resultado <- 1 - p_coincidencias
  return(resultado)
}

pcumples()

pbirthday(N, classes = 365, coincident = 2)

grupos <- 1:100
p_c <- c()

for (i in 1:length(grupos)) {
  p_c <- append(p_c, pbirthday(grupos[i], classes = 365, coincident = 2))  
}

p_c <- data.frame(Prob = p_c)

p_c$Grupo <- seq_along(p_c$Prob)

ggplot(p_c) +
  geom_line(mapping = aes(y = Prob, x = Grupo))

# 4.3

















