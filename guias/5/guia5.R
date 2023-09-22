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
n <- 500 # numero de votos para A
m <- 400 # numero de votos para B
votos <- c()
for (i in 1:n) {votos <- append(votos, 1)}
for (i in 1:m) {votos <- append(votos, -1)}
votos <- sample(votos)

resultado <- cumsum(votos)
for (i in 1:length(resultado)) {
  if (resultado[i] == -1) {
    A_gana_siempre <- FALSE
  }
}
A_gana_siempre

cuenta <- 0
Nrep   <- 1000
for (i in 1:Nrep){
  A_gana_siempre <- TRUE
  escrutinio <- sample(votos)
  resultado  <- cumsum(escrutinio)
  for (i in 1:length(escrutinio)) {
    if (resultado[i] == -1) {
      A_gana_siempre <- FALSE
      break
    }
  }
  if (A_gana_siempre == TRUE){
    cuenta <- cuenta + 1
  }
}
p <- cuenta / Nrep
print(p)
print((n-m)/(n+m))

n <- 505 # numero de votos para A
m <- 495 # numero de votos para B
votos <- c( rep(1,n), rep(-1,m) )

# cuentaA va a contar cuántas veces ocurre que A va ganando 
# hasta contar 800 votos
cuentaA <- 0  
cuentaB <- 0  # esta hace lo mismo para B
Nrep   <- 10000
for (i in 1:Nrep){
  A_gana_siempre <- TRUE
  B_gana_siempre <- TRUE
  escrutinio <- sample(votos, n+m, replace = FALSE)
  resultado  <- cumsum(escrutinio)
  for (i in 1:800){
    if (resultado[i] == -1){
      A_gana_siempre <- FALSE
    }
    if (resultado[i] > 0 ){  
      B_gana_siempre <- FALSE
    }
  }
  if (A_gana_siempre == TRUE){
    cuentaA <- cuentaA + 1
  } else {
    cuentaB <- cuentaB + 1
  }
}

pA <- cuentaA / Nrep
pB <- cuentaB / Nrep
print(pA)
print(pB)

"que no habria votos en blancos?"

# 4.4
N  <- 10   # numero total de personas
n1 <- 4    # numero de personas que inicialmente opinan +1
Op <- c( rep(1, n1), sample(c(1,-1), N - n1, replace = TRUE))

n_int     <- sample(1:N, 3, replace = FALSE)
Op[n_int] <- ifelse(sum(Op[n_int]) == 1, 1, -1 )

Op <- c( rep(1, n1), sample(c(1,-1), N - n1, replace = TRUE))
consenso <- 0
while( consenso != 1 ){
  
  n_int     <- sample(1:N, 3, replace = FALSE)
  Op[n_int] <- ifelse(sum(Op[n_int]) > 0, 1, -1 )
  
  if (sum(Op) == length(Op)){
    consenso <- 1
    Oconsenso <- 1
  }
  if (sum(Op) == -length(Op)){
    consenso <- 1
    Oconsenso <- -1
  }
}
Oconsenso

p_consenso <- function(N, n1){
  
  output   <- 0
  
  for (i in 1:100){
    
    Op <- c( rep(1, n1), sample(c(1,-1), N - n1, replace = TRUE))
    consenso <- 0
    while( consenso != 1 ){
      
      n_int     <- sample(1:N, 3, replace = FALSE)
      Op[n_int] <- ifelse(sum(Op[n_int]) > 0, 1, -1 )
      
      if (sum(Op) == length(Op)){
        consenso <- 1
        Oconsenso <- 1
      }
      if (sum(Op) == -length(Op)){
        consenso <- 1
        Oconsenso <- -1
      }
    }
    if (Oconsenso == 1){
      output <- output + 1
    }
  }
  return(output/100)
}
p_consenso(100, 10)

p_consensos <- c()
for (i in 1:10) {p_consensos <- append(p_consensos, p_consenso(100, i))}
p_consensos

p_consensos <- data.frame(porcentajes = p_consensos)

p_consensos$iniciantes <- seq_along(p_consensos$porcentajes)

require(ggplot2)
ggplot(p_consensos) +
  geom_line(mapping = aes(x = iniciantes, y = porcentajes))

# 4.5
monty_hall <- function(n_puerta, mantiene_puerta){
  puertas <- sample(c(TRUE, FALSE, FALSE))
  
  eleccion <- puertas[n_puerta]
  
  indices_restantes <- c(1,2,3)[-eleccion_indice]
  
  if (puertas[indices_restantes[1]] == FALSE) {
    indices_restantes <- indices_restantes[-indices_restantes[1]]
  } else {
    indices_restantes <- indices_restantes[-indices_restantes[2]]
  }
  
  mantiene <- mantiene_puerta
  
  if (mantiene == TRUE){
    eleccion <- eleccion
  } else {
    eleccion <- puertas[indices_restantes]
  }
  return(eleccion)
}

monty_hall(1,FALSE)

prob_monty_hall <- function(iteraciones, mantiene_puerta){
  cuenta <- 0
  for (i in 1:iteraciones){
    puerta <- sample(c(1,2,3), size = 1)
    if (monty_hall(puerta, mantiene_puerta) == TRUE){
      cuenta <- cuenta + 1
    }
  }
  return(cuenta/iteraciones)
}
prob_monty_hall(100000, TRUE)
prob_monty_hall(100000, FALSE)


