# Librerias
require(dplyr)

# Importacion de Datos
data_clima_2022 <- read.csv("data/clima_aeroparque_2022.csv")
data_bici_2022 <- read.csv("data/trips_2022_reducido.csv")

# Preprocesado de Datos
data_bici_2022 <- data_bici_2022 %>% filter(duracion_recorrido >= 300 & duracion_recorrido <= 3600)

#
data_bici_2022 %>% 
  group_by(Género) %>% 
  count(Género)
"los hombres"

data_bici_2022 %>%
  select(nombre_estacion_origen, nombre_estacion_destino, duracion_recorrido) %>%
  group_by(nombre_estacion_origen, nombre_estacion_destino) %>%
  summarise(count = n(), promedio_viaje = mean(as.numeric(duracion_recorrido), na.rm = TRUE)) %>% 
  arrange(desc(count))

  
  
