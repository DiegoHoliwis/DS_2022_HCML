# Clase 08 ------

# firacode
library(tidyverse)
library(datos)
library(patchwork)
library(ggcorrplot)

# Repaso ----
getwd() # Estoy parado en la ruta de mi proyecto
setwd('Clase 08') # Cambio mi ruta a la carpeta clase 08

# Lectura desde Internet
anime <- read_csv2('https://raw.githubusercontent.com/DiegoHoliwis/DS_2022_HCML/main/Clase%2008/anime.csv')

# read_csv() # Asume que tiene separación por ,
# read_csv2()  # Asume que tiene separación por ;

anime <- read_csv2('anime.csv')

# read_csv(file,  locale = locale(encoding = "Latin1"))
# data.table::fread(ecoding = 'Latin1')

anime %>% 
  glimpse()

anime <- anime %>% 
  filter(episodes != 'Unknown')


anime <- anime %>% 
  mutate(episodes = as.numeric(episodes))

anime %>% 
  glimpse()


anime2 <- anime %>% 
  mutate(genre = str_split(genre, pattern = ',')) %>% 
  unnest(genre) %>% 
  mutate(genre = str_trim(genre)) # str_trim elimina los espacios al principio y al final

# 'Romantico, acción' %>% str_split(',')

anime2

resumen <- function(x){
  x %>% 
    summarise(numero_anime = n(),
              prom_rating = mean(rating, na.rm = TRUE),
              prom_members = mean(members, na.rm = TRUE),
              prom_episodios = mean(episodes, na.rm = TRUE))
}


my_grafico <- function(x,y){
  x %>% 
    ggplot(aes(x = rating)) +
    ggtitle(paste0(y)) + 
    geom_histogram(fill = 'skyblue', color = '#FFFFFF')
}

resumen(anime2)
my_grafico(anime2)

aux <- anime2 %>%  # Selecciono la base de datos
  group_by(genre) %>% # Creamos los grupos para la utilización del nest
  nest() %>%          # comprimimos la base de datos usando nenst
  mutate(Resumen = map(data,resumen), # Generamos un resumen para cada conjunto de datos
         ANOVA   = map(data,.f = function(x){aov(rating ~ type, data = x)}), # Creamos un modelo anova para cada conjunto
         GRAFICO = map2(data,genre,my_grafico)) # Creamos un gráfico para cada conjunto de datos

aux %>% 
  filter(genre == 'Action') %>% 
  pull(Resumen)
  

aux %>% 
  filter(genre == 'Action') %>% 
  pull(ANOVA) %>% 
  .[[1]] %>%
  summary()


aux %>% 
  filter(genre == 'Drama') %>% 
  pull(GRAFICO)

# Select selecciona la columna y retorna el tibble
aux %>% 
  select(genre)

# pull selecciona los valores que tiene la columna y retorna dicho valor
aux %>% 
  pull(genre)

  
saveRDS(aux, 'base de resumen.RDS')

perrito <- readRDS('base de resumen.RDS')


mylista <- list(1:10,11:20)

mylista[[1]][4]

# Gráficos con ggplot 2 -----

base_chile = paises %>% 
  filter(pais=="Chile")
base_vecinos = paises %>% 
  filter(pais %in% c("Chile","Perú","Bolivia","Argentina"))

df_rating <- anime %>% 
  group_by(episodes) %>% 
  summarise(rating = mean(rating, na.rm = TRUE), .groups = 'drop')


# R base

plot(x = df_rating$episodes, 
     y = df_rating$rating,
     type = 'l',   # tipo de gráfico
     col = 'red',  # Color
     lwd = 2,      # Grosor
     xlab = 'episodios',  # Nombre eje x
     ylab = 'Rating promedio', # Nombre eje y
     main = 'Rating promedio por número de episodios') # titulo
 
# ggplot2

df_rating %>% 
  ggplot()

df_rating %>%
  ggplot(aes(x = episodes, y = rating)) +
  geom_line(color = 'red', size = 1.5) +
  labs(title = 'Rating promedio por número de episodios',
       subtitle = 'Información extraida al 2019',
       x  = 'Episodios',
       y  = 'Rating promedio')

# histograma con r base

hist(anime$rating,
     breaks = 60,
     col = rainbow(60))


anime %>% 
  filter(!is.na(rating)) %>% 
  ggplot(aes(x = rating)) +
  geom_histogram(bins = 60, fill = '#40D94C', color = '#FFFFFF')
