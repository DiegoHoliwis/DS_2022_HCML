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


# ggplot2 por capas 

anime %>% 
  ggplot()

anime %>% 
  ggplot(aes(x = type))

anime %>% 
  ggplot(aes(x = type)) + 
  coord_polar()

anime %>% 
  ggplot(aes(x = type)) + 
  coord_polar() +
  geom_bar(aes(fill = type))

anime %>% 
  ggplot(aes(x = type)) + 
  coord_polar() +
  geom_bar(aes(fill = type))

anime %>% 
  ggplot(aes(x = type)) + 
  geom_bar(fill = c('#000000','#000000','#000000',
                    '#000000','#000000','#40D94C')) +
  labs(title = 'Mi gráfico Tenpo',
       x = 'tipo',
       y = '') +
  theme_minimal() +
  theme(plot.title = element_text(color = '#40D94C',
                                  size = 40)) +
  coord_polar()

ggplot() +
  geom_histogram(data = anime %>% filter(type == 'OVA'),
                 aes(x = rating), fill = 'red', bins = 30, alpha = 0.5) +
  geom_histogram(data = anime %>% filter(type == 'TV'),
                 aes(x = rating), fill = 'skyblue', bins = 30, alpha = 0.5) +
  theme_minimal() +
  labs(title = 'Distribución OVA vs TV')



ggplot() +
  geom_density(data = anime %>% filter(type == 'OVA'),
                 aes(x = rating), fill = 'red', alpha = 0.5) +
  geom_density(data = anime %>% filter(type == 'TV'),
                 aes(x = rating), fill = 'skyblue', alpha = 0.5) +
  theme_minimal() +
  labs(title = 'Distribución OVA vs TV')



ggplot(data = paises, mapping = aes(x = pib_per_capita, 
                                    y = esperanza_de_vida)) +
  geom_point(shape = 4, size = 8, 
             color = "black",  fill = "#c5c5ff")



anime %>% 
  filter(!is.na(rating),!is.na(type)) %>% 
  ggplot(aes(y = type, x = rating)) +
  geom_boxplot() +
  geom_violin(aes(fill = type), alpha = .5) +
  theme(axis.text.y = element_text(angle = 45,
                                   size = 10,
                                   color = '#40D94C')) +
  # stat_summary(fun.data = 'rating', color = 'red')
  stat_summary(fun = 'mean', color = 'red', size = 0.5)

base_chile %>% 
  mutate(esperanza_de_vida = round(esperanza_de_vida,0)) %>% 
  ggplot(aes(x = anio, y = esperanza_de_vida)) + 
  geom_line(color = 'skyblue') +
  geom_point(color = 'skyblue') +
  geom_text(aes(label = esperanza_de_vida), vjust= -.5) + 
  geom_hline(yintercept = 65) + 
  geom_hline(yintercept = 72) +
  labs(title = "Esperanza de vida en Chile", 
       subtitle = "Años 1952 a 2007",
       x = "Año",
       y = "Esperanza de vida") +
  theme_minimal() +
  theme(axis.ticks = element_blank())


ggplot(data = paises, 
       aes(x = esperanza_de_vida, y = pib_per_capita)) +
  geom_point(aes(size = poblacion, color = continente), alpha = 0.7)

ggplot(data = base_vecinos, 
       aes(x = anio, y = esperanza_de_vida, color = pais) ) +
  geom_point(size = 3) + geom_line(size = 1) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.background = element_rect(fill="#40D94C"))
  

# options(scipen = 999) Elimina la notación cientifica


ggplot(data=mtautos, aes(x=peso, y=millas, color = factor(cilindros) )) +
  geom_point(size = 8) +
  scale_color_manual(name = 'Cilindros' ,values = c("#c5c5ff","#ffc5c5","#c5ffc5"))




