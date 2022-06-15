# Clase 08 ------

# firacode
library(tidyverse)
library(datos)
library(patchwork)
library(ggcorrplot)

# Repaso
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

aux <- anime2 %>% 
  group_by(genre) %>% 
  nest() %>% 
  mutate(Resumen = map(data,resumen),
         ANOVA   = map(data,.f = function(x){aov(rating ~ type, data = x)}),
         GRAFICO = map2(data,genre,my_grafico))


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
  
saveRDS(aux, 'base de resumen.RDS')

perrito <- readRDS('base de resumen.RDS')

