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
  mutate(genre = str_split(genre, pattern = ',')) %>% View()
  unnest(genre) %>% 
  mutate(genre = str_trim(genre)) # str_trim elimina los espacios al principio y al final



'Romantico, acción' %>% str_split(',')



