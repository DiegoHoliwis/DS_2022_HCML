# Clase numero 9 HCML -----

# https://r-graph-gallery.com/all-graphs Muchos gráficos :D

library(tidyverse)
library(datos)
library(patchwork) # Permite visualizar varios gráficos al mismo tiempo
library(showtext)  # Permite añadir fond
showtext_auto()    # Activa los nuevos fonds
library(ggcorrplot)
library(gganimate)
# library(plotly)


windowsFonts()

font_add_google('Fascinate','Fascinate')
font_add_google('Alfa Slab One','Alfa Slab One')


# Repaso clase 8 ----

anime <- read_csv2('anime.csv')
anime2 <- anime %>% 
  mutate(genre = genre %>% str_split(', ')) %>% 
  unnest(genre)



anime %>% 
  filter(!is.na(type)) %>% 
  ggplot(aes(x = type, fill = type)) +
  geom_bar() +
  labs(title = 'Tipos de anime',
       subtitle = 'Al 2019',
       x = 'Categoria del anime',
       y = 'Registros') +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(color = 'red',
                                  size = 25,
                                  family = 'Fascinate'),
        plot.subtitle =  element_text(color = 'red',
                                      size = 15,
                                      family = 'Fascinate'))


# gráfico de puntos
anime %>% 
  group_by(type, episodes) %>% 
  summarise(cantidad = mean(rating)) %>% 
  ggplot() +
  geom_point(aes(x = episodes,
                 y = cantidad), color = '#C10A0A90') # Los ultimos 2 numeros indican la transparencia del color

Cor_Autos = cor(mtautos) # crear una matriz de correlación
ggcorrplot(Cor_Autos) +  # La función ggcorplot recibe una matriz de correlación
  # Se puede modificar con las funciones clásicas de ggplot
  theme_bw() +
  labs(title = 'gráfico de prueba',
       x = '')


ggplot(data=paises, 
       aes(x = esperanza_de_vida, 
           y = pib_per_capita, color = continente))+
  geom_point(size = 2, alpha = 0.7) + 
  theme_minimal() +
  facet_wrap(~ continente, nrow = 5, scales = 'free_x')




