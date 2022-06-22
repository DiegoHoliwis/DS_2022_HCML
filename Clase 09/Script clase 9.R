# Clase numero 9 HCML -----

# https://r-graph-gallery.com/all-graphs Muchos gráficos :D

library(tidyverse)
library(datos)
library(patchwork) # Permite visualizar varios gráficos al mismo tiempo
library(showtext)  # Permite añadir fond
showtext_auto()
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
                 y = cantidad), color = 'red')

