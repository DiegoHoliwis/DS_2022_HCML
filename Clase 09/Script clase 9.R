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
# install.packages('GGally')
library(GGally)
library(ggrepel)

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


# Grilla de gráficos
gg1 <- anime %>% 
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

gg2 <- ggplot(data=paises, 
              aes(x = esperanza_de_vida, 
                  y = pib_per_capita, color = continente))+
  geom_point(size = 2, alpha = 0.7) + 
  theme_minimal() 

(gg1 + gg2) / gg2


# Como cambiar el número de break y limites de los ejes

ggplot(data=paises, 
       aes(x = esperanza_de_vida, 
           y = pib_per_capita, color = continente))+
  geom_point(size = 2, alpha = 0.7) + 
  theme_minimal() +
  scale_x_continuous(n.breaks = 10,
                     limits = c(30,60)) + 
  scale_y_continuous(n.breaks = 15,
                     limit = c(0,10000))


# Clase numero 9 -----

base_chile <- paises %>% 
  filter(pais=="Chile")
base_vecinos <- paises %>% 
  filter(pais %in% c("Chile","Perú","Bolivia","Argentina"))
america07 <- paises %>% 
  filter(anio==2007, continente == "Américas")

# Funcionalidades de resumen
# ..count..
# ..prop..
# ..density..

anime %>% 
  filter(!is.na(type)) %>% 
  ggplot(aes(x = type)) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  # geom_bar(aes(y = ..count../sum(..count..))) +
  scale_y_continuous(labels = scales::percent)



anime %>% 
  filter(!is.na(type)) %>% 
  group_by(type) %>% 
  summarise(count = n()) %>% 
  mutate(proporción = count/sum(count)) %>% 
  ggplot(aes(x = type, y = proporción)) +
  geom_bar(stat = 'identity')


# Diferencias entre usar group vs no usarl

# Usando group
mtautos %>% 
  ggplot(aes(x = factor(cilindros), group = factor(cambios))) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..))) +
  facet_grid(~cambios)
           
# Sin usar group
mtautos %>% 
  ggplot(aes(x = factor(cilindros))) +
  geom_bar(aes(y = ..count../sum(..count..),fill = factor(..x..))) +
  facet_grid(~cambios)


mtautos %>% 
  group_by(cilindros,cambios) %>% 
  summarise(cantidad = n(), .groups = 'drop') %>% 
  mutate(proporcion = cantidad/sum(cantidad)) %>% 
  ggplot(aes(x = cilindros, y = proporcion)) +
  geom_bar(stat = 'identity') +
  facet_grid(~cambios)


# Gráfico de torta
 
df_cilindros = with(mtautos, table(cilindros)) %>% 
  data.frame()

#equivalente: mtautos %>% pull(cilindros) %>% table() %>% data.frame()

df_cilindros %>% 
  mutate(porcentaje = 100*Freq/sum(Freq)) %>% 
  ggplot(aes(x = Freq, y = '', fill = cilindros)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('x') +
  theme_void() +
  geom_text(aes(label = porcentaje),
            size = 10,
            position = position_stack(vjust=0.5))


df_cilindros %>% 
  mutate(porcentaje = 100*Freq/sum(Freq)) %>% 
  ggplot(aes(y = porcentaje, x = '', fill = cilindros)) +
  geom_bar(stat = 'identity', width = 1) +
  geom_text(aes(label = paste0(round(porcentaje,1),'%')),
            size = 10,
            position = position_stack(vjust=0.5)) +
  coord_polar('y') +
  theme_void()

# geom_abline

ggplot(data = mtautos, aes(x=millas, y=velocidad))+
  geom_point(size = 3, shape = 21, fill = "turquoise") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  scale_y_continuous(limits = c(0,30)) +
  scale_x_continuous(limits = c(0,30))


ggplot(data = mtautos, aes(x=millas, y=velocidad))+
  geom_point(size = 3, shape = 21, fill = "turquoise") +
  stat_smooth(method = "lm", se = F)

# GGpairs

# Solo utilizar para pequeños conjuntos de datos
GGally::ggpairs(flores)


# Texto en los gráficos

ggplot(data=mtautos, aes(x=peso, y=millas)) +
  geom_point(color = "darkgreen", alpha = 0.5) + 
  geom_label(x = 3.5, y = 25, label = "Texto de prueba",
            color = "black",stat = "unique", size = 10)


gg3 <- ggplot(data = america07, aes(x = pib_per_capita, 
                             y = esperanza_de_vida)) + 
  geom_point(size=4, alpha=0.3, col = "darkblue") + 
  geom_text(aes(label = pais))


gg3 <- ggplot(data = america07, aes(x = pib_per_capita, 
                             y = esperanza_de_vida)) + 
  geom_point(size=4, alpha=0.3, col = "darkblue") + 
  ggrepel::geom_text_repel(aes(label = pais))

## Plotly

plotly::ggplotly(gg3)

## ggAnime

plot_1 = ggplot(data = base_vecinos, 
                aes(x = anio, y = esperanza_de_vida, color = pais) ) +
  geom_line(size = 1) 

animate(plot_1 + 
          geom_point(aes(group = seq_along(anio)), size = 2) + 
          transition_reveal(anio))


plot_3 = ggplot(paises, aes(x = pib_per_capita,
                            y = esperanza_de_vida,
                            color = continente)) +
  geom_point(size=5, alpha=0.5) +
  labs(x = "PIB per cápita (en dólares)",
       y = "Esperanza de vida (en años)",
       title = "Esperanza de vida dado PIB pér cápita",
       subtitle = "Diferenciado según continente")
plot_3

animate(plot_3 +
          transition_states(continente,transition_length = 1,
                            state_length = 2) +
          shadow_mark(alpha=0.3, size=2) +
          enter_fade() +
          exit_shrink())

plotly::ggplotly(plot_1)


# Higcharter

library(highcharter)
library(data.table)

cols <- c("#4285F4",'#3F06E2','#CD8D04','#9C0ADB','#FBBC05','#34A853','#EA4335','#9C0ADB')

data <- fread('https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_T.csv') %>%
  tibble() %>%
  select(Fecha,
         Casos = `Casos nuevos totales`,
         `Casos activos`) %>% 
  filter(Fecha >= '2021-01-01',
         Fecha <= '2022-06-21')

data %>% names()

data %>% 
  hchart(type = "line", 
         name = 'Casos COVID',
         showInLegend = TRUE,
         hcaes(x = Fecha, 
               y = Casos)) %>% 
  hc_add_series(data,showInLegend = TRUE,type = 'line',name = 'Activos', hcaes(x = Fecha, y = `Casos activos`)) %>% 
  hc_xAxis(
    title = list(text = "Fecha"),
    labels = list(
      # rotation = -45,
      format = '{value:%y/%m/%d}')
  ) %>% 
  hc_yAxis(opposite = FALSE,
           title = list(text = "Casos COVID"),
           labels = list(format = "{value}")) %>% 
  hc_title(text = paste0('Casos COVID'),
           margin = 30,
           align = "left") %>% 
  hc_subtitle(text = 'Casos Chile', 
              align = "left") %>% 
  hc_legend(enabled = TRUE
            # align = "right",
            # verticalAlign = "top",
            # layout = "horizontal"
  ) %>% 
  hc_credits(enabled = TRUE,
             text = "Datos ministerio de ciencia",
             href = "https://github.com/MinCiencia/Datos-COVID19"
  ) %>% 
  hc_colors(cols) %>%
  hc_tooltip(
    # pointFormat = "<span>Casos COVID: <b>{point.y}</b><br></span>",
    shared = TRUE,
    borderWidth = 0
  ) %>% 
  hc_chart(
    zoomType = "x"
  ) %>% 
  hc_exporting(
    enabled = TRUE, # always enabled
    filename = "custom-file-name"
  )


