# Clase 12 - Diplomado Data Science 2022
# Diego Muñoz

setwd('Clase 12')

library(tidyverse)
library(data.table)
library(InformationValue)

df <- read_csv('salarios.csv')
# df <- read_csv2('https://raw.githubusercontent.com/DiegoHoliwis/DS_2022_HCML/main/Clase%2012/seguros.csv')
df <- df %>% 
  select(-educacion) %>% 
  mutate(salario = if_else(salario == '>50K',1,0))

df %>% 
  sample_n(1000) %>% 
  ggplot(aes(x = edad, y = salario, color = factor(salario))) +
  geom_point() +
  geom_smooth(method = "lm",
              color = "gray20",
              se = FALSE,
              formula = 'y ~ x')


df %>% 
  sample_n(1000) %>% 
  ggplot(aes(x = edad, y = salario, color = factor(salario))) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "gray20",
              se = FALSE,
              formula = 'y ~ x')

# Modelo de regresión lineal (lm)

mod1 <- glm(formula = salario ~ edad,
            data    = df,
            family  = binomial(link = 'logit'))

summary(mod1)
# H0: Beta_edad == 0 (La variable NO es significativa)
# H1: Beta_edad != 0 (La variable ES significativa)


