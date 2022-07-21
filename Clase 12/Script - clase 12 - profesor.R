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

# Separación en train and test


# Paso 1: Separar la base de datos en train and test
# Paso 2: Modelar usando la base de train
# Paso 2.1: Ajustar hiperparametros (no se aplica en regresión logísitca)
# Paso 3: Validar usando la base de test

set.seed(123)
# Selecciona el 70 de los datos
train <- df %>% sample_frac(0.7) 

# Selecciona los datos del df que no esten en la base train
test  <- df %>% filter(!row.names(.) %in% row.names(train)) 

mod1 <- glm(formula = salario ~ edad + estado_civil + raza + sexo + nativo + horas_semanal,
            data    = train,
            family  = binomial(link = 'logit'))

summary(mod1)

# Predicción

y_test_prob <- predict(mod1,
                       newdata = test,
                       type = 'response') %>% as.vector()

y_test = test$salario


# ¿Qué ocurre cuando existe desbalance en la base datos?
# Los modelos de regresión logística están basados en la idea de que existen
# la misma cantidad de observaciones de las dos clases (misma cantidad de 0 y 1)
# Cuando esto no ocurre, en algunos casos el modelo podría llegar aprender muy
# bien de una clase y muy mal de la otra (normalmente mal de la que nos interesa
# Predecir). Para solucionar estas problemáticas existen varias técnicas, por ejemplo:

# Opción 1: Eliminar usuarios de la clase con mayor representatividad
# Opción 2: Incluir varias veces a los usuarios con la clase menor
# Opción 3: Utilizar las dos técnicas anteriores al mismo tiempo
# Opción 4: Utilizar regularización L1 y L2
# Opción 5: Utilizar un modelo que no se vea afectado por desbalance :D

# Nota: Explorar la metodología smote

y_test_pred <- if_else(y_test_prob > 0.5,1,0)

MLmetrics::ConfusionMatrix(y_test_pred, test$salario)

#        y_pred
#  y_true    0    1
#       0 6821  543
#       1 1734  670

# sensibilidad(recall)       = 670/(1734 + 670)
# precisión                  = 670/(670 + 543)
# Especificidad(specificity) = 6821/(6821 + 543)
# Exactitud(Acurracy)        = (6821 + 670)/(6821 + 543 + 1734 + 670)
  
sensitivity(y_test, y_test_prob, 0.5)
  