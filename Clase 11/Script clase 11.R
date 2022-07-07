# Clase 11 HCML - Diplomado Data Science 2022
# Diego Muñoz :D
library(tidyverse)
library(nortest)
library(janitor)
library(lmtest)

# MODELOS ANOVA ----

sueldos <- readxl::read_excel('sueldos.xlsx')

sueldos <- sueldos %>% 
  mutate(SEXO = factor(SEXO, levels = c(0,1), labels = c('Mujer','Hombre')))

# ¿La renta promedio es diferete según el sexo?
# ¿El estado civil en función del sexo genera diferencias de renta promedio?

## Muestra ----
set.seed(123)
sueldos2 <- sueldos %>% 
  sample_n(5000)


sueldos2 %>% 
  group_by(SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA))



sueldos2 %>% 
  ggplot(aes(x = SEXO, y = RENTA, fill = SEXO)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", size=8, color="red", fill="red")


# Cuando tenemos solo 2 niveles, podemos realizar un t.test
t.test(
  x = sueldos2 %>% filter(SEXO == 'Hombre') %>% pull(RENTA),
  y = sueldos2 %>% filter(SEXO == 'Mujer') %>% pull(RENTA),
  mu = 0,
  alternative = 't',
  conf.level = 0.95
)

# Análogo a utilizar un modelo anova cuando solo se tienen 2 niveles.
aov(RENTA ~ SEXO, data = sueldos2) %>% summary()

# Que ocurre al incluir el estado civil?

sueldos2 %>% 
  group_by(EST_CIVIL,SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA))

# Utilicemos el boxplot para ver la interacción
sueldos2 %>% 
  ggplot(aes(x = EST_CIVIL, y = RENTA, fill = SEXO)) +
  geom_boxplot()

# ufff, el grafico de boxplot no sirve para esto..., por ello usaremos un gráfico de lineas :D

sueldos2 %>% 
  group_by(EST_CIVIL,SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA)) %>% 
  ggplot(aes(x = EST_CIVIL,
             y = RENTA,
             color = SEXO)) +
  geom_line(aes(group = SEXO)) +
  geom_point() +
  theme_minimal()

# Ajustamos un modelo anova
mod <- aov(RENTA ~ SEXO * EST_CIVIL, data = sueldos2)
mod %>% summary()


# Equivalente a utilizar el *
aov(RENTA ~ SEXO + EST_CIVIL + SEXO:EST_CIVIL, data = sueldos2)

## Datos completos ----

# Que pasaría si tuviera más información?
# Utilicemos la base de datos completa



sueldos %>% 
  group_by(SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA))



sueldos %>% 
  ggplot(aes(x = SEXO, y = RENTA, fill = SEXO)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom="point", size=8, color="red", fill="red")


# Cuando tenemos solo 2 niveles, podemos realizar un t.test
t.test(
  x = sueldos %>% filter(SEXO == 'Hombre') %>% pull(RENTA),
  y = sueldos %>% filter(SEXO == 'Mujer') %>% pull(RENTA),
  mu = 0,
  alternative = 't',
  conf.level = 0.95
)

# Análogo a utilizar un modelo anova cuando solo se tienen 2 niveles.
aov(RENTA ~ SEXO, data = sueldos) %>% summary()

# Que ocurre al incluir el estado civil?

sueldos %>% 
  group_by(EST_CIVIL,SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA))

# Utilicemos el boxplot para ver la interacción
sueldos %>% 
  ggplot(aes(x = EST_CIVIL, y = RENTA, fill = SEXO)) +
  geom_boxplot()

# ufff, el grafico de boxplot no sirve para esto..., por ello usaremos un gráfico de lineas :D

sueldos %>% 
  filter(EST_CIVIL != 'N') %>% 
  group_by(EST_CIVIL,SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA)) %>% 
  ggplot(aes(x = EST_CIVIL,
             y = RENTA,
             color = SEXO)) +
  geom_line(aes(group = SEXO)) +
  geom_point() +
  theme_minimal()

# Ajustamos un modelo anova
mod <- aov(RENTA ~ SEXO + EST_CIVIL, data = sueldos %>% filter(EST_CIVIL != 'N'))
mod %>% summary()


# Y que pasaría si utilizará la edad en vez de est_civil?


sueldos %>% 
  filter(EDAD != 'N') %>% 
  group_by(EDAD,SEXO) %>% 
  summarise(CANT = n(),
            RENTA = mean(RENTA)) %>% 
  ggplot(aes(x = EDAD,
             y = RENTA,
             color = SEXO)) +
  geom_line(aes(group = SEXO)) +
  geom_point() +
  theme_minimal()

# Ajustamos un modelo anova
mod <- aov(RENTA ~ SEXO + EDAD + EST_CIVIL, data = sueldos %>% filter(EDAD != 'N'))
mod %>% summary()

mod <- aov(RENTA ~ SEXO + EDAD, data = sueldos %>% filter(EDAD != 'N'))
mod %>% summary()

# Test de normalidad

lillie.test(mod$residuals)

# El valor - p = (< 2.2e-16) < 0.05 -> Rechazamos la hipótesis de que los residuos siguen
# una distribución normal

# Unas formas de solucionar la normalidad son:

# Incluir/excluir variables del modelo

# Incluir/excluir observaciones

# Utilizar otra metodología

# Regresión lineal -----

vida <- read_csv('life.csv')
vida <- vida %>% 
  clean_names()

vida %>% summary()

# Omitimos los casos NA's por simplicidad para la clase

vida <- vida %>% 
  drop_na()

modelo <- lm(life_expectancy ~ adult_mortality, data = vida)
modelo %>% summary()

# Escrito el modelo es:
# life_expectancy = 77.598238 + -0.049317*adult_mortality

vida %>% 
  ggplot(aes(x = adult_mortality, y = life_expectancy)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = 'y ~ x')

# Que ocurre si utilizamos el statrus del país

modelo2 <- lm(life_expectancy ~ adult_mortality + status, data = vida)
modelo2 %>% summary()

vida %>% 
  ggplot(aes(x = adult_mortality, y = life_expectancy, fill = status)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = 'y ~ x')


# Y ~ Normal # Esto no es correcto.
# Y|X = x ~ Normal # Esto es lo que buscamos.


modelo3 <- lm(life_expectancy ~ adult_mortality + alcohol, data = vida)
modelo3 %>% summary()


lm(life_expectancy ~ adult_mortality, data = vida) %>% summary()
lm(life_expectancy ~ alcohol, data = vida) %>% summary()

modelo3 %>% summary()

# Modelo de regresión lineal múltiple

vida <- vida %>% 
  select(-country)

modeloFull <- lm(life_expectancy ~ ., data = vida)
modeloFull %>% summary()


modelo_final <- step(modeloFull, direction = 'both')
modelo_final %>% summary()


AIC(modelo_final)
AIC(modelo2)

# Análisis de resiudos
# Normalidad
# H0: Los residuos siguen una distribución normal
# H1: Los residuos no siguen una distribución normal
lillie.test(modelo_final$residuals)

# Homocedasticidad
# H0: Los residuos son homocedasticos
# H1: Los residuos son heterocedasticos
lmtest::bptest(modelo_final)

# Independencia
# H0: Los residuos son independeintes
# H1: Los residuos están autocorrelacionados
lmtest::dwtest(modelo_final)


# Analisis de factor de inflación de la varianza (VIF)

VIF <- car::vif(modelo_final)

df_VIF <- tibble(variable = VIF %>% names(),
                 VIF = VIF)

df_VIF %>% 
  mutate(Alto = ifelse(VIF > 5, 'Alto','bajo')) %>% 
  ggplot(aes(y = variable, x = VIF, fill = Alto)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Análisis del factor de inflación de la varianza')



modF <- step(lm(life_expectancy ~ ., data = vida %>% select(-under_five_deaths)),
             direction = 'both')

VIF <- car::vif(modF)

df_VIF <- tibble(variable = VIF %>% names(),
                 VIF = VIF)

df_VIF %>% 
  mutate(Alto = ifelse(VIF > 5, 'Alto','bajo')) %>% 
  ggplot(aes(y = variable, x = VIF, fill = Alto)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Análisis del factor de inflación de la varianza')

modF %>% summary()






