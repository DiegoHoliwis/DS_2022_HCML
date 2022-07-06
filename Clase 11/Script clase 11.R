# Clase 11 HCML - Diplomado Data Science 2022
# Diego Muñoz :D

# MODELOS ANOVA ----

sueldos <- readxl::read_excel('sueldos.xlsx')

sueldos <- sueldos %>% 
  mutate(SEXO = factor(SEXO, levels = c(0,1), labels = c('Mujer','Hombre')))

# ¿La renta promedio es diferete según el sexo?
# ¿El estado civil en función del sexo genera diferencias de renta promedio?

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






