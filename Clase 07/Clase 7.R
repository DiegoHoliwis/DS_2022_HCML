# Clase 7 -----
# install.packages('datos')
# install.packages('data.table')

library(tidyverse)
library(datos)
library(data.table)


# Familia de funciones apply -----
## Apply() ----

# Aplica una función sobre las filas o columnas de un data.frame

flores %>% tibble()

# La función apply recibe una función la cual no debe estar ejecutada
# esto quiere decir que no se debe colocar los paréntesis, ejemplo:

# Forma incorrecta
apply(flores[,1:4], MARGIN = 2, FUN = sum())
# Forma correcta
apply(flores[,1:4], MARGIN = 2, FUN = sum)


# Que pasaría si quiero activar el argumento na.rm = TRUE

# Forma 1
apply(flores[,1:4], MARGIN = 2, FUN = function(x){sum(x,na.rm = TRUE)})

# Forma 2
my_suma <- function(x){
  # valor_suma = sum(x,na.rm = TRUE)
  # return(valor_suma)
  
  sum(x,na.rm = TRUE) %>% return()
}

apply(flores[,1:4], MARGIN = 2, FUN = my_suma)

## tapply ----

# Usando tapply
tapply(
  X = flores$Largo.Sepalo,
  INDEX = flores$Especies,
  FUN = mean
)

# Equivalente usando dplyr
flores %>% 
  group_by(Especies) %>% 
  summarise(Premio_Largo_sepalo = mean(Largo.Sepalo))

## lapply ----
# Aplicamos una función para cada elemento de una lista

Lista = list("Elemento 1" = 1:10,
             "Elemento 2" = 11:20,
             "Elemento 3" = 21:30)

lapply(X = Lista, FUN = median)

## sapply ----
Lista = list("Elemento 1" = 1:10,
             "Elemento 2" = 11:20,
             "Elemento 3" = 21:30)

sapply(X = Lista[c(1,3)], FUN = median)

# Paquete purrr ----

Lista = list("Elemento 1" = 1:10,
             "Elemento 2" = 11:20,
             "Elemento 3" = 21:30)

map_dbl(Lista, median)
map_dfc(Lista, median)

# Actividad de ejemplo 1----

asignaturas <- readxl::read_excel('Clase 07//Alumnos por asignatura.xlsx')

asignaturas %>% 
  select(`2015-1-APR`,`2015-2-APR`)

names(asignaturas) %>% str_subset('2015-1')

semestre <- '2015-2'

asignaturas %>% 
  select(str_subset(names(.),semestre)) %>% 
  mutate(TOTAL = apply(., MARGIN = 1, FUN = function(x){sum(x,na.rm = TRUE)})) %>% 
  setnames('TOTAL',paste0('Inscritos_',semestre))


c('2015-1','2015-2','2016-1','2016-2','2017-1') %>% 
  map_dfc(.f = function(x){
    asignaturas %>% 
      select(str_subset(names(.),x)) %>% 
      mutate(TOTAL = apply(., MARGIN = 1, FUN = function(x){sum(x,na.rm = TRUE)})) %>% 
      setnames('TOTAL',paste0('Inscritos_',x))
    
  })

# Actividad de ejemplo 2----

# Que pasaría si quiero cargar muchas bases de datos que estas codificadas del mismo modo

# Utilizando read_csv
autos <- dir('autos')[1:5] %>% 
  map_dfr(.f = function(x){
    paste0('autos//',x) %>% 
      read_csv()
  })


# Utilizando data.table
autos <- dir('autos') %>% 
  map_dfr(.f = function(x){
    print('hola')
    
    paste0('autos//',x) %>% 
      fread(encoding = 'UTF-8',
            nrows = 5
            # select = c('Nombre')
            ) %>% 
      slice(1:2,5)
})

## map2 ----

Ejemplo = function(x,y){
  log(x) + y^2
}

map2_dbl(1:3, 11:13, Ejemplo)

## Actividad 1 -----

# Pregunta 1.

funcion_par <- function(x){
  x*2 %>% return()
}

map_dbl(0:9, funcion_par)

# Pregunta 2.

Mensaje_alumno <- function(x){
  mensaje <- case_when(is.numeric(x) == FALSE ~ "Error, ingrese un número.",
            x < 1 ~ "Error, ingrese un número entre 1 y 7.",
            x > 7 ~ "Error, ingrese un número entre 1 y 7.",
            x >= 4 ~ "¡Felicitaciones!",
            x <4 ~ "Reprobaste")
  
  tibble(Nota = x, mensaje = mensaje)
  
}

Mensaje_alumno(7)

notas <- c(6.8, 5.5, 3.2, 4.3, 2.1,
           6.5, 4.7, 5.9, 6.2, 3.8)

map_dfr(notas, Mensaje_alumno)

## Paquete nest ----

flores %>% 
  group_by(Especies) %>% 
  nest()

## Función walk

dir()
dir.create()

# Paso 1
dir.create('Bases_Continentes')

# Opción 1

paises %>% 
  group_nest(continente) %>% 
  mutate(file = paste0('Bases_Continentes//',continente,'.csv')) %>% 
  select(x = data,
         file = file) %>% 
  pwalk(write_csv)

# Opción 2

paises %>% 
  group_nest(continente) %>% 
  mutate(file = paste0('Bases_Continentes//',continente,'.csv')) %>% 
  select(data,file) %>% 
  walk2(
    .x = .$data,
    .y = .$file,
    .f = ~write_csv(.x,.y)
  )

## Actividad final ----

df <- readxl::read_excel('Clase 07//esperanza.xlsx')
df %>% glimpse()

df <- df %>% 
  mutate(life_expectancy = life_expectancy %>% as.double(),
         alcohol = alcohol %>% as.double(),
         bmi = bmi %>% as.double(),
         hiv_aids = hiv_aids %>% as.double(),
         schooling = schooling %>% as.double())

df %>% glimpse()


# Preguinta 1

tabla_resumen <- function(df){
  df %>% 
    group_by(status) %>% 
    summarise(N_paises          = n(),
              Media_vida        = mean(life_expectancy, na.rm = TRUE),
              Media_alcohol     = mean(alcohol, na.rm = TRUE),
              Media_schooling   = mean(schooling, na.rm = TRUE),
              Media_muertes_VIH = mean(hiv_aids, na.rm = TRUE))
}
tabla_resumen(df)


# Pregunta 2
dir.create('Bases_Anuales')

# Pregunta 3

# Guardar en csv
df %>% 
  group_nest(year) %>% 
  mutate(tabla_resumen = map(data,tabla_resumen),
         file = paste0('Bases_Anuales//Año ',year,'.csv')) %>% 
  select(x = tabla_resumen,
         file = file) %>% 
  pwalk(write_csv)

# Guardar en xlsx
library(openxlsx)

df %>% 
  group_nest(year) %>% 
  mutate(file = paste0('Bases_Anuales//Año ',year,'.xlsx'),
         tabla_resumen = map(data,tabla_resumen)) %>% 
  select(x = tabla_resumen,
         file = file) %>% 
  pwalk(write.xlsx)

