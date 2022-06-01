# Clase 6 - estructuras de control -----
library(tidyverse)

## operadores lógicos ----

2 < 3
3 < 2

2 <= log(4)
3 <= log(100)

paises <- c('chile','perú','brasil')

!'canada' %in% paises

PIB_PAISES <- tibble(pais = c('chile','canada','EEUU'),
       PIB = c(33,23,50))

PIB_PAISES %>% 
  filter(!pais %in% paises)


PIB_PAISES %>% 
  filter(!pais %in% paises & PIB > 40)

"Gato" == "gato"

str_to_lower("Gato") == str_to_lower("gato")

letters
LETTERS

str_to_lower(c("a","B","c")) %in% letters

## Estructuras de control ----

numero <- 44
numero >= 4

numero %% 2 == 0

## Comando if -----

nota <- 5.8
if(nota >= 4){
  print('Felicidades, aprobaste el curso') 
}else{
  print('Lo sentimos, pero has reprobado :c')
}

if(nota >= 4) print('Felicidades, aprobaste el curso') 

nota <- -4
ifelse(nota >= 4,
       'Felicidades, aprobaste el curso',
       'Lo sentimos, pero has reprobado :c')


if(nota >= 4){
  print('Felicidades, aprobaste el curso') 
}else{
  print('Lo sentimos, pero has reprobado :c')
}


notas <- "cr"
is.numeric(notas)
if(notas >= 4){
  print('Felicidades, aprobaste el curso') 
}else{
  print('Lo sentimos, pero has reprobado :c')
}


if((nota < 1) | (nota > 7) | (is.numeric(nota) == FALSE)) { 
  print("Error, ingrese un número entre 1 y 7")
} else if (nota >= 4) {
  print("¡Felicitaciones!")
} else {
  print("Reprobaste")
}


ifelse((nota < 1) | (nota > 7) | (is.numeric(nota) == FALSE),
        "Error, ingrese un número entre 1 y 7",
        ifelse(nota >= 4,
               "¡Felicitaciones!",
               "Reprobaste"))

## case_when ------

nota <- '2'

case_when(is.numeric(nota) == FALSE ~ "Error, ingrese un número.",
          (nota < 1 | nota > 7) ~ "Error, ingrese un número entre 1 y 7.",
          nota == 7 ~ '¡Felicidades, tuviste una nota perfecta!',
          nota >= 4 ~ "¡Felicitaciones!",
          TRUE ~ 'Reprobaste')

# data.table::fifelse() Este comando es más eficiente en grandes volúmenes de datos.

## Comando for -----

notas <- runif(62,3,7) %>% round(1)

resultados <- character(0)

for(i in 1:length(notas)){
  resultados[i] <- case_when(is.numeric(notas[i]) == FALSE ~ "Error, ingrese un número.",
              (notas[i] < 1 | notas[i] > 7) ~ "Error, ingrese un número entre 1 y 7.",
              notas[i] == 7 ~ '¡Felicidades, tuviste una nota perfecta!',
              notas[i] >= 4 ~ "¡Felicitaciones!",
              TRUE ~ 'Reprobaste')
}

alumnos_turing <- tibble(nota = notas,
                         mensaje = resultados)


## Actividad I -----














