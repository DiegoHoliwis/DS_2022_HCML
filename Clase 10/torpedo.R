# Torpedo clase 10
library(tidyverse)
library(DT)


# Rmarkdown es una herramienta que combina múltiples lenguajes de programación
# el principal enfoque de este lenguaje de programación, es el crear reportes
# utilizando lenguajes de programación de alto nivel como R/Python
# Además integra lenguaje LaTex, HTML, CSS, entre otros.

# Estructura de la clase -----

# Tipos de encabezados:
  # Se mostrarán todos los tipos de encabezados que podemos modificar
# Como escribir texto en Rmarkdown, pregunta seria, cómo podemos centrar el texto?



# Encabezado -----

# Encabezado básico

# ---
# title: "Título del Documento"
# author: "Mi nombre"
# date: "Fecha"
# output: pdf_document
# ---

# Encabezado utilizando formatos prettydoc

# ---
# title: "Título del Documento"
# author: "Mi nombre"
# date: "Fecha"
# output:
#   prettydoc::html_pretty:
#   theme: hpstr
# highlight: github
# ---
  
# Encabezado utilizando formato material
  
# ---
# title: "Título del Documento"
# author: "Data Science UC"
# date: "Versión 2022"
# output:
#   rmdformats::material:
#   code_folding: show #hide
# ---
  

# Multiples autores

# ---
# title: "Título del Documento"
# description: 'Hola, como estan'
# author:
#   - Diego Muñoz
#   - Javiera Preuss
#   - Josefa Silva
# date: "Versión 2022"
# output:
#   rmdformats::material:
#   code_folding: show #hide
# ---
  

# Trucasos ----------

## Modificar el css ----

# ```{=html}
#   <style>
#   p {
#     font-size: 20px;
#     line-height: 24px;
#     margin: 0px 0px 12px 0px;
#   }
# 
# h1, h2, h3, h4, h5, h6, legend {
#   font-family: Arial, sans-serif;
#   font-weight: 600;
#   color: #04346c;
# }
# 
# .header-panel  {
#   background-color: #04346c;
# }
# 
# .nav-tabs{
#   background-color:#04346c9e;
# }
# 
# .nav-tabs > li > a{
#   background-color: #04346c9e !important;
#     <!-- border: medium none; -->
#     <!-- border-radius: 0; -->
#     <!-- color:#fff; -->
# }
# 
# .pages .nav-tabs>li.active>a {
#   background-color: #04346c !important;
# }
# 
# </style>
#   ```

## Crear una caja con scroll ----

# ```{css, echo = FALSE}
# .myBox {
#   border: none;
#   padding: 5px;
#   font: 18px/24px sans-serif;
#   width: max;
#   overflow-x: auto;
#   <!-- overflow: scroll; -->
# }
# ```
# 
# # Luego
# 
# <div class="myBox"> Aquí se escribe <div>

## insertar un logo -----

# <script>
#   $(document).ready(function() {
#     $head = $('#header');
#     $head.prepend('<img src=\"https://raw.githubusercontent.com/DiegoHoliwis/DIGITAPALOOZA_2022/main/Presentaci%C3%B3n/media/logo3.png" style=\"float: left;width: 200px;\" />')
#   });
# </script>

## Modificar todos los chunk -----

# knitr::opts_chunk$set(warning = FALSE,
#                       message = FALSE,
#                       echo    = TRUE)

## Crear tablas bonitas -----

iris %>% 
  group_by(Species) %>% 
  summarise(promedio_sepallength = mean(Sepal.Length),
            promedio_sepalwidth  = mean(Sepal.Width),
            promedio_petallegth  = mean(Petal.Length),
            promedio_petalwidth  = mean(Petal.Width)) %>% 
DT::datatable(
  extensions = c('Buttons','Scroller'),
  rownames = FALSE,
  options = list(
    dom = "Blftipr"
    ,scrollY = 510
    ,scrollx = '100%'
    ,buttons = list(
      list(extend = 'excel',
           text = 'Descargar - EXCEL',
           filename = paste0("Iris.xlsx"),
           exportOptions = list(
             modifier = list(page = "all")
           )
      )
    )
    ,language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
    ,lengthMenu = list(c(5,10,20,30,-1), list('5','10','20','30','Todo'))
    ,pageLength = -1
    ,columnDefs = list(list(className = 'dt-center', targets = "_all"))
    ,searchHighlight = TRUE
    ,autoWidth = TRUE
    ,initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '15px','background-color': '#04346c', 'color': '#fff'});
            $(this.api().table().container()).css({'font-size': '13px'})",
      "}")
    # ,rowCallback = JS('
    #                    function(nRow, aData, iDisplayIndex, iDisplayIndexFull,displayNum) {
    #                    if (parseFloat(aData[1]) < 0.01)
    #                    $("td:eq(1)", nRow).css("opacity", 0.3),
    #                    $("td:eq(1)", nRow).css("color", "green");
    #                    if (parseFloat(aData[2]) < 5) 
    #                    $("td:eq(2)", nRow).css("opacity", 0.3),
    #                    $("td:eq(2)", nRow).css("color", "green");
    #                    if (parseFloat(aData[3]) < 0.01)
    #                    $("td:eq(3)", nRow).css("opacity", 0.3),
    #                    $("td:eq(3)", nRow).css("color", "green");
    #                    if (parseFloat(aData[4]) < 0.01)
    #                    $("td:eq(4)", nRow).css("opacity", 0.3),
    #                    $("td:eq(4)", nRow).css("color", "green");
    #                    if (parseFloat(aData[5]) < 0.01)
    #                    $("td:eq(5)", nRow).css("opacity", 0.3),
    #                    $("td:eq(5)", nRow).css("color", "green");
    #                    if (parseFloat(iDisplayIndexFull) == 0)
    #                    $("td", nRow).css({"background-color": "#FA9E08"});
    #                    if (parseFloat(iDisplayIndexFull) == 1)
    #                    $("td", nRow).css({"background-color": "#FA9E08"});
    #                    if (parseFloat(iDisplayIndexFull) == 2)
    #                    $("td", nRow).css({"background-color": "#FED187"});
    #                    }')
  ),
  fillContainer = TRUE,
  class = "display") %>% 
  formatRound(columns = c('promedio_sepallength'), digits = 0) %>% 
  formatPercentage(columns = c('promedio_sepalwidth'))




