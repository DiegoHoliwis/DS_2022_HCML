---
title: "Título del Documento"
description: 'Hola, como estan'
author:
  - name: "Diego Muñoz"
    email: dimunoz1@uc.cl
  - name: "Diego Muñoz"
    email: dimunoz1@uc.cl
  - name: "Diego Muñoz"
    email: dimunoz1@uc.cl
date: "29 de junio, 2022"
output:
  rmdformats::readthedown:
  code_folding: hide # ocultar los chunk de código
knit: (
  function(inputFile, encoding) { 
    rmarkdown::render( 
      input       = inputFile, 
      encoding    = encoding, 
      output_file = 'Reporte de prueba.html') })
---

```{=html}
<script>
   $(document).ready(function() {
     $head = $('#header');
     $head.prepend('<img src=\"https://raw.githubusercontent.com/DiegoHoliwis/DIGITAPALOOZA_2022/main/Presentaci%C3%B3n/media/logo3.png" style=\"float: left;width: 200px;\" />')
   });
</script>

<style>
p {
    font-size: 20px;
    line-height: 24px;
    margin: 0px 0px 12px 0px;
    text-align: justify;
    texto-justify: inter-palabra;
}

h1, h2, h3, h4, h5, h6, legend {
    font-family: Arial, sans-serif;
    font-weight: 600;
    color: #04346c;
}

.header-panel  {
    background-color: #04346c;
}

.nav-tabs{
  background-color:#04346c9e;
}

.nav-tabs > li > a{
  background-color: #04346c9e !important;
    <!-- border: medium none; -->
    <!-- border-radius: 0; -->
    <!-- color:#fff; -->
}

.pages .nav-tabs>li.active>a {
background-color: #04346c !important;
}


#sidebar {
    background-color: #0A31C1;
}


#sidebar h2 {
    background-color: #111dc2;
}

#sidebar a {
    color: #3F4548;
}

#sidebar a:hover {
    background-color: #3F4548;
    color: #FFFFFF;
}

</style>
```



# Primer título

RStudio es un entorno de desarrollo integrado (IDE) para el lenguaje de programación R, dedicado a la computación estadística y gráficos. Incluye una consola, editor de sintaxis que apoya la ejecución de código, así como herramientas para el trazado, la depuración y la gestión del espacio de trabajo.

RStudio está disponible para Windows, Mac y Linux o para navegadores conectados a RStudio Server o RStudio Server Pro (Debian / Ubuntu, RedHat / CentOS, y SUSE Linux).3 RStudio tiene la misión de proporcionar el entorno informático estadístico R. Permite un análisis y desarrollo para que cualquiera pueda analizar los datos con R.
