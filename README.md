# Ludica
Registro en shiny para lúdica Calidad

**Instalar los paquetes requeridos en R local**
```r
if (!require('shiny')) install.packages('shiny')
if (!require('ggplot2')) install.packages('ggplot2')
if (!require('plotly')) install.packages('plotly')
```
**Para Correr la Aplicación en local**
```r
shiny::runGitHub("Ludica", "johanmarin",  launch.browser= TRUE)
```
