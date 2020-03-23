# For any question please contact Gabriel E Garcia Peña gegp[AT]ciencias.unam.mx
library(shiny)
library(shinythemes)
library(shinyjs)
library(leaflet)
#library(maptools) # Solo cuando se agregan poligonos
library(epicontacts)
library(adegenet)
library(htmltools)

options(shiny.maxRequestSize = 30*1024^2) # ALLOW HANDLING LARGE FILES

ui <- shinyUI(
    
    fluidPage(theme=shinytheme("spacelab")
        , titlePanel("", windowTitle="reporta | COVID19")
        
        , mainPanel(
            HTML("<h4>Reporta C3 </h4>")
            , htmlOutput("caption")
            , HTML("<br>")
            , leafletOutput("map", width="95vh", height="70vh")
            , HTML("<p style='width: 90vh;'><br><b>Los círculos representan los casos con COVID19 </b>. El tamaño del círculo representa el número de casos y el número de pacientes muertos se representa en un gradiente de colores, de <font color='royalblue'> azul (0 muertos) </font> a <font color='red'>rojo (> 3000 muertos)</font>.<br><br>")
            , HTML("<br><br><a rel='license' href='http://creativecommons.org/licenses/by-sa/4.0/'><img alt='Creative Commons License' style='border-width:0' src='https://i.creativecommons.org/l/by-sa/4.0/88x31.png'/></a>
        <br><a  href='mailto:gegp@ciencias.unam.mx' style='font-size:80%;'>Contact: gegp@ciencias.unam.mx</a>")
        )
    )
)
