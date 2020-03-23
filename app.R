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

server <- shinyServer(function(input, output) {
    
    X<-read.csv("https://gegp01.github.io/C3reporta/coords.csv") # Datos de Jhon Hopkins Institute y WHO
    D<-X[3:104,] # reported cases
    
    death<-X[grep("death", X$name),]
        death$country<-do.call(rbind, strsplit(as.vector(death$name), ".", fixed=T))[,1]
        y<-do.call(rbind, strsplit(as.vector(death$name), ".", fixed=T))[,2]
        death$check<-y=="deaths"
        d<-death[death$check==T,]

    dead.count<-aggregate(as.numeric(as.character(d$data)), list(d$country), sum)
        dead.count$Group.1<-as.vector(dead.count$Group.1)
        dead.count$Group.1[dead.count$Group.1=="UnitedStatesofAmerica"]<-"United States"
        dead.count$Group.1[dead.count$Group.1=="RepublicofKorea"]<-"South Korea"
        dead.count$Group.1[dead.count$Group.1=="UnitedKingdom"]<-"United Kingdom"
        dead.count$Group.1[dead.count$Group.1=="SanMarino"]<-"San Marino"
        dead.count$latitude<-X$latitude[match(dead.count$Group.1, X$name)]
        dead.count$longitude<-X$longitude[match(dead.count$Group.1, X$name)]
        
#    D$dead<-d$data[match(D$name, d$country)]
    D$dead<-dead.count$x[match(D$name, dead.count$Group.1)]    
    D$dead[is.na(D$dead)]<-0

    text0<-paste(D$name, ":", D$dead, "muertes", "|", D$data, "casos confirmados")
    text1<-paste("<p style='width: 90vh;'><b>Total de casos confirmados con COVID19: </b>", X$data[X$name=="Global.confirmed"]
                 , "<br><b>Muertes: </b>", sum(dead.count$x)
                 , "<br><a href='https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports' target='_blank'>Reporte de la WHO No: ", X$data[X$name=="WHO report"], "</a>"
                 , "<br>Fecha: ", X$data[X$name=="Date"]
                 , "<br></p>")
    

    # # Polygon values, ordered by wrld_simpl$NAME
    # infected<-as.numeric(as.character(D$data)[match(as.vector(wrld_simpl$NAME), D$name)])
    # susceptibles<-wrld_simpl$POP2005
    # dead<-as.numeric(as.character(D$dead)[match(as.vector(wrld_simpl$NAME), D$name)])
    # 
    # # z<-(infected/susceptibles)*1000
    # 
    # popup<-text0[match(as.vector(wrld_simpl$NAME), D$name)]
    # 
    # z<-dead/infected
    
    #z<-as.numeric(as.character(D$dead))/as.numeric(as.character(D$data)) # dead/infected. Note: Add epsilon here
    z<-log(as.numeric(as.character(D$dead))+1)
    
    myPal <- colorRampPalette(c("royal blue", "red"))
    p<-transp(num2col(z, col.pal=myPal),.8)

    output$map <-renderLeaflet({
        leaflet() %>% setView(20, 10, zoom = 2) %>% addTiles() %>% addCircles(lng = D$longitude, lat = D$latitude, radius = as.numeric(as.character(D$data))*10, label=htmlEscape(text0), labelOptions(noHide=T), color=p)
        # leaflet(wrld_simpl) %>% addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 1, fillColor = p, popup=htmlEscape(popup), popupOptions=popupOptions(closeButton = FALSE))        
        })
    
    output$caption<-renderUI({
        HTML(text1)
    })

})

shinyApp(ui = ui, server = server)