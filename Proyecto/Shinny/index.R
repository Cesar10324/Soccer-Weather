library(shiny)
library(knitr)
library(ggplot2)
library(shinyWidgets)


#Preprocesado de la aplicacion

#mpgData <- mtcars
#mpgdataSam <- factor (mpgDataSam, labels = c("Automatico", "Manual"))

setwd("~/Diplomado/Proyecto/DataSets/Final")
#Se obtiene la data respectiva
resultados <-read.csv("LigaSantander_2017.csv",sep = ",",stringsAsFactors = F)
resultadosCiudad <-read.csv("Equipo_Ciudad.csv",sep = ",",stringsAsFactors = F)
equiposLigas <-read.csv("Equipo_Liga.csv",sep = ",",stringsAsFactors = F)
resultadosClima <-read.csv("Clima.csv",sep = ",",stringsAsFactors = F)
resultadosClimaCiudad <-read.csv("Clima_Ciudad.csv",sep = ",",stringsAsFactors = F)


#Se crea el dataFrame
df <-data.frame(resultados)
equiposDist <-unique(df$Equipo.Local)
equiposDist <-equiposDist[order(equiposDist,decreasing=F)]

dataFrameGanadorLocal <-df


campo <-c("Local","Visitante")

#Se crea el vector de ligas
ligas <- c("Liga Española","Bundesliga","Premier League")
ligas <-ligas[order(ligas,decreasing=F)]

#Interfaz Usuario
ui <- fluidPage(
        #Titulo de la App
        headerPanel("Analisis de Futbol Soccer"),
        
        #Crear sideBar panel para Liga
        sidebarPanel(
          
          #Input: Selector de la variable a visualizar
          selectInput("league","Liga: ",
                      ligas),
          
          #Input: Selector de la variable a visualizar
          selectInput("equ","Equipo: ",
                      equiposDist),
          
          #Input 2: radioBox para inclusion
          radioButtons("locVis",
                       "Campo:",
                       campo),
          
          #Input 3: chooseSliderSkin para grados centigrados
          chooseSliderSkin("Modern"),
          sliderInput("clima", "Temperatura en C°:",
                      min = -5, max = 38, value = c(20, 30)
          ),
          
          #Input 5: chooseSliderSkin para altura
          chooseSliderSkin("Modern"),
          sliderInput("altura", "Altura en m°:",
                      min = 30, max = 550, value = c(210, 320)
          ),
          actionButton("preview", "Obtener consolidado")
        ),     
        
        #Crear mainPanel que visualiza resultados
        mainPanel(
          
          #Final
          #Salida 1: Texto formateado de output$texto
          h3(textOutput("texto")),
          
          #Salida dos
          plotOutput("futPlot"),
          
          #Salida tres
          plotOutput("resultados"),
          
          plotOutput('regPlot')
        )
)


#Logica del servidor

server <- function (input, output, session){
  observe({
    dfEquipoLoga <- data.frame(equiposLigas)
    dfLiga <-dfEquipoLoga[dfEquipoLoga[,1] == input$league,c("NombreEquipo")]
    equiposSelecLiga <-unique(dfLiga)
    equiposSelecLiga <-equiposSelecLiga[order(equiposSelecLiga,decreasing=F)]
    updateSelectInput(session, "equ",
                      label = paste("Seleccione liga", length(equiposSelecLiga)),
                      choices = equiposSelecLiga,
                      selected = tail(equiposSelecLiga, 1))
    
  })
  
  
  # Obtener información de los input y mostrar graficas en main
  #Calcular el texto
  #Expresion reactiva que genera variables de salida
  #output$texto y aoutput$mgplot
  
  formulaTexto <- reactive({
    paste(input$equ)
  })
  
  #El output del titulo es la variable output$text
  
  #renderText, pintar de forma reactiva y dinamica el texto
  #Cada vez que cambie, la variable de salida se actualiza
  output$texto<- renderText({
    formulaTexto()
  })
  
  #Generar el plot de la variable
  #Excluir outliers si se requiere
  
  #as.formula(formulaTexto())
  output$futPlot <- renderPlot({
    rangoGrados <- c(input$clima)
    rangoAltura <- c(input$altura)
    
    if(input$locVis == "Local"){
      condicion <- df[,6]==input$equ
    }else if(input$locVis == "Visitante"){
      condicion <- df[,8]==input$equ
    }
    condicionClima <- df[,10] >=rangoGrados[1] & df[,10] <=rangoGrados[2]
    condicionAltura <- df[,11] >=rangoAltura[1] & df[,11] <=rangoAltura[2]
    dataFrameGanadorLocal <-df[condicion  & condicionClima &
                                 condicionAltura,
                               c("Equipo.Local",
                                 "Equipo.Visitante",
                                 "Goles.Equipo.Local",
                                 "Goles.Equipo.Visitante",
                                 "Temperatura","Altitud")]
      
      if(input$locVis == "Local"){
        equipoSeleccionado <- dataFrameGanadorLocal$Equipo.Local
      }else if(input$locVis == "Visitante"){
        equipoSeleccionado <- dataFrameGanadorLocal$Equipo.Visitante
      }
      
      #Se obtiene la frecuencia de equipos locales ganadores
      frecuenciaGananciaLocal <-data.frame(
        Equipo=table(equipoSeleccionado))
      sumaGoles <-sum(dataFrameGanadorLocal$Goles.Equipo.Local)
      if(!is.null(frecuenciaGananciaLocal)){
        ggplot(dataFrameGanadorLocal,
               aes(x = Temperatura,y = Goles.Equipo.Local,
                   size=Goles.Equipo.Local,
                   color=Temperatura))+geom_point() +labs(y = "Goles a favor",size = "Goles a favor")
      }
      else{
        cat("FALLA")
      }
  })
  
  #as.formula(formulaTexto())
  output$resultados <- renderPlot({
    condicionEmpate <- df[,7]==df[,9]
    rangoGrados <- c(input$clima)
    rangoAltura <- c(input$altura)
    if(input$locVis == "Local"){
      equipoSelec <- df[,6]==input$equ
      condicionVictoria <- df[,7]>df[,9]
      condicionDerrota <- df[,7]<df[,9]
    }else if(input$locVis == "Visitante"){
      equipoSelec <- df[,8]==input$equ
      condicionVictoria <- df[,9]>df[,7]
      condicionDerrota <- df[,9]<df[,7]
    }    
    condicionClima <- df[,10] >=rangoGrados[1] & df[,10] <=rangoGrados[2]
    condicionAltura <- df[,11] >=rangoAltura[1] & df[,11] <=rangoAltura[2]  
    dfResultadoVictoria <-df[condicionVictoria & equipoSelec & condicionClima &
                               condicionAltura,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    

    dfResultadoEmpate <-df[condicionEmpate & equipoSelec & condicionClima &
                             condicionAltura,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    
    dfResultadoDerrota <-df[condicionDerrota & equipoSelec & condicionClima &
                              condicionAltura,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    #Se obtiene la frecuencia de equipos locales ganadores
    if(input$locVis == "Local"){
      resultadoDfV <- dfResultadoVictoria$Equipo.Local
      resultadoDfE <- dfResultadoEmpate$Equipo.Local
      resultadoDfD <- dfResultadoDerrota$Equipo.Local
    }else if(input$locVis == "Visitante"){
      resultadoDfV <- dfResultadoVictoria$Equipo.Visitante
      resultadoDfE <- dfResultadoEmpate$Equipo.Visitante
      resultadoDfD <- dfResultadoDerrota$Equipo.Visitante
    } 
    frecuencia <-data.frame(
      Equipo=table(resultadoDfV))
    cantVictoria <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(resultadoDfE))
    cantEmpate <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(resultadoDfD))
    cantDerrota <-frecuencia$Equipo.Freq
    
    mydf <- data.frame( Resultados=c(cantVictoria,cantEmpate,cantDerrota))
      barplot(as.matrix(mydf),
           beside=TRUE,
           horiz = F,
           col=c("darkblue","orange","red"),
           names.arg=c("Victorias", "Empates" , "Derrotas"), 
           width = 0.2,
           ylab = "Frecuencia",
           main="Consolidado de resultados") 
      
      sumaGoles <-sum(dataFrameGanadorLocal$Goles.Equipo.Local)
      })
  
  
  
  
  
  
  observeEvent(input$preview, {
    rangoGrados <- c(input$clima)
    rangoAltura <- c(input$altura)
    
    condicionEmpate <- df[,7]==df[,9]
    if(input$locVis == "Local"){
      equipoSelec <- df[,6]==input$equ
      condicion <- df[,6]==input$equ
      condicionVictoria <- df[,7]>df[,9]
      condicionDerrota <- df[,7]<df[,9]
    }else if(input$locVis == "Visitante"){
      equipoSelec <- df[,8]==input$equ
      condicion <- df[,8]==input$equ
      condicionVictoria <- df[,9]>df[,7]
      condicionDerrota <- df[,9]<df[,7]
    }
    condicionClima <- df[,10] >=rangoGrados[1] & df[,10] <=rangoGrados[2]
    condicionAltura <- df[,11] >=rangoAltura[1] & df[,11] <=rangoAltura[2]
    dataFrameGanadorLocal <-df[condicion  & condicionClima &
                                 condicionAltura,
                               c("Equipo.Local",
                                 "Equipo.Visitante",
                                 "Goles.Equipo.Local",
                                 "Goles.Equipo.Visitante",
                                 "Temperatura","Altitud")]
    
    dfResultadoVictoria <-df[condicionVictoria & equipoSelec & condicionClima &
                               condicionAltura,
                             c("Equipo.Local","Goles.Equipo.Local",
                               "Goles.Equipo.Visitante","Equipo.Visitante")]
    
    
    dfResultadoEmpate <-df[condicionEmpate & equipoSelec & condicionClima &
                             condicionAltura,
                           c("Equipo.Local","Goles.Equipo.Local",
                             "Goles.Equipo.Visitante","Equipo.Visitante")]
    
    dfResultadoDerrota <-df[condicionDerrota & equipoSelec & condicionClima &
                              condicionAltura,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]   
    #Se obtiene la frecuencia de equipos locales ganadores

    if(input$locVis == "Local"){
      equipoSeleccionado <- dataFrameGanadorLocal$Equipo.Local
      sumaGolesFavor <-sum(dataFrameGanadorLocal$Goles.Equipo.Local)
      sumaGolesContra <-sum(dataFrameGanadorLocal$Goles.Equipo.Visitante)
      resultadoDfV <- dfResultadoVictoria$Equipo.Local
      resultadoDfE <- dfResultadoEmpate$Equipo.Local
      resultadoDfD <- dfResultadoDerrota$Equipo.Local
    }else if(input$locVis == "Visitante"){
      equipoSeleccionado <- dataFrameGanadorLocal$Equipo.Visitante
      sumaGolesFavor <-sum(dataFrameGanadorLocal$Goles.Equipo.Visitante)
      sumaGolesContra <-sum(dataFrameGanadorLocal$Goles.Equipo.Local)
      resultadoDfV <- dfResultadoVictoria$Equipo.Visitante
      resultadoDfE <- dfResultadoEmpate$Equipo.Visitante
      resultadoDfD <- dfResultadoDerrota$Equipo.Visitante
    }
    frecuencia <-data.frame(
      Equipo=table(resultadoDfV))
    cantVictoria <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(resultadoDfE))
    cantEmpate <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(resultadoDfD))
    cantDerrota <-frecuencia$Equipo.Freq
    
    #Se obtiene la frecuencia de equipos locales ganadores
    frecuenciaGananciaLocal <-data.frame(
      Equipo=table(equipoSeleccionado))
    totalPartidos <- cantVictoria + cantDerrota + cantEmpate
    promedioGoles <- sumaGolesFavor/totalPartidos
    promedioGoles <-format(round(promedioGoles, 2), nsmall = 2)
    showModal(modalDialog(
      footer = modalButton("Aceptar"),
      title = paste ("Estadisticas de ", input$equ,sep = " ", collapse = NULL),
      HTML("Goles anotados: ",sumaGolesFavor,"</br>
            Goles recibidos: ",sumaGolesContra,"</br>
           Partidos ganados: ",cantVictoria,"</br>
           Partidos perdidos: ",cantDerrota,"</br>
           Partidos empatados: ",cantEmpate,"</br>
           Total Partidos: ",totalPartidos,"</br>
           Rango de temperatura: ",rangoGrados[1],"C° - ",rangoGrados[2],"C°</br>
           Rango de Altitud: ",rangoAltura[1],"mts - ",rangoAltura[2],"mts</br>
           Promedio de goles: ", promedioGoles,"</br>"
      )
    ))
  })
  
}


#Unir UI y Server
shinyApp(ui = ui, server = server)
