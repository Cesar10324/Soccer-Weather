library(shiny)
library(shinyWidgets)

#Preprocesado de la aplicacion

#mpgData <- mtcars
#mpgdataSam <- factor (mpgDataSam, labels = c("Automatico", "Manual"))

setwd("~/Diplomado/Proyecto/DataSets")
#Se obtiene la data respectiva
resultados <-read.csv("LigaSantander_2017-test.csv",sep = ",",stringsAsFactors = F)

#Se crea el dataFrame
df <-data.frame(resultados)
equiposDist <-unique(df$Equipo.Local)
equiposDist <-equiposDist[order(equiposDist,decreasing=F)]



campo <-c("Victoria","Derrota","Empate")

#Se crea el vector de ligas
ligas <- c("Santander","Bundesliga","Premier League","Serie A","Premier de rusia")
ligas <-ligas[order(ligas,decreasing=F)]

#Interfaz Usuario
ui <- pageWithSidebar(
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
                      min = -5, max = 38, value = c(20, 30)
          )
        ),     
        
        #Crear mainPanel que visualiza resultados
        mainPanel(
          
          #Final
          #Salida 1: Texto formateado de output$texto
          h3(textOutput("texto")),
          
          #Salida dos
          plotOutput("futPlot"),
          
          #Salida tres
          plotOutput("resultados")
        )
)


#Logica del servidor

server <- function (input, output){
  
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
    if(input$locVis == "Victoria"){
      condicion <- df[,8]>df[,10]
    }else if(input$locVis == "Derrota"){
      condicion <- df[,8]<df[,10]
      
    }else{
      condicion <- df[,8]==df[,10]
    }
    condicionClima <- df[,11] >=rangoGrados[1] & df[,11] <=rangoGrados[2]
    condicionAltura <- df[,12] >=rangoAltura[1] & df[,12] <=rangoAltura[2]
    
      dataFrameGanadorLocal <-df[df[,7]==input$equ & condicionClima &
                                   condicionAltura,
                                 c("Goles.Equipo.Local",
                                   "Goles.Equipo.Visitante",
                                   "Clima","Altura")]
      
      #Se obtiene la frecuencia de equipos locales ganadores
      frecuenciaGananciaLocal <-data.frame(
        Equipo=table(dataFrameGanadorLocal$Equipo.Local))
      if(!is.null(frecuenciaGananciaLocal)){
        
        ggplot(dataFrameGanadorLocal,
               aes(x = Clima,y = Goles.Equipo.Local,
                   size=Goles.Equipo.Local,
                   color=Clima))+geom_point()   
      }
      else{
        cat("FALLA")
      }
  })
  
  #as.formula(formulaTexto())
  output$resultados <- renderPlot({
    if(input$locVis == "Victoria"){
      condicion <- df[,8]>df[,10]
    }else if(input$locVis == "Derrota"){
      condicion <- df[,8]<df[,10]
      
    }else{
      condicion <- df[,8]==df[,10]
    }
    condicion <- df[,8]>df[,10]
    dfResultadoVictoria <-df[condicion & df[,7]==input$equ,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    
    condicion <- df[,8]==df[,10]
    dfResultadoEmpate <-df[condicion & df[,7]==input$equ,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    condicion <- df[,8]<df[,10]
    dfResultadoDerrota <-df[condicion & df[,7]==input$equ,
                            c("Equipo.Local","Goles.Equipo.Local",
                              "Goles.Equipo.Visitante","Equipo.Visitante")]
    #Se obtiene la frecuencia de equipos locales ganadores
      barplot(table(dfResultadoVictoria$Equipo.Local), 
              table(dfResultadoVictoria$Equipo.Visitante),
           horiz = F,
           col=c("orange","blue"),
           width = 0.5,
           ylab = "Frecuencia",
           main="Ganancia durante la temporada equipo Local")     
    
    
  })
  
  
}


#Unir UI y Server
shinyApp(ui = ui, server = server)