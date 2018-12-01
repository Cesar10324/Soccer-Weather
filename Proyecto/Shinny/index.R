library(shiny)
library(knitr)
library(ggplot2)
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
          
          downloadButton("report", "Generate report")
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

server <- function (input, output){
  
  output$report = downloadHandler(
    filename = 'myreport.pdf',
    
    content = function(file) {
      out = knit2pdf('input.Rnw', clean = TRUE)
      file.rename(out, file) # move pdf to file for downloading
    },
    
    contentType = 'application/pdf'
  )
  
  
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
    if(input$locVis == "Victoria"){
      condicion <- df[,8]>df[,10]
    }else if(input$locVis == "Derrota"){
      condicion <- df[,8]<df[,10]
      
    }else{
      condicion <- df[,8]==df[,10]
    }
    
      dataFrameGanadorLocal <-df[df[,7]==input$equ,
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
    frecuencia <-data.frame(
      Equipo=table(dfResultadoVictoria$Equipo.Local))
    cantVictoria <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(dfResultadoEmpate$Equipo.Local))
    cantEmpate <-frecuencia$Equipo.Freq
    
    frecuencia <-data.frame(
      Equipo=table(dfResultadoDerrota$Equipo.Local))
    cantDerrota <-frecuencia$Equipo.Freq
    
    mydf <- data.frame( Resultados=c(cantVictoria,cantEmpate,cantDerrota))
      barplot(as.matrix(mydf),
           beside=TRUE,
           horiz = F,
           col=c("green","orange","red"),
           width = 0.5,
           ylab = "Frecuencia",
           main="Ganancia durante la temporada equipo Local")  
      
    
    
  })
  
  output$downloadReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(n = input$slider)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}


#Unir UI y Server
shinyApp(ui = ui, server = server)
