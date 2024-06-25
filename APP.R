#options("scipen"=20)

## Pacotes ##

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinydashboardPlus)
library(shinyWidgets)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(forecast)
library(DT)

## Loadind data ##


ocorrencia <- read.csv('ocorrencia.csv', sep =';')

ocorrencia$ocorrencia_dia <- as.Date(ocorrencia$ocorrencia_dia, format = "%d/%m/%Y")

ocorrencia$ocorrencia_dia <- format(ocorrencia$ocorrencia_dia, format = '%y/%m')


filtro <- function(x, tipo = c('ACIDENTE', 'INCIDENTE', 'INCIDENTE GRAVE')){
  
  tipo <- switch(tipo,
                 "ACIDENTE" = x %>% filter( ocorrencia_classificacao == 'ACIDENTE'),
                 "INCIDENTE" = x %>% filter( ocorrencia_classificacao == 'INCIDENTE'),
                 "INCIDENTE GRAVE" = x %>% filter( ocorrencia_classificacao == 'INCIDENTE GRAVE') 

  )
  
  return(tipo)
  
}


init <- unique(ocorrencia$ocorrencia_classificacao)

prev <- 1:12

ui <- dashboardPage(
  dashboardHeader(title = "A3Data"),
  dashboardSidebar(
    selectInput('names', "Classificacao ocorrencia", init),
    selectInput('prev', "Meses de previsao (2022)", prev)
    ),
  dashboardBody(
  
    fluidRow( width = 700, 
      
      box(title = 'Série Temporal', plotOutput("test", height = 350)),
      box(title = 'Série Temporal',plotOutput("test2", height = 350)),
      box(title = 'Previsão', plotOutput("test3", height = 350)),
      box(title = 'Tabela de Previsão', DT::dataTableOutput("mytable1", height = 350))
      
    )
  ) 
)
server <- function(input, output) {
  
  output$test <- renderPlot({
    
    filther = filtro(ocorrencia, tipo = input$names)  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid <- ts( tb, start = c(2012,1), frequency = 12)    
    title = paste0(input$names, " de Sao Paulo")
  plot(ts_acid, xlab = 'Tempo (mês)', ylab='Contagem de ocorrências',
            main =  paste0('Série Temporal de', " ", input$names , " ", "do CENIPA") )
    
  })
  
  output$test2 <- renderPlot({
    
    filther = filtro(ocorrencia, tipo = init[1])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid <- ts( tb, start = c(2012,1), frequency = 12)  

    filther = filtro(ocorrencia, tipo = init[2])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid2 <- ts( tb, start = c(2012,1), frequency = 12)
    
    filther = filtro(ocorrencia, tipo = init[3])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid3 <- ts( tb, start = c(2012,1), frequency = 12)
    
    plot(ts_acid, xlab = 'Tempo (mês)', ylab='Contagem de ocorrências',
         main =  paste0("Série Temporal de ocorrências por classificação"),
         ylim =c(0,60))
    
    lines(ts_acid2, col ='blue')
    lines(ts_acid3, col ='red')
    legend("topleft",c("Incidente","Acidente", 'Incidente Grave '), 
           col=c('black','blue', 'red'), lty=1, bty="n")
    
    
  })
  
  
  output$test3 <- renderPlot({
    
    # Modelo Arima para Incidencia
    
    filther = filtro(ocorrencia, tipo = init[1])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid <- ts( tb, start = c(2012,1), frequency = 12)  
    
    mod1 <- arima(ts_acid, order = c(2,1,2))
    
    
    # Modelo Arima para Acidente
    
    filther = filtro(ocorrencia, tipo = init[2])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid2 <- ts( tb, start = c(2012,1), frequency = 12)
    
    mod2 <- arima(ts_acid2, order = c(1,1,0), seasonal = c(1,0,1))
    
    # Modelo Arima para Incidente Grave
    
    filther = filtro(ocorrencia, tipo = init[3])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid3 <- ts( tb, start = c(2012,1), frequency = 12)  
    
    mod3 <- arima(ts_acid3, order =c(0,0,0), seasonal = c(0,0,1))
    
    if (input$names == 'INCIDENTE'){
      
      plot(forecast(mod1, input$prev), main = 'Previsão do Modelo Arima para Incidente',
                    xlab='Tempo(mês)', ylab = 'Contagem de ocorrências')
      
    }
    
    if (input$names == 'ACIDENTE'){
      
      plot(forecast(mod2, input$prev), main = 'Previsão do Modelo Arima para Acidente',
                    xlab='Tempo(mês)', ylab = 'Contagem de ocorrências')
      
    }
    
    if (input$names == 'INCIDENTE GRAVE'){
      
      plot(forecast(mod3, input$prev), main = 'Previsão do Modelo Arima para Incidente Grave',
           xlab='Tempo(mês)', ylab = 'Contagem de ocorrências')
      
    }
    
    
  })
  
  output$mytable1 <- DT::renderDataTable({
    
    # Modelo Arima para Incidencia
    
    filther = filtro(ocorrencia, tipo = init[1])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid <- ts( tb, start = c(2012,1), frequency = 12)  
    
    mod1 <- arima(ts_acid, order = c(1,0,1), seasonal = c(0,0,1) )
    
    
    # Modelo Arima para Acidente
    
    filther = filtro(ocorrencia, tipo = init[2])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid2 <- ts( tb, start = c(2012,1), frequency = 12)
    
    mod2 <- arima(ts_acid2, order = c(1,1,0), seasonal = c(1,0,1))
    
    # Modelo Arima para Incidente Grave
    
    filther = filtro(ocorrencia, tipo = init[3])  
    
    data_acid <- filther %>% select(ocorrencia_dia)
    
    tb <- table(data_acid)
    
    ts_acid3 <- ts( tb, start = c(2012,1), frequency = 12)  
    
    mod3 <- arima(ts_acid3, order =c(0,0,0), seasonal = c(0,0,1))
    
    
    
    if (input$names == 'INCIDENTE'){
      
      previsao <- forecast(mod1, input$prev)
      mes <- 1:input$prev
      tab <- data.frame(mes, ceiling(previsao$mean))
      names(tab) <- c('Mês de 2022', 'Previsão')
    }
    
    if (input$names == 'ACIDENTE'){
      
      previsao <- forecast(mod2, input$prev)
      mes <- 1:input$prev
      tab <- data.frame(mes, ceiling(previsao$mean))
      names(tab) <- c('Mês de 2022', 'Previsão')
      
    }
    
    if (input$names == 'INCIDENTE GRAVE'){
      
      previsao <- forecast(mod3, input$prev)
      mes <- 1:input$prev
      tab <- data.frame(mes, ceiling(previsao$mean))
      names(tab) <- c('Mês de 2022', 'Previsão')
      
    }
    
    tab
  })
  
  
  
}

shinyApp(ui = ui, server = server)


