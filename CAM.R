#options("scipen"=20)

## Pacotes ##
library(datasets)
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
library(tidyverse)
library(dplyr)
library(fdth)
library(ggplot2)
library(car)
library(purrr)
library(caTools)
library(rpart)
library(rpart.plot)
library(e1071)
library(neuralnet)
library(xgboost)
library(randomForest)
library(caret)
library(visdat)
library(multiROC)
library(pROC)
library(glmnet)
library(hnp)
require(nnet)

## Loadind data ##

ocorrencia <- read.csv('ocorrencia.csv', sep =';')
aeronave <- read.csv('aeronave.csv', sep = ';')
#ocorrencia_tipo <- read.csv('ocorrencia_tipo.csv', sep=';')
#fator_contribuinte <- read.csv('fator_contribuinte.csv', sep = ';')
#recomendacao <- read.csv('recomendacao.csv', sep = ';')

# Juntando em uma única tabela (Tabelão de dados!)

ocor_aren <- left_join(ocorrencia, aeronave, by = c('codigo_ocorrencia2' = 'codigo_ocorrencia2') )

#ocor_tipo <- left_join(ocor_aren, ocorrencia_tipo, by = c('codigo_ocorrencia1' = 'codigo_ocorrencia1') )

#ocor_contribuinte <- left_join(ocor_tipo, fator_contribuinte, by = c('codigo_ocorrencia3' = 'codigo_ocorrencia3') )

#dados <- left_join(ocor_contribuinte, recomendacao, by = c('codigo_ocorrencia4' = 'codigo_ocorrencia4') )

# Análise descritivas para o conjunto de dados
colym <- c('codigo_ocorrencia','codigo_ocorrencia1', 'codigo_ocorrencia2', 'codigo_ocorrencia3', 'codigo_ocorrencia4', 'ocorrencia_latitude', 'ocorrencia_longitude', 'aeronave_pais_fabricante', 'aeronave_pais_registro', 'ocorrencia_dia', 'ocorrencia_hora', 'divulgacao_relatorio_numero', 'aeronave_pmd', 'aeronave_pais_registro', 'aeronave_assentos', 'total_recomendacoes', 'aeronave_fabricante', 'investigacao_aeronave_liberada', 'investigacao_status', 'divulgacao_relatorio_numero', 'divulgacao_dia_publicacao', 'aeronave_assentos', 'aeronave_ano_fabricacao', 'investigacao_aeronave_liberada', 'divulgacao_relatorio_publicado', 'aeronave_voo_origem', 'aeronave_voo_destino', 'aeronave_tipo_icao', 'aeronave_fase_operacao', 'aeronave_pmd_categoria', 'aeronave_matricula', 'ocorrencia_cidade', 'ocorrencia_pais', 'ocorrencia_aerodromo', 'ocorrencia_uf', 'aeronave_modelo' )
dados <- ocor_aren %>% dplyr::select(-colym)

to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

names(dados)

variable <- names(dados)
variable
dados <- to.factors(dados, variable)

str(dados)

length(names(dados))


  write.table(dados, file = "tab_test.txt", row.names = FALSE, col.names = T, append = F) 
#write.table(tab, file = "shinySnowfall/results/tab_results.txt", row.names = FALSE, col.names = T, append = F)


summary(dados)




set.seed(13)
Class.RandomForest <- 
  randomForest(dados %>% dplyr::select(-ocorrencia_classificacao),
               as.factor(dados$ocorrencia_classificacao),
               data = dados, ntree = 100, mtry = 3, 
               importance = TRUE, proximity = TRUE)


res <- read.table('X_test.txt', h=T, sep ='\t')

res

res <- to.factors(res, variable[-1])


str(res)

str(dados)

levels(res$aeronave_fatalidades_total) <- levels(dados$aeronave_fatalidades_total)


Pred.Class.RF <- predict(Class.RandomForest, dados)




ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "MLCenipa"),
  dashboardSidebar(

               
               # Input: Select a file ----
               fileInput("file1", "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ","),
               
               # Input: Select quotes ----
               radioButtons("quote", "Quote",
                            choices = c(None = "",
                                        "Double Quote" = '"',
                                        "Single Quote" = "'"),
                            selected = '"'),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons("disp", "Display",
                            choices = c(Head = "head",
                                        All = "all"),
                            selected = "head")
               
      
    
  ),
    dashboardBody(
   
      fluidRow( width = 700, 
              
                box(title = 'Tabela de Predição ML Random Forest', DT::dataTableOutput("mytable2", height = 350))

                
      )
    
              
      )
      
      )
  

server <- shinyServer(function(input, output, session) {
  
  data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.table(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    updateSelectInput(session, inputId = 'col', label = 'Variable',
                      choices = names(df) , selected = names(df))
    
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$mytable1 <- DT::renderDataTable({
    
    data()
    
    
  })
  
  
  output$mytable2 <- DT::renderDataTable({
    
    
    
    
    datat <- to.factors(data(), variable)
    
    levels(datat$aeronave_fatalidades_total) <- levels(dados$aeronave_fatalidades_total)
    levels(datat$aeronave_nivel_dano) <- levels(dados$aeronave_nivel_dano)
    levels(datat$aeronave_tipo_operacao) <- levels(dados$aeronave_tipo_operacao)
    levels(datat$aeronave_registro_segmento) <- levels(dados$aeronave_registro_segmento)
    levels(datat$aeronave_registro_categoria) <- levels(dados$aeronave_registro_categoria)
    levels(datat$aeronave_motor_quantidade) <- levels(dados$aeronave_motor_quantidade)
    levels(datat$aeronave_motor_tipo) <- levels(dados$aeronave_motor_tipo)
    levels(datat$aeronave_tipo_veiculo) <- levels(dados$aeronave_tipo_veiculo)
    levels(datat$aeronave_operador_categoria) <- levels(dados$aeronave_operador_categoria)
    levels(datat$ocorrencia_saida_pista) <- levels(dados$ocorrencia_saida_pista)
    levels(datat$total_aeronaves_envolvidas) <- levels(dados$total_aeronaves_envolvidas)
    levels(datat$ocorrencia_classificacao) <- levels(dados$ocorrencia_classificacao)
    
    previsao <- Pred.Class.RF <- predict(Class.RandomForest, datat)
    mes <- 1:nrow(data())
    tab <- data.frame(mes, previsao)
    names(tab) <- c('Ocorrencia', 'Predição da Ocorrencia')
    tab 
    
  })
  
  
  
  
  
  
})  
shinyApp(ui = ui, server = server)


