#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyFiles)
library(DT)


source("src/functions.R")
source("src/funcion_berlin.R")

#input <- c("data/Procolo_COVID-19_Prueba1_4Abr20.eds")
#output <- c("results/")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  #titlePanel( div(column(width = 6, tags$img(src = "images/inmegen.jpg"))),
  #                column(width = 6, h2("Aplicaci??n para an??lisis de RT-PCR")), 
  #            windowTitle="MyPage"),
  
  titlePanel( div(column(width = 6, h1("ARPA - Generador de reportes")), 
                  column(width = 4, tags$img(src = "images/inmegen.jpg"))),
              windowTitle="rt-PCR-analysis"),
  
  hr(),
  
  ###### SIDE BAR - CONTROLADOR DE ACCIONES
  sidebarPanel(
    
    ######## h2("Selecciona el archivo a procesar"),
    #fileInput("rtpcr", "Sube el archivo a procesar"),
    h5('Selecciona el archivo EDS a procesar'),
    shinyFilesButton('file_eds', 'Archivo EDS', 'Selecciona el archivo EDS a procesar', FALSE),
    hr(),
    
    
    ######## h2("Selecciona el directorio para los resultados"),
    h5('Selecciona el directorio para almacenar los resultados'),
    shinyDirButton('directory', 'Directorio de resultados', 'Selecciona el directorio para almacenar los resultados'),
    #shinyDirChoose(input, 'out_dir', roots = c(home = '~')),
    #fileInput("dir_out", "Selecciona el directorio para almacenar los resultados"),
    hr(),
    
    ######## BUTTON TO GENERATE SUMMARY TABLE
    h5('Presiona para analizar los datos de la corrida'),
    actionButton("analizar", "Analizar corrida"),
    hr()

  ),
  
  ###### DISE??O DEL PANEL PARA IMPRESION DE RESULTADOS
  mainPanel(
    
    tabsetPanel(
      id = "navbar",
      tabPanel(title = "Tabla Resumen",
               value = "table",
               
               ###### PROTOCOLO
               fluidRow(
                 h4("Protocolo seleccionado"),
                 textOutput("protocolo")
               ),
               hr(),
               hr(),
               
               ###### ARCHIVO A PROCESAR    
               fluidRow(
                 h4("Archivo EDS seleccionado"),
                 textOutput("input_eds_file")
               ),
               hr(),
               hr(),
               
               
               ###### DIRECTORIO DE RESULTADOS
               fluidRow(
                 h4("Directorio de resultados seleccionado"),
                 textOutput("output_dir")
               ),
               hr(),
               hr(),
               
               ###### TABLA DE RESULTADOS
               fluidRow( 
                 h4("Tabla de resultados"),
                 #textOutput("run_ready")
                 dataTableOutput(outputId = 'run_ready')
               )
      ),
      tabPanel(title = "Curvas QC",
               value = "curves", 
               h3(textOutput("caption1")),
               plotOutput("plot1"),
               br(),
               br(),
               br(),
               h2(textOutput("caption2")),
               plotOutput("plot2"),
               br(),
               br(),
               br(),
               h2(textOutput("caption3")),
               plotOutput("plot3")
               #dataTableOutput(outputId = 'summary_table_gene')
      ), 
      tabPanel(title = "Curvas muestra",
               value = "samples", 
               
               h3("Selecciona una muestra para visualizar sus curvas de amplificacion"),
               selectInput(inputId = "sample", label = "", choices = NULL),
               br(),
               br(),
               
               h3(textOutput("caption_sample")),
               plotOutput("plot_sample")
      )
    )
  )
)


###### SERVIDOR
server <- function(input, output, session) {
  
  ###### LEER ESTRUCTURA DE DIRECTORIOS LOCAL
  ###### DEPENDE DEL SISTEMA OPERATIVO
  osSystem <- Sys.info()["sysname"]
  
  if (osSystem == "Darwin"){
    
    volumes <- getVolumes()()
    ###### DESPLIEGUE PARA LA ELECCION DEL ARCHIVO EDS A PROCESAR
    shinyFileChoose(input,'file_eds', roots=volumes, session=session)
    
    input_eds_file <- reactive({
      inFile <- parseFilePaths(volumes, input$file_eds)
      inFile.path <- as.character(inFile$datapath)
    })
    
    ###### DESPLIEGUE PARA LA ELECCION DEL DIRECTORIO DE SALIDA
    
    shinyDirChoose(input, 'directory', roots=volumes, session=session)
    
    output_dir <- reactive({
      return(print(parseDirPath(volumes, input$directory)))
    })
  }
  else{
    ###### DESPLIEGUE PARA LA ELECCION DEL ARCHIVO EDS A PROCESAR
    shinyFileChoose(input,'file_eds', roots=c('wd' = '/home/'), session=session)
    
    input_eds_file <- reactive({
      inFile <- parseFilePaths(c('wd' = '/home/'), input$file_eds)
      inFile.path <- as.character(inFile$datapath)
    })
    
    
    ###### DESPLIEGUE PARA LA ELECCION DEL DIRECTORIO DE SALIDA
    
    shinyDirChoose(input, 'directory', roots=c('wd' = '/home/'), session=session)
    
    output_dir <- reactive({
      return(print(parseDirPath(c('wd' = '/home/'), input$directory)))
    })
  }
      
    
  ####### IMPRIMIR LA RUTA DEL ARCHIVO EDS A PROCESAR
  output$input_eds_file <- renderText({
    input_eds_file()
  })
  
  
  ####### IMPRIMIR EL DIRECTORIO DE SALIDA
  output$output_dir <- renderText({
    output_dir()
  })
  
  ####### CORRER EL PROCESO DE CLASIFICACION AL DARLE CLICK AL BOTON ANALIZAR
  table_out <- eventReactive(input$analizar, {
    
    
    rtpcr <- input_eds_file()
    
    if (is.null( rtpcr))
      return("NO EXISTE ARCHIVO DE ENTRADA")
    
    output <- output_dir()
    
    ######## VALIDATE THAT  INPUT EDS FILE WAS SELECTED
    ######## OTHERWISE PRINT TEXT DESCRIBIING THE ERROR
    validate(
      need(rtpcr != "", "NO EDS FILE WAS SELECTED")
    )
    
    ######## VALIDATE THAT OUTPUT DIRECTORY WAS SELECTED
    ######## OTHERWISE PRINT TEXT DESCRIBIING THE ERROR
    validate(
      need(output != "", "NO OUTPUT DIRECTORY WAS SELECTED")
    )
    
    withProgress(message = 'corriendo analisis', value = 0.3, {
      all_results <- funcion_berlin(
        input_eds = rtpcr, 
        output = paste(output, "/", sep=""))
    })
    
    return(all_results)
    
  })
  
  ####### DESPLEGAR TABLA DE RESULTADOS
  output$run_ready <- renderDataTable({
    table_out()
    
    qc <- table_out()$qc_results
    
    if (qc$QC != "PASS"){
      datatable(table_out()$test_results) %>% 
        formatStyle('gen_e', 
                    target='row',
                    backgroundColor = "yellow" )
    }else{
      datatable(table_out()$test_results) %>% 
        formatStyle( 'classification', 
                     target = 'row',
                     backgroundColor = styleEqual(c("Positivo", "Negativo"),
                                                  c('pink', 'aquamarine')) )
    }
  })
  
  
  ################################################################################
  # IMPRIMIR CURVAS DE QC 
  ################################################################################
  
  output$caption1 <- renderText({
    "NTC"
  })
  
  output$plot1 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    (plots[['NTC']] + ggtitle("NTC"))
  })
  
  output$caption2 <- renderText({
    "PTC"
  })
  
  output$plot2 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    (plots[['PTC']] + ggtitle('PTC'))
  })
  
  output$caption3 <- renderText({
    'CRE'
  })
  
  output$plot3 <- renderPlot({
    table_out()
    plots <- table_out()$single_plots
    (plots[['CRE']] + ggtitle('CRE'))
  })
  
  ################################################################################
  #Get sample names for drop-down menu
  ################################################################################
  
  observe({
    updateSelectInput(session = session, inputId = "sample", choices = table_out()[['test_results']]$sample_name)
  })
  
  output$caption_sample <- renderText({
    table_out()
    sample <- input$sample
    sample
  })
  
  output$plot_sample <- renderPlot({
    table_out()
    sample <- input$sample
    plots <- table_out()$single_plots
    plot <- plots[[sample]]
    (plot)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

