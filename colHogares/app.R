#-------------------------------------------------------------------------------
# Aplicación creada por el equipo de Col Hogares                               
# 
# Integrantes:
#       Alejandro Bedoya Cataño
#       Estefanía Echeverry Franco
#       Juan Fernando Patino Castro
#       Mateo Espinal Londoño
#       Sebastian Agudelo Jimenez
#
# Para la asignatura: Técnicas de aprendizaje estadístico
#-------------------------------------------------------------------------------

#LEctura de las librerías
library(shiny)
library(shinythemes)
library(dplyr)
library(DT)
library(ggplot2)
library(thematic)

#Lectura el conjunto de datos
datos <- read.csv("../Bases de datos/BASE_FILTRADA(sin_nas).csv")

#Selección de las varaibles relevantes del conjunto de datos
datos <- datos %>% select(CANT_PERSONAS_HOGAR,
                          con_pareja,
                          edad_maxima,
                          PERCAPITA,
                          I_HOGAR,
                          consumo_energia,
                          Hijos)

#Nombrado de las variables del conjunto de datos
colnames(datos) <- c("Personas hogar",
                     "Con pareja",
                     "Edad máxima",
                     "Ingreso percapita",
                     "Ingreso mensual",
                     "Consumo de energia",
                     "Hijos")



#Definición de la interfaz de usuario
ui <- fluidPage(
    
    #Definición del tema de la aplicación
    theme = shinytheme("cyborg"),
    
    #Definición de la barra de navegación
    navbarPage( 
        
        title = "Col Hogares",                            #Titulo de la barra de navegación
        
        #Definición de la primera pestaña de la barra de navegación
        tabPanel(
            
            "Descripción",                                #Nombre de la pestaña
            
            icon = icon("info-circle"),                   #Icono de la pestaña
            
            titlePanel("Descripción de la aplicación")    #Titulo de la pestaña
            
        ),
        
        
        #Definición de la primera pestaña de la barra de navegación
        tabPanel(
            
            "Predicción",                                #Nombre de la pestaña
            
            icon = icon("chart-line"),                   #Icono de la pestaña
            
            titlePanel("Modelo de predicción"),          #Titulo de la pestaña
            
            
            #Definición de la capa de variables y salida de la aplicación
            sidebarLayout(
                
                #Definición del panel de variables de entrada
                sidebarPanel(
                    
                    #Definición del control desizante para numero de personas
                    sliderInput("personas",
                                "Número de personas en el hogar:",
                                min = 1,
                                max = 19,
                                value = 3),
                    
                    #Definición de las opciones para la situacion sentimental del jefe del hogar
                    radioButtons("con_pareja", 
                                 "Situación sentimental del jefe del hogar:",
                                 c("1. Con pareja que vive en el hogar" = 1,
                                   "2. Sin pareja" = 2
                                 )
                    ),
                    
                    #Definición del control desizante para la edad máxima entre el jefe y el conyugue
                    sliderInput("edad_maxima",
                                "Edad maxima entre el conyugue y el jefe de familia:",
                                min = 13,
                                max = 113,
                                value = 49),
                    
                    #Definición del control desizante para el ingreso percapita
                    sliderInput("percapita",
                                "Ingreso per-cápita:",
                                min = 0,
                                max = 120000000 ,
                                value = 711110,
                                ticks = FALSE),
                    
                    #Definición del control desizante para el ingreso mensual
                    sliderInput("i_hogar",
                                "Ingreso mensual del hogar:",
                                min = 0,
                                max = 284600000,
                                value = 1734180,
                                ticks = FALSE),
                    
                    
                    #Definición del control desizante para la energia de la ultima factura
                    sliderInput("energia",
                                "Pago por consumo de energía en la ultima factura:",
                                min = 0,
                                max = 284600000,
                                value = 46161,
                                ticks = FALSE),
                    
                ),
                
                
                #Definición del panel de variables de salida
                mainPanel(
                    
                    #Definición del panel que refleja las variables de entrada
                    tableOutput("entradas"),
                    
                    #Definición del panel de pestañas de la variable de salida
                    tabsetPanel(
                        
                        #Tipo de panel
                        type = "tabs",
                        
                        #Definición de la pestaña del gráfico de distribución
                        tabPanel("Distribución de la cantidad de hijos", 
                                 plotOutput('distribucion'), 
                                 icon = icon("bar-chart-o")),
                        
                        #Definición de la pestaña de la tabla de frecuencias
                        tabPanel("Tabla de frecuencias", 
                                 dataTableOutput("frecuencias"),
                                 icon = icon("table")),
                        
                        #Definición de la pestaña de la base de datos
                        tabPanel("Base de datos", 
                                 div(dataTableOutput('bd'), style = "font-size:85%"), 
                                 icon = icon("database")
                        )
                    )
                )
            )
        )
    )
)




# Definción de la funcionalidad lógica de la aplicación (servidor)
server <- function(input, output) {
    
    #Creación del dataframe de varaibles de entrada
    variables_entrada <- reactive({
        data.frame(
            Caracteristica = c("Número de personas en el hogar",
                               "Situación sentimental del jefe del hogar",
                               "Edad maxima entre el conyugue y el jefe de familia",
                               "Ingreso per-cápita",
                               "Ingreso mensual del hogar",
                               "Pago por consumo de energía en la ultima factura"),
            Valor = c(input$personas,
                      input$con_pareja,
                      input$edad_maxima,
                      input$percapita,
                      input$i_hogar,
                      input$energia))
        
    })
    
    #Variable de salida que refleja las variables de entrada
    output$entradas <- renderTable({
        variables_entrada()
    })
    
    #Variable de salida que contiene el gráfico de la distribución de la cantidad de hijos
    output$distribucion <- renderPlot({
        
        #Creación del gráfico de la distribución de los hijos
        ggplot(datos, aes(x = Hijos)) +
            
            #Definición del gráfico como histograma
            geom_histogram(bins = 30, ) +
            
            #Definición del titulo del histograma
            ggtitle("Frecuencia de la cantidad de hijos")+
            
            #Configuración del tema del gráfico
            theme(plot.title = element_text(hjust = 0.5),
                  text = element_text(size=14),
                  plot.caption = element_text(hjust = 0, 
                                              size = 12, 
                                              colour="orange"))+
            
            #Definición de los textos del gráfico
            labs(
                y = "Frecuencia",
                caption = "En los hogares colombianos lo mas frecuente no tener hijos o tener de uno a tres hijos")+
            
            #Definición de la escala del gráfico en el eje x
            scale_x_continuous(breaks = unique(datos$Hijos))
        
        
    })
    
    
    #Variable de salida que contiene la tabla de frecuencias de la cantidad de hijos
    output$frecuencias <- renderDataTable({
        
        #Creación de las frecuencias absolutas
        tabla <- table(as.factor(datos$Hijos))
        
        #Conversión de las frecuencias absolutas a dataframe
        frecuencias <- as.data.frame(tabla)
        
        #Creación de las frecuencias absolutas acumuladas
        frecuencias$acum <- cumsum(frecuencias$Freq)
        
        #Creación de las frecuencias relativas
        frecuencias$frecRel <- lapply(frecuencias$Freq, 
                                      function(x){
                                          round(
                                              x/max(frecuencias$acum),
                                              digits = 5)
                                          })
        
        #Creación de las frecuencias relativas acumuladas
        frecuencias$relAcum <- cumsum(frecuencias$frecRel)
        
        #Eliiminación de la notación cientifica
        options(scipen=999)
        
        #Renombrado de los nombres de las columnas de la tabla de frecuencias
        colnames(frecuencias) <- c("Hijos", 
                                   "Frecuencia", 
                                   "Frecuencia acumulada", 
                                   "Frecuencia relativa", 
                                   "Frecuencia relativa acumulada")
        
        #Retorno la tabla de frecuencias para ser añadida a la variable de salida
        return(frecuencias)},
        
        #Eliminación del indice de la tabla
        rownames = FALSE,
        #Adaptación de la tabla al estilo de la aplicación
        style = "bootstrap4",
        #Control de opciones de la tabla para ser mostrada de forma simple
        options = list(lengthChange = FALSE, pageLength = 15, dom = "t"),
    )
    
    
    #Variable de salida que contiene la tabla de la base de datos
    output$bd <- renderDT(
        
        #Conjunto de datos utilizado
        datos,
        
        #Se establecen los filtros de la parte superior
        filter = "top",
        
        #Control de opciones de la tabla para eliminar el filtro y control de filas
        options = list(
            sDom  = '<"top">lrt<"bottom">ip',
            lengthChange = FALSE
        ),
        #Adaptación de la tabla al estilo de la aplicación
        style = "bootstrap4"
    )
    
}


#Configuración tematica de los gráficos al estilo de la aplicación
thematic_shiny()

#Definición de la aplicación
shinyApp(ui = ui, server = server)
