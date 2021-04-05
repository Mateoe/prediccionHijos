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
library(xgboost)
library(dplyr)
library(xgboost)
library(shinydashboard)

#Lectura el conjunto de datos
datos <- read.csv("BASE_FILTRADA(sin_nas).csv")

#Selección de las varaibles relevantes del conjunto de datos
datos <- datos %>% select(I_HOGAR,
                          PERCAPITA,
                          CANT_PERSONAS_HOGAR,
                          consumo_energia,
                          edad_maxima,
                          con_pareja,
                          Hijos) 

#Nombrado de las variables del conjunto de datos
colnames(datos) <- c("Ingreso mensual",
                     "Ingreso percapita",
                     "Número de personas",
                     "Consumo de energia",
                     "Edad máxima",
                     "Con pareja",
                     "Hijos")



#Definición de la interfaz de usuario
ui <- fluidPage(
    
    #Definición del tema de la aplicación
    theme = shinytheme("sandstone"),
    
    #Definición de la barra de navegación
    navbarPage( 
        
        title = "Col Hogares",                            #Titulo de la barra de navegación
        
        #Definición de la primera pestaña de la barra de navegación
        tabPanel(
            
            "Descripción",                                #Nombre de la pestaña
            
            icon = icon("info-circle"),                   #Icono de la pestaña
            
            HTML("
                              <div style='text-align: center;margin-top:80px;'>
                                  <img src='Col_Hogares.png' width='300px'/>
                              </div>
                              <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                        
                              <p style='font-size: 20px;text-align: justify;'>
                              En este sitio web se encontrara una herramienta de predicción que servirá en la estimación
                              del número de hijos de un hogar colombiano actual, el algoritmo de predicción está basado
                              en la información presentada por el DANE en la ultima encuesta nacional de calidad de vida
                              realizada en el año 2019, el reporte técnico puede ser encontrado en el siguiente 
                              <a href='https://rpubs.com/AlejandroBedoya/Col_Hogares'>enlace</a>.
                              Además, el código empleado en la construcción de esta aplicación se encuentra alojado en el
                              siguiente <a href='https://github.com/Mateoe/prediccionHijos.git'>repositorio</a>.
                              </p>
                            ")
            
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
                    
                    
                    #Definición del control desizante para el ingreso mensual
                    numericInput("i_hogar",
                                 "Ingreso mensual del hogar:",
                                 min = 0,
                                 max = 284600000,
                                 value = 1734180,
                                 #ticks = FALSE
                    ),
                    
                    #Definición del control desizante para el ingreso percapita
                    numericInput("percapita",
                                 "Ingreso per-cápita:",
                                 min = 0,
                                 max = 120000000 ,
                                 value = 711110,
                                 #ticks = FALSE
                    ),
                    
                    #Definición del control desizante para numero de personas
                    sliderInput("personas",
                                "Número de personas en el hogar:",
                                min = 1,
                                max = 19,
                                value = 3),
                    
                    #Definición del control desizante para la energia de la ultima factura
                    numericInput("energia",
                                 "Pago por consumo de energía en la ultima factura:",
                                 min = 0,
                                 max = 284600000,
                                 value = 46161,
                                 #ticks = FALSE
                    ),
                    
                    #Definición del control desizante para la edad máxima entre el jefe y el conyugue
                    sliderInput("edad_maxima",
                                "Edad maxima entre el conyugue y el jefe de familia:",
                                min = 13,
                                max = 113,
                                value = 49),
                    
                    
                    #Definición de las opciones para la situacion sentimental del jefe del hogar
                    radioButtons("con_pareja", 
                                 "Situación sentimental del jefe del hogar:",
                                 c("1. Con pareja que vive en el hogar" = 1,
                                   "2. Sin pareja" = 2
                                 )
                    ),
                ),
                
                
                #Definición del panel de variables de salida
                mainPanel(
                    
                    splitLayout(
                        #Definición del panel que refleja las variables de entrada
                        tableOutput("entradas"),
                        
                        
                        column(8, wellPanel(
                            span(textOutput("predText"), style="font-size: 22px"),
                            verbatimTextOutput("prediccion")
                        ))
                    ),
                    
                    
                    
                    
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

    #Texto de salida de la predicción
    output$predText <- renderText({
        as.character("Predicción")
    })
    
    
    #Función de predicción
    prediccion2<-reactive({
        
        #--------------------- Modelo --------------------#
        #Lectura del modelo
        mod_xg <- readRDS("modelo_xg1.rds")
        
        #Variables de entradas al modelo
        entradas<-c(as.numeric(input$i_hogar),
                    as.numeric(input$percapita),
                    as.numeric(input$personas),
                    as.numeric(input$energia),
                    as.numeric(input$edad_maxima),
                    as.numeric(input$con_pareja))
        
        #Se convierten las entradas a data frame
        m <- as.data.frame(t(entradas))
        
        #Se establecen los nombres de las columnas del dataframe
        colnames(m) <- c("I_HOGAR",
                         "PERCAPITA", 
                         "CANT_PERSONAS_HOGAR",
                         "consumo_energia",
                         "edad_maxima",
                         "con_pareja")
        
        #Se convierte el data frame a matriz
        sub_test_matrix <- as.matrix(m)
        
        #Se realiza la predicción
        test_pred2 <- predict(mod_xg, newdata = sub_test_matrix)
        
        #Se extrae el número de hijos predicho
        #se resta 1 dado que los grupos se re etiquetaron de 1-12
        #Y con este procedimiento se devuelven al rango 0-11
        prediccion <- max.col(t(test_pred2), "last")-1 
        
        #Se retorna la predicción
        return(prediccion)
        })
    
    
    #Salida de la predicción
    output$prediccion <- renderText({
        prediccion2()
    })

    
    
    #--------------------- Tabla que refleja las entradas --------------------#
    #Creación del dataframe de varaibles de entrada
    variables_entrada <- reactive({

        entradas<-c(as.numeric(input$i_hogar),
                    as.numeric(input$percapita),
                    as.numeric(input$personas),
                    as.numeric(input$energia),
                    as.numeric(input$edad_maxima),
                    as.numeric(input$con_pareja))
        
        
        #--------------------- salida de datos --------------------#
        data.frame(
            Caracteristica = c("Ingreso mensual del hogar",
                               "Ingreso per-cápita",
                               "Número de personas en el hogar",
                               "Pago por consumo de energía en la ultima factura",
                               "Edad maxima entre el conyugue y el jefe de familia",
                               "Situación sentimental del jefe del hogar"),
            Valor = as.integer(entradas)
        )
        
    })
    
    #Variable de salida que refleja las variables de entrada
    output$entradas <- renderTable({
        variables_entrada()
    })
    
    
    #--------------------- Tabs --------------------#
    
    #Variable de salida que contiene el gráfico de la distribución de la cantidad de hijos
    output$distribucion <- renderPlot({
        
        #Creación del gráfico de la distribución de los hijos
        ggplot(datos, aes(x = Hijos)) +
            
            #Definición del gráfico como histograma
            geom_bar(col = "black", fill = "#738ACD") +
            
            #Definición del titulo del histograma
            ggtitle("Frecuencia de la cantidad de hijos")+
            
            #Configuración del tema del gráfico
            theme(plot.title = element_text(hjust = 0.5, size = 22),
                  text = element_text(size=17),
                  plot.caption = element_text(hjust = 0, 
                                              size = 12, 
                                              colour="#738ACD"))+
            
            #Definición de los textos del gráfico
            labs(
                y = "Frecuencia",
                caption = "En los hogares colombianos lo mas frecuente es no tener hijos o tener de uno a tres hijos")+
            
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
        apply(datos, 2, as.integer),
        
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
