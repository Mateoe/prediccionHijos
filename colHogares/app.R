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
library(dplyr)
library(DT)


datos <- read.csv("../Bases de datos/BASE_FILTRADA(sin_nas).csv")
datos <- datos %>% select(CANT_PERSONAS_HOGAR,edad_jefe,conyugue_hogar,edad_conyugue,sexo, Hijos)
colnames(datos) <- c("Personas hogar","Edad jefe","Conyugue hogar","Edad conyugue","sexo", "Hijos")
# Define UI for application that draws a histogram

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                navbarPage( title = "Col Hogares",
                            
                            tabPanel("Descripción", 
                                     
                                     titlePanel("Descripción de la aplicación")
                                     ),
                            
                            
                            tabPanel("Predicción", 
                                     
                                     titlePanel("Modelo de predicción"),
                                     
                                     # Sidebar with a slider input for number of bins 
                                     sidebarLayout(
                                         
                                         sidebarPanel(
                                             
                                             sliderInput("personas",
                                                         "Número de personas en el hogar:",
                                                         min = 1,
                                                         max = 19,
                                                         value = 3),
                                             
                                             sliderInput("edad_jefe",
                                                         "Edad del jefe de familia:",
                                                         min = 13,
                                                         max = 113,
                                                         value = 20),
                                             
                                             radioButtons("conyugue_hogar", 
                                                          "¿El conyugue vive en el hogar?",
                                                          c("Si" = 1,
                                                            "No" = 2
                                                          )
                                             ),
                                             
                                             sliderInput("edad_conyugue",
                                                         "Edad del conyugue del jefe:",
                                                         min = 12,
                                                         max = 112,
                                                         value = 20),
                                             
                                             radioButtons("sexo", 
                                                          "Sexo del jefe del hogar:",
                                                          c("Masculino" = 1,
                                                            "Femenino" = 2
                                                            )
                                                          )
                                             
                                             
                                             
                                         ),
                                         
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                             
                                             tableOutput("entradas"),
                                             
                                             tabsetPanel(type = "tabs",
                                                         
                                                         tabPanel("Gráfico", textOutput("grafico")),
                                                         
                                                         tabPanel("Resumen", textOutput("resumen")),
                                                         
                                                         tabPanel("Tablas", DTOutput('tabla'))
                                             )
                                             
                                         )
                                     )
                                     
                                     
                                     
                                     
                            )
                            
                            
                )
                
                
                
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    #output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$personas + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #})
    
    sliderValues <- reactive({
        
        data.frame(
            Caracteristica = c("Número de personas",
                     "Edad del jefe del hogar",
                     "Conyugue vive en el hogar",
                     "Edad del conyugue",
                     "Sexo del jefe del hogar"),
            Valor = c(input$personas,
                                   input$edad_jefe,
                                   input$conyugue_hogar,
                                   input$edad_conyugue,
                                   input$sexo))
        
    })
    
    output$entradas <- renderTable({
        sliderValues()
    })
    
    output$hijos <- renderText(input$personas)
    output$grafico <- renderText("Aquí va el gráfico")
    output$resumen <- renderText("Aquí van los resumenes")
    output$tabla <- renderDT(datos,
                             filter = "top",
                             options = list(
                                 sDom  = '<"top">lrt<"bottom">ip'
                             )
    )
    
    
    #output$tabla <- renderText("Aquí van las tablas")
}

# Run the application 
shinyApp(ui = ui, server = server)
