library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(vcd)
library(coin)
library(DescTools)
library(MASS)

ui <- dashboardPage(
  dashboardHeader(title = "Analisis Estadistico para datos cualitativos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Datos", tabName = "datos", icon = icon("database")),
      menuItem("Estadística Descriptiva", tabName = "descriptiva", icon = icon("chart-bar")),
      menuItem("Estadística Inferencial", tabName = "inferencial", icon = icon("project-diagram")),
      menuItem("Pruebas No Paramétricas", tabName = "no_param", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "datos",
              fluidRow(
                box(title = "Cargar Datos", width = 12,
                    fileInput("file1", "Seleccione archivo CSV",
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    checkboxInput("header", "Encabezado", TRUE),
                    radioButtons("sep", "Separador",
                                 choices = c(Coma = ",",
                                             PuntoComa = ";",
                                             Tabulador = "\t"),
                                 selected = ","),
                    radioButtons("quote", "Comillas",
                                 choices = c(Ninguna = "",
                                             "Doble" = '"',
                                             "Simple" = "'"),
                                 selected = '"')
                ),
                box(title = "Vista previa", width = 12,
                    DTOutput("contents")
                )
              )
      ),
      
      tabItem(tabName = "descriptiva",
              fluidRow(
                box(title = "Opciones Descriptivas", width = 4,
                    selectInput("var_cuali", "Seleccione variable cualitativa:", choices = NULL),
                    selectInput("var_cuali2", "Seleccione segunda variable (opcional):", choices = NULL),
                    radioButtons("tipo_grafico", "Tipo de gráfico:",
                                 choices = c("Barras" = "barras",
                                             "Sectores" = "pastel",
                                             "Tabla" = "tabla"),
                                 selected = "barras")
                ),
                box(title = "Resultados Descriptivos", width = 8,
                    verbatimTextOutput("resumen_descriptivo"),
                    plotOutput("grafico_descriptivo"),
                    DTOutput("tabla_frecuencias")
                )
              )
      ),
      
      tabItem(tabName = "inferencial",
              fluidRow(
                box(title = "Opciones Inferenciales", width = 4,
                    selectInput("var_inf1", "Variable 1:", choices = NULL),
                    selectInput("var_inf2", "Variable 2:", choices = NULL),
                    radioButtons("prueba_inf", "Prueba estadística:",
                                 choices = c("Chi-cuadrado" = "chisq",
                                             "Fisher" = "fisher",
                                             "V de Cramer" = "cramer",
                                             "Coef. Phi" = "phi",
                                             "Lambda" = "lambda",
                                             "Coef. Contingencia" = "cont")),
                    actionButton("calcular_inf", "Calcular")
                ),
                box(title = "Resultados Inferenciales", width = 8,
                    verbatimTextOutput("resultado_inferencial")
                )
              )
      ),
      
      tabItem(tabName = "no_param",
              fluidRow(
                box(title = "Opciones No Paramétricas", width = 4,
                    selectInput("var_resp", "Variable respuesta:", choices = NULL),
                    selectInput("var_grupo", "Variable de agrupación:", choices = NULL),
                    radioButtons("prueba_np", "Prueba no paramétrica:",
                                 choices = c("Mann-Whitney" = "mw",
                                             "Kruskal-Wallis" = "kw",
                                             "Wilcoxon" = "wx",
                                             "Friedman" = "fried",
                                             "Spearman" = "spear",
                                             "Kolmogorov-Smirnov" = "ks")),
                    actionButton("calcular_np", "Calcular")
                ),
                box(title = "Resultados No Paramétricos", width = 8,
                    verbatimTextOutput("resultado_noparam")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    updateSelectInput(session, "var_cuali", choices = names(df))
    updateSelectInput(session, "var_cuali2", choices = c("Ninguna", names(df)))
    updateSelectInput(session, "var_inf1", choices = names(df))
    updateSelectInput(session, "var_inf2", choices = names(df))
    updateSelectInput(session, "var_resp", choices = names(df))
    updateSelectInput(session, "var_grupo", choices = names(df))
    return(df)
  })
  
  output$contents <- renderDT({
    datatable(datos(), options = list(scrollX = TRUE))
  })
  
  output$resumen_descriptivo <- renderPrint({
    req(input$var_cuali)
    var <- datos()[[input$var_cuali]]
    
    cat("=== FRECUENCIAS ABSOLUTAS ===\n")
    print(table(var))
    
    cat("\n=== FRECUENCIAS RELATIVAS ===\n")
    print(prop.table(table(var)))
    
    cat("\n=== MEDIDAS RESUMEN ===\n")
    moda <- names(sort(table(var), decreasing = TRUE))[1]
    cat("Moda:", moda, "\n")
    
    if(!is.null(input$var_cuali2) && input$var_cuali2 != "Ninguna") {
      var2 <- datos()[[input$var_cuali2]]
      cat("\n=== TABLA DE CONTINGENCIA ===\n")
      print(table(var, var2))
      
      cat("\n=== RAZÓN ENTRE CATEGORÍAS ===\n")
      tab <- table(var, var2)
      if(nrow(tab) >= 2 && ncol(tab) >= 2) {
        razon <- tab[1,1] / tab[2,2]
        cat("Razón entre [1,1] y [2,2]:", razon, "\n")
      }
    }
  })
  
  output$grafico_descriptivo <- renderPlot({
    req(input$var_cuali)
    var <- datos()[[input$var_cuali]]
    df <- as.data.frame(table(var))
    names(df) <- c("Categoria", "Frecuencia")
    
    if(input$tipo_grafico == "barras") {
      ggplot(df, aes(x = Categoria, y = Frecuencia, fill = Categoria)) +
        geom_bar(stat = "identity") +
        labs(title = "Gráfico de Barras", x = input$var_cuali, y = "Frecuencia") +
        theme_minimal()
    } else if(input$tipo_grafico == "pastel") {
      ggplot(df, aes(x = "", y = Frecuencia, fill = Categoria)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Gráfico de Sectores", fill = input$var_cuali) +
        theme_void()
    }
  })
  
  output$tabla_frecuencias <- renderDT({
    req(input$var_cuali)
    var <- datos()[[input$var_cuali]]
    df <- as.data.frame(table(var))
    df$Relativa <- prop.table(df$Freq)
    names(df) <- c("Categoría", "Frecuencia Absoluta", "Frecuencia Relativa")
    datatable(df)
  })
  
  observeEvent(input$calcular_inf, {
    output$resultado_inferencial <- renderPrint({
      req(input$var_inf1, input$var_inf2)
      var1 <- datos()[[input$var_inf1]]
      var2 <- datos()[[input$var_inf2]]
      tab <- table(var1, var2)
      
      cat("=== TABLA DE CONTINGENCIA ===\n")
      print(tab)
      
      if(input$prueba_inf == "chisq") {
        cat("\n=== PRUEBA CHI-CUADRADO ===\n")
        if(length(dim(tab)) == 1) {
          prueba <- chisq.test(tab)
          print(prueba)
        } else {
          prueba <- chisq.test(tab)
          print(prueba)
        }
      } else if(input$prueba_inf == "fisher") {
        cat("\n=== PRUEBA EXACTA DE FISHER ===\n")
        prueba <- fisher.test(tab)
        print(prueba)
      } else if(input$prueba_inf == "cramer") {
        cat("\n=== V DE CRAMER ===\n")
        cramer <- sqrt(chisq.test(tab)$statistic / (sum(tab) * (min(dim(tab)) - 1)))
        names(cramer) <- "V de Cramer"
        print(cramer)
      } else if(input$prueba_inf == "phi") {
        cat("\n=== COEFICIENTE PHI ===\n")
        phi <- sqrt(chisq.test(tab)$statistic / sum(tab))
        names(phi) <- "Coeficiente Phi"
        print(phi)
      } else if(input$prueba_inf == "lambda") {
        cat("\n=== LAMBDA DE GOODMAN Y KRUSKAL ===\n")
        lambda <- Lambda(tab)
        print(lambda)
      } else if(input$prueba_inf == "cont") {
        cat("\n=== COEFICIENTE DE CONTINGENCIA ===\n")
        cont <- sqrt(chisq.test(tab)$statistic / (chisq.test(tab)$statistic + sum(tab)))
        names(cont) <- "Coeficiente de Contingencia"
        print(cont)
      }
    })
  })
  
  observeEvent(input$calcular_np, {
    output$resultado_noparam <- renderPrint({
      req(input$var_resp, input$var_grupo)
      resp <- datos()[[input$var_resp]]
      grupo <- datos()[[input$var_grupo]]
      
      if(input$prueba_np == "mw") {
        cat("=== PRUEBA DE MANN-WHITNEY ===\n")
        prueba <- wilcox.test(resp ~ grupo, exact = FALSE)
        print(prueba)
      } else if(input$prueba_np == "kw") {
        cat("=== PRUEBA DE KRUSKAL-WALLIS ===\n")
        prueba <- kruskal.test(resp ~ grupo)
        print(prueba)
      } else if(input$prueba_np == "wx") {
        cat("=== PRUEBA DE WILCOXON (PARA DATOS APAREADOS) ===\n")
        prueba <- wilcox.test(resp, grupo, paired = TRUE, exact = FALSE)
        print(prueba)
      } else if(input$prueba_np == "fried") {
        cat("=== PRUEBA DE FRIEDMAN ===\n")
        prueba <- friedman.test(resp ~ grupo | rep(1:length(resp)))
        print(prueba)
      } else if(input$prueba_np == "spear") {
        cat("=== COEFICIENTE DE CORRELACIÓN DE SPEARMAN ===\n")
        prueba <- cor.test(resp, as.numeric(grupo), method = "spearman")
        print(prueba)
      } else if(input$prueba_np == "ks") {
        cat("=== PRUEBA DE KOLMOGOROV-SMIRNOV ===\n")
        grupo1 <- resp[grupo == levels(grupo)[1]]
        grupo2 <- resp[grupo == levels(grupo)[2]]
        prueba <- ks.test(grupo1, grupo2)
        print(prueba)
      }
    })
  })
}

shinyApp(ui, server)