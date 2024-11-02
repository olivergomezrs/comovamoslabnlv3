library(shiny)
library(shinydashboard)
library(shinythemes)
library(jsonlite)
library(plotly)
library(fmsb)
library(dplyr)

# Cargar archivo JSON
json_data <- fromJSON("evaluation_data.json")

# Obtener las claves (conjuntos de datos) que terminan en ".csv"
csv_keys <- names(json_data)[grepl(".csv$", names(json_data))]

# Definir UI
ui <- dashboardPage(
  dashboardHeader(
    title = NULL,  
    tags$li(
      class = "dropdown",
      tags$div(
        style = "text-align: right; width: 100%; font-size: 24px; font-weight: bold; padding-right: 20px;",
        "LABNL ¿Cómo vamos en Datos en Nuevo León?"
      )
    )
  ),
  
  # Cambiar el color del tema a amarillo
  dashboardSidebar(
    width = 300,  
    selectInput("dataset", "Seleccione un conjunto de datos", choices = csv_keys),
    h4("Metadata"),
    verbatimTextOutput("metadata")
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Overall Data Quality Score",
        width = 6,
        plotlyOutput("qualityRadar"),  # Radar chart en esta área
        HTML("<h5>Consistency/Consistencia:</h5><p>Los datos deben mantener un formato y estructura uniforme, sin contradicciones.</p>"),
        HTML("<h5>Completeness/Completitud:</h5><p>Los datos deben incluir toda la información necesaria, sin valores faltantes.</p>"),
        HTML("<h5>Uniqueness/Singularidad:</h5><p>Cada dato debe ser único, sin registros duplicados.</p>"),
        HTML("<h5>Accuracy/Precisión:</h5><p>Los datos deben reflejar la realidad de forma exacta y correcta.</p>")
      ),
      box(
        title = "Score Details",
        width = 6,
        h4("Overall Quality Score"),
        plotlyOutput("overall_score_plot"),  # Gráfico de barras horizontal para Score
        h5("Grade"),
        verbatimTextOutput("overall_grade"),
        h5("Interpretation"),
        verbatimTextOutput("overall_interpretation"),
        h5("Recommendations"),
        verbatimTextOutput("overall_recommendations")
      )
    ),
    fluidRow(
      box(
        title = "Data Standards Compliance",
        width = 12,
        tableOutput("complianceTable"),
        uiOutput("match_grade_indicator")  # Salida para el semáforo de match_grade
      ),
      box(
        title = "Open Data Assessment",
        width = 12,
        plotlyOutput("openDataBarChart")
      )
    )
  )
)

# Definir servidor
server <- function(input, output) {
  # Filtrar datos según selección del usuario
  selected_data <- reactive({
    json_data[[input$dataset]]
  })
  
  # Mostrar "metadata" en la barra lateral
  output$metadata <- renderPrint({
    metadata <- selected_data()$metadata
    if (!is.null(metadata)) {
      metadata
    } else {
      "No metadata available"
    }
  })
  
  # Renderizar gráfico de radar en "Overall Data Quality Score"
  output$qualityRadar <- renderPlotly({
    quality_checks <- selected_data()$quality_checks
    if (!is.null(quality_checks)) {
      scores <- c(
        Completeness = quality_checks$completeness$grade$score,
        Accuracy = quality_checks$accuracy$grade$score,
        Uniqueness = quality_checks$uniqueness$grade$score,
        Consistency = quality_checks$consistency$grade$score
      )
      radar_data <- as.data.frame(t(scores))
      colnames(radar_data) <- names(scores)
      radar_data <- rbind(rep(1, length(scores)), rep(0, length(scores)), radar_data)
      
      plot_ly(
        type = 'scatterpolar',
        r = unlist(radar_data[3, ]),
        theta = colnames(radar_data),
        fill = 'toself',
        text = unlist(radar_data[3, ]),
        textposition = 'top right',
        mode = 'markers+text'
      ) %>% layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 1)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        showlegend = FALSE
      )
    } else {
      plot_ly() %>% layout(title = "No data available")
    }
  })
  
  # Renderizar gráfico de barras horizontal para "Score" en "Score Details"
  output$overall_score_plot <- renderPlotly({
    score <- selected_data()$overall_quality$score
    if (!is.null(score)) {
      plot_ly(
        x = ~score,
        y = ~"Score",
        type = 'bar',
        orientation = 'h',
        marker = list(color = 'gray', line = list(width = 1.5, color = 'black')),  # Color gris con borde negro
        text = ~score,
        textposition = 'inside',  # Coloca el texto dentro de la barra
        insidetextanchor = 'middle',
        textfont = list(size = 16, color = "white")  # Tamaño y color de texto de etiqueta
      ) %>% layout(
        xaxis = list(title = "Score", range = c(0, 1)),
        yaxis = list(title = "", tickvals = NULL),  # Elimina el título y valores del eje Y
        bargap = 0.8,  # Reduce el grosor de la barra
        title = "Overall Quality Score"
      )
    } else {
      plot_ly() %>% layout(title = "No data available")
    }
  })
  
  # Mostrar detalles de "overall_quality" en el área "Score Details"
  output$overall_grade <- renderPrint({
    grade <- selected_data()$overall_quality$grade
    if (!is.null(grade)) grade else "No grade available"
  })
  
  output$overall_interpretation <- renderPrint({
    interpretation <- selected_data()$overall_quality$interpretation
    if (!is.null(interpretation)) interpretation else "No interpretation available"
  })
  
  output$overall_recommendations <- renderPrint({
    recommendations <- selected_data()$overall_quality$recommendations
    if (is.null(recommendations) || length(recommendations) == 0) {
      "No recommendations available"
    } else {
      recommendations
    }
  })
  
  # Renderizar tabla de "Data Standards Compliance" y semáforo para "match_grade"
  output$complianceTable <- renderTable({
    standards_match <- selected_data()$standards_match
    if (!is.null(standards_match) && length(standards_match) > 0) {
      compliance_df <- as.data.frame(t(standards_match))
      rownames(compliance_df) <- sapply(rownames(compliance_df), function(name) paste0("**", name, "**"))
      compliance_df
    } else {
      data.frame(Mensaje = "No se encontraron estandares para evaluar el conjunto de datos")
    }
  }, rownames = TRUE, sanitize.text.function = function(x) x)
  
  # Semáforo para la llave "match_grade"
  output$match_grade_indicator <- renderUI({
    match_grade <- selected_data()$match_grade
    if (!is.null(match_grade)) {
      color <- switch(match_grade,
                      "red" = "#FF0000",
                      "yellow" = "#FFFF00",
                      "green" = "#00FF00",
                      "#FFFFFF")
      tags$div(style = paste("width: 30px; height: 30px; background-color:", color, "; border-radius: 50%; margin-top: 10px;"),
               "")
    } else {
      tags$div("No match grade data available")
    }
  })
  
  # Renderizar gráfica de barras horizontales para "Open Data Assessment" en color negro
  output$openDataBarChart <- renderPlotly({
    open_data_grading <- selected_data()$open_data_grading
    if (!is.null(open_data_grading$scores)) {
      scores_data <- open_data_grading$scores
      open_data_df <- data.frame(
        Criteria = names(scores_data),
        Score = as.numeric(unlist(scores_data))
      )
      
      # Crear gráfico de barras horizontales con color negro
      plot_ly(
        open_data_df,
        x = ~Score,
        y = ~Criteria,
        type = 'bar',
        orientation = 'h',
        marker = list(color = 'black'),
        text = ~Score,
        textposition = 'outside'
      ) %>% layout(
        title = "Open Data Assessment Scores",
        xaxis = list(title = "Score"),
        yaxis = list(title = "Criteria")
      )
    } else {
      plot_ly() %>% layout(title = "No data available")
    }
  })
}

# Ejecutar la app
shinyApp(ui = ui, server = server)