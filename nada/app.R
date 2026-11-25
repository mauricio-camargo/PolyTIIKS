#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
library(modelr)
library(purrr)

data("sim1")

GRID <- list(beta_0 = list(min = -20, max = 40, by = 0.2),
             beta_1 = list(min = -5, max = 5, by = 0.2))
RESULTS <- lm(y ~ x, data = sim1)

model1 <- function(beta, x) {
  beta[1] + x * beta[2]
}

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data$x)
  sqrt(mean(diff ^ 2))
}

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  
  # Some help text
  h2("Cost Function of Linear Regression"),
  
  # Vertical space
  tags$hr(),
  
  sliderInput("beta_0", "beta0:",
              min = GRID$beta_0$min, max = GRID$beta_0$max,
              value = coef(RESULTS)[1], step = GRID$beta_0$by),
  
  sliderInput("beta_1", "beta1:",
              min = GRID$beta_1$min, max = GRID$beta_1$max,
              value = coef(RESULTS)[2], step = GRID$beta_1$b),
  
  # Plotly Chart Area
  fluidRow(
    column(6, plotlyOutput(outputId = "regplot", height = "600px")),
    column(6, plotlyOutput(outputId = "surface", height = "600px"))
  )
)



server <- function(input, output){
  
  grid_0 <- seq(GRID$beta_0$min, GRID$beta_0$max, by = GRID$beta_0$by)
  grid_1 <- seq(GRID$beta_1$min, GRID$beta_1$max, by = GRID$beta_1$by)
  grid <- map(grid_0, ~ purrr::map2_dbl(.x, grid_1, sim1_dist)) %>%
    flatten_dbl() %>%
    matrix(nrow = length(grid_1))
  
  grid_x <- seq(min(sim1$x), max(sim1$x), length.out = 10)
  
  beta <- reactive(c(input$beta_0, input$beta_1))
  
  # Plot time series chart
  output$surface <- renderPlotly({
    p <- plot_ly(x = grid_0, y = grid_1, z = grid) %>%
      add_surface() %>%
      add_markers(x = input$beta_0, y = input$beta_1,
                  z = measure_distance(beta(), sim1)) %>%
      layout(title = "Cost Function",
             yaxis = list(title = "beta_0"),
             xaxis = list(title = "beta_1"))
    p
  })
  
  output$regplot <- renderPlotly({
    fit <- model1(beta(), sim1$x)
    err <- (sim1$y - fit) ^ 2
    
    p <- plot_ly(data = sim1, x = ~ x, y = ~ y) %>%
      add_markers(mode = "scatter", color = err) %>%
      add_trace(mode = 'lines', x = grid_x, y = model1(beta(), grid_x)) %>%
      add_segments(x = sim1$x, xend = sim1$x, y = sim1$y, yend = fit,
                   segmant = list(color = "ltgray")) %>%
      layout(title = "Linear Regression on modelr::sim1 data",
             yaxis = list(zeroline = FALSE, title = "y"),
             xaxis = list(zeroline = FALSE, title = "x"))
    p
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
